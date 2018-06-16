library(valuespotter)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(raster)
library(digest)
library(sp)
library(htmlwidgets)
library(geosphere)
library(ncdf4)
library(rgdal)

options(scipen = 999)

# RDS datasets expected
shiny_data_dir <- "shiny_data/"

# How many addresses should the download csv file contain?
config.max_map_markers <- 10 ^ 4
# Debounce delay for the inputs
config.debounce_delay_milliseconds <- 200
# Radius (meters) of the circle when user clicks on the map
config.focus_radius <- 200

# Change raster resolution here.
# Smaller square width/height (meters) will result in higher
# resolution, thus slowing down the app and increasing traffic
# to the client.
config.raster_square_wdith <- 50
config.raster_square_height <- 50

ui <- fluidPage(
  title = "Value spots in Amsterdam",
  titlePanel("Value spots in Amsterdam"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = 'display_mode',
        tabPanel(
          title = "Observe",
          value = "show_scores_valuespots",
          uiOutput(outputId = "spotscores_options"),
          uiOutput(outputId = "valuemarks_options")
        ),
        tabPanel(
          title = "Create",
          value = "create",
          uiOutput(outputId = "create_area")
        ),
        tabPanel(
          title = "Help",
          value = "helptext",
          br(),
          p(
            "With this app, you can easily find out where in Amsterdam the true valueable spots for housing are. This works by measuring the distance between each building and certain value marks, such as the closest Starbucks store or the closest access to the motorway."
          ),
          p(
            "These \"distance scores\" tell you exactly which streets and buildings match your needs."
          ),
          p(
            "You can also combine your custom needs and create your individual score. By calculating the weighted mean distance for a multitude of value marks, the app will help you find your perfect spot to live."
          ),
          p(
            "Author: ",
            a(href = "https://www.linkedin.com/in/nils-gl%C3%BCck-60a959121/", target =
                "_blank", "Nils Glück")
          ),
          p(
            "Code on GitHub: ",
            a(
              href = "https://github.com/NilsOle/valuespots",
              target =
                "_blank",
              "github.com/NilsOle/valuespots"
            )
          ),
          p(
            "Data source (as of November 8, 2017): ",
            a(href = "https://data.nlextract.nl/bag/csv/", target = "_blank", "NL Extract")
          )
        )
      ),
      hr(),
      h4("Best-scored addresses"),
      downloadButton(outputId = "download_dataset", label = "Download csv file"),
      hr(),
      h4("Score distribution across Amsterdam"),
      plotOutput(outputId = "plot"),
      hr(),
      h4("Lowest distance scores in Amsterdam"),
      tableOutput(outputId = "table")
    ),
    mainPanel(
      leafletOutput(
        outputId = "map",
        width = "100%",
        height = "650px"
      ),
      hr(),
      h4("Local selection (click on the map to select an area)"),
      dataTableOutput(outputId = "focus_table")
    )
  )
)
server <- function(input, output, session) {
  data_bag_aggr <- readRDS(paste0(shiny_data_dir,"bag_aggr.RDS"))
  variables <- readRDS(paste0(shiny_data_dir,"variables.RDS"))
  valuespots <- readRDS(paste0(shiny_data_dir,"valuespots.RDS"))
  scoring_formulae <- readRDS(paste0(shiny_data_dir,"scoring_formulae.RDS"))

  # object for storing debounced inputs
  input_d <- list()
  # these names will be parts of input_d and addressable as reactive functions
  config.expected_names <- c(
    "display_mode",
    "spotscores_selection",
    "valuemarks_selection",
    paste0("weight_", variables[, "technical_name"]),
    "button_save_formula",
    "map_click"
  )
  status.last_input_names <- NULL
  input_all <- reactive({
    all_values <- reactiveValuesToList(input)
    if (is.null(status.last_input_names)) {
      wanted_values <- c(names(all_values), config.expected_names)
      new_reactive_names <- wanted_values
    } else {
      new_reactive_names <-
        names(all_values)[which(!names(all_values) %in% status.last_input_names)]
    }
    if (length(new_reactive_names) > 0) {
      input_d[new_reactive_names] <<-
        sapply(new_reactive_names, function(cur_name) {
          reactive({
            if (!all(is.null(input[[cur_name]]))) {
              input[[cur_name]]
            } else {
              NULL
            }
          }) %>% debounce(config.debounce_delay_milliseconds)
        })
    }
    status.last_input_names <<- names(input_d)
  })
  # === activate wrapper on startup ===
  observeEvent(input_all(), {
  })
  update_ui <- function(spotscore_selection = NULL) {
    output$spotscores_options <- renderUI({
      choices <- scoring_formulae[, "technical_name"]
      names(choices) <- scoring_formulae[, "name"]
      tagList(
        br(),
        radioButtons(
          inputId = "spotscores_selection",
          label = "Which distance score would you like to see?",
          choices = choices,
          selected = spotscore_selection
        )
      )
    })
    output$valuemarks_options <- renderUI({
      choices <- variables[, "technical_name"]
      names(choices) <- variables[, "name"]
      tagList(
        checkboxGroupInput(
          inputId = "valuemarks_selection",
          label = "Which value marks would you like to see?",
          choices = choices
        )
      )
    })
    output$create_area <- renderUI({
      choices <- variables[, "name"]
      names(choices) <- variables[, "technical_name"]
      tagList(
        br(),
        p(
          "Create your own custom score! Configure the weights for each spot distance, and the formula will be changed accordingly. Hit \"Save\" when you are done."
        ),
        lapply(names(choices), function(cur_choice) {
          if (is.null(input_d[[paste0("weight_", cur_choice)]]())) {
            insert_nr <- 0
          } else {
            insert_nr <- input_d[[paste0("weight_", cur_choice)]]()
          }
          numericInput(
            inputId = paste0("weight_", cur_choice),
            label = choices[cur_choice],
            value = insert_nr,
            min = 0,
            max = 1000,
            step = 1,
            width = NULL
          )
        }),
        p("Your score formula looks like this:"),
        verbatimTextOutput(outputId = "formula_preview", placeholder = T),
        actionButton(inputId = "button_save_formula", label = "Save")
      )
    })
  }
  update_spotscore_selection <- function(spotscore_selection = session$userData$spotscore_selection,
                                         valuespots_selection = session$userData$valuespot_selection){
    if (is.null(spotscore_selection)) {
      get_map_values <- data_bag_aggr[0,]
    } else {
      get_map_values <-
        data_bag_aggr[order(data_bag_aggr[, spotscore_selection]),]
    }
    if (nrow(get_map_values) > config.max_map_markers) {
      session$userData$map_addresses <- get_map_values[1:config.max_map_markers,]
    } else {
      session$userData$map_addresses <- get_map_values
    }
    if (is.null(valuespots_selection)) {
      markers_values <- valuespots[0,]
    } else {
      markers_values <-
        valuespots[valuespots[, "variable_technical_name"] %in% valuespots_selection ,]
    }
    valuerange <- get_map_values[[spotscore_selection]]
    session$userData$spotscore_selection <- spotscore_selection
    pal2 <- colorQuantile(palette = c("red", "yellow", "darkgreen"),
                          domain = na.omit(data_bag_aggr[[spotscore_selection]]),
                          n = 100,
                          na.color = NA)
    pal3 <- colorQuantile(palette = c("red", "yellow", "darkgreen"),
                          domain = na.omit(data_bag_aggr[[spotscore_selection]]),
                          n = 5,
                          na.color = NA)
    r <- getRasterImage( scoring_formulae = scoring_formulae,
                         technical_name = spotscore_selection,
                         raster_square_wdith_m = config.raster_square_wdith,
                         raster_square_height_m = config.raster_square_height,
                         data = data_bag_aggr)
    percentile <- ecdf(data_bag_aggr[[spotscore_selection]])
    total_lon <- c(get_map_values$lon, markers_values$lon)
    total_lat <- c(get_map_values$lat, markers_values$lat)

    leafletProxy( mapId = "map") %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      fitBounds(min(total_lon),
                min(total_lat),
                max(total_lon),
                max(total_lat)) %>%
      htmlwidgets::onRender("
          function(el, x) {
            var myMap = this;
            myMap.on('click', function(e) {
              var popLocation= e.latlng;
              Shiny.onInputChange('map_click',{ lng: popLocation.lng, lat: popLocation.lat });
            });
          }")
      if( length(unique(data_bag_aggr[[spotscore_selection]])) > 1 ){
        leafletProxy( mapId = "map") %>%
          addLegend("bottomright",
                    pal = pal3 ,
                    values = data_bag_aggr[[spotscore_selection]],
                    title = "Distance score",
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      p = paste0(round(p * 100), '%')
                      paste0(prettyNum(floor(cuts[-n]),big.mark = ",",preserve.width = "none"),
                             " m &ndash; ",
                             prettyNum(floor(cuts[-1]),big.mark = ",",preserve.width = "none"),
                             " m" )
                    },
                    opacity = 1
          ) %>%
          addRasterImage(r,
                         colors = pal2,
                         opacity = 0.8)
    }
    output$plot <- renderPlot({
      ggplot(data_bag_aggr, aes(data_bag_aggr[[spotscore_selection]])) +
        geom_histogram(bins = 10) + xlab(scoring_formulae[scoring_formulae[, "technical_name"] == spotscore_selection, "name"])
    })
    output$download_dataset <- downloadHandler(
      filename = function() {
        paste('data-addresses-amsterdam-', Sys.Date(), '.csv', sep = '')
      },
      content = function(con) {
        write.csv(session$userData$map_addresses, con, row.names = F)
      }
    )
    output$table <- renderTable({
      data <- data_bag_aggr[order(data_bag_aggr[[spotscore_selection]]),]
      until <- 10
      if (nrow(data) == 0) {
        return(NULL)
      }
      if (nrow(data) < 10) {
        until <- nrow(data)
      }
      returnval <-
        data[1:until, c("fulladdress", spotscore_selection)]
      returnval[, "fulladdress"] <-
        ifelse(nchar(returnval[, "fulladdress"]) < 30,
               returnval[, "fulladdress"],
               paste0(substr(returnval[, "fulladdress"], 1, 30), "..."))
      colnames(returnval) <-
        c("Address", scoring_formulae[scoring_formulae[, "technical_name"] == spotscore_selection , "name"])
      return(returnval)
    })
    output$focus_table <- renderDataTable({
      NULL
    })

  }
  update_valuespots_selection <-
    function(valuespots_selection = session$userData$valuespot_selection,
             spotscore_selection = session$userData$spotscore_selection) {
      if (is.null(spotscore_selection)) {
        get_map_values <- data_bag_aggr[0,]
      } else {
        get_map_values <-
          data_bag_aggr[order(data_bag_aggr[, spotscore_selection]),]
      }
      if (is.null(valuespots_selection)) {
        markers_values <- valuespots[0,]
      } else {
        markers_values <-
          valuespots[valuespots[, "variable_technical_name"] %in% valuespots_selection ,]
      }
      session$userData$valuespot_selection <- valuespots_selection
      pal <- colorNumeric(palette = c("red", "yellow", "darkgreen"),
                          domain = get_map_values[[spotscore_selection]])
      getColor <- function(data) {
        data$variable_color
      }
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'white',
        library = 'ion',
        markerColor = getColor(markers_values)
      )
      total_lon <- c(get_map_values$lon, markers_values$lon)
      total_lat <- c(get_map_values$lat, markers_values$lat)
      leafletProxy(mapId = "map") %>%
        clearMarkers() %>%
        fitBounds(min(total_lon),
                  min(total_lat),
                  max(total_lon),
                  max(total_lat)) %>%
        addAwesomeMarkers(
          data = markers_values,
          lng = ~ lon,
          lat = ~ lat,
          icon = icons,
          popup = ~ variable_name
        )
    }
  # === Start with default values ===
  session$userData$spotscore_selection <- NULL
  session$userData$valuespot_selection <- NULL
  output$map <- renderLeaflet({
    total_lon <- c(data_bag_aggr$lon, valuespots$lon)
    total_lat <- c(data_bag_aggr$lat, valuespots$lat)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(min(total_lon),
                min(total_lat),
                max(total_lon),
                max(total_lat))
  })
  # == Update the UI on app startup ==
  update_ui()
  # === Handle changing the formula ===
  observeEvent(input_d$spotscores_selection(), {
    update_spotscore_selection(spotscore_selection = input_d$spotscores_selection())
  })
  # === Handle value spots selection ===
  observeEvent(input_d$valuemarks_selection(),{
    update_valuespots_selection(valuespots_selection = input_d$valuemarks_selection())
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  # === Handle changing the formula ===
  listen_list <- reactive({
    lapply(variables[, "technical_name"], function(el) {
      input_d[[paste0("weight_", el)]]()
    })
  })
  observeEvent(listen_list(), {
    variables_custom_score <- variables
    variables_custom_score[, "weight"] <-
      as.integer(sapply(variables[, "technical_name"], function(el) {
        ifelse(is.null(input_d[[paste0("weight_", el)]]()), 0,     input_d[[paste0("weight_", el)]]())
      }))
    session$userData$custom_formula <-
      generate_formula(variables_custom_score)
    output$formula_preview <- renderText({
      session$userData$custom_formula
    })
  })
  # === Handle formula save button click ===
  observeEvent(input_d$button_save_formula(), {
    if (!"custom_score" %in% scoring_formulae[, "technical_name"]) {
      scoring_formulae <<- rbind(
        scoring_formulae,
        data.frame(
          "scoring_id" = max(scoring_formulae$scoring_id) + 1,
          "description" = "Your custom score.",
          "name" = "Your custom score",
          "technical_name" = "custom_score",
          "formula" = session$userData$custom_formula,
          stringsAsFactors = F
        )
      )
    } else {
      scoring_formulae[scoring_formulae[, "technical_name"] == "custom_score", "formula"] <<-
        session$userData$custom_formula
    }
    data_bag_aggr$custom_score <<- eval(parse(
      text = paste0(
        "with(data_bag_aggr,",
        session$userData$custom_formula,
        ")"
      )
    ))
    update_ui(spotscore_selection = "custom_score")
    update_spotscore_selection(spotscore_selection = "custom_score")
  })
  # === Handle map click event ===
  observeEvent(input_d$map_click(),{
    lng <- as.numeric(input_d$map_click()$lng)
    lat <- as.numeric(input_d$map_click()$lat)
    session$userData$map_focus <- list(
      "lng"=lng,
      "lat"=lat,
      "addresses"=get_circle_subset(longitude_arg = lng,
                                    latitude_arg = lat,
                                    radius = config.focus_radius,
                                    data = data_bag_aggr)
    )
    leafletProxy("map") %>%
      clearShapes() %>%
      setView(lng = session$userData$map_focus$lng,
              lat = session$userData$map_focus$lat,
              zoom = 15) %>%
      addCircles(lng = session$userData$map_focus$lng,
                 lat = session$userData$map_focus$lat,
                 radius = config.focus_radius,
                 color = "#000000",
                 fillOpacity = 0,
                 stroke = T)
    output$focus_table <- renderDataTable({
      data <- session$userData$map_focus$addresses[order(session$userData$map_focus$addresses[[session$userData$spotscore_selection]]),]
      returnval <-
        data[, c("fulladdress", session$userData$spotscore_selection)]
      returnval[,session$userData$spotscore_selection] <- round(returnval[,session$userData$spotscore_selection],2)
      colnames(returnval) <-
        c("Address", scoring_formulae[scoring_formulae[, "technical_name"] == session$userData$spotscore_selection , "name"])
      return(returnval)
    },
    options = list("pageLength" = 10))
  })
}
shinyApp(ui = ui, server = server)
