#' @export
decimalDegreesToDMS <- function(degree){
  d <- as.integer(degree)
  m <- as.integer((degree - d) * 60)
  s <- (degree - d - m/60) * 3600
  return(
    list("d"=d,"m"=m,"s"=s)
  )
}

#' @export
DMStoAngle <- function(d,m,s){
  return( d + m/60 + s/3600 )
}

#' @export
angleToRadian <- function(angle){
  return( angle * pi / 180 )
}

#' @export
calculateProperties <- function(latitude_vector,distance){
  if(abs(max(latitude_vector)) >= abs(min(latitude_vector))){
    extreme_func <- "max"
  } else {
    extreme_func <- "min"
  }
  narrowest_lat <- do.call(extreme_func,list(latitude_vector))
  dms <- decimalDegreesToDMS( narrowest_lat )
  angle <- DMStoAngle( dms$d, dms$m, dms$s )
  radians <- angleToRadian(angle)
  x_per_lat_degree <- distance / (111.32 * 10^3)
  x_per_long_degree <- distance / (40075 * 10^3 * cos( radians ) / 360)
  return( list("extreme_func"=extreme_func,
               "narrowest_lat"=narrowest_lat,
               "x_per_lat_degree"=x_per_lat_degree,
               "x_per_long_degree"=x_per_long_degree) )
}

#' @export
getRasterImage <- function( scoring_formulae, technical_name, raster_images_path = "raster_images/", raster_square_wdith_m, raster_square_height_m, data ){
  formula_selected <- scoring_formulae[scoring_formulae[, "technical_name"] == technical_name, ]
  formula_name_md5 <- digest::digest(formula_selected[,"formula"], algo = "md5")
  file_name <- paste0(raster_images_path,formula_name_md5,".nc")
  if(!file.exists(file_name)){
    createRasterImage(scoring = formula_selected,
                      file_name = file_name,
                      raster_square_wdith_m,
                      raster_square_height_m,
                      data = data)
  }
  return( raster::raster(file_name) )
}

#' @export
createRasterImage <- function( scoring, file_name, raster_square_wdith_m, raster_square_height_m, data ){
  long_lat_value_table <- data.frame(
    "longitude"=data$lon,
    "latitude"=data$lat,
    "value"=data[[ scoring[,"technical_name"] ]],
    stringsAsFactors = F
  )
  distance_values <- calculateProperties(latitude_vector=long_lat_value_table[,"latitude"],
                                         distance=raster_square_wdith_m)
  min_lat <- min(long_lat_value_table[,"latitude"])
  max_lat <- max(long_lat_value_table[,"latitude"])
  min_lng <- min(long_lat_value_table[,"longitude"])
  max_lng <- max(long_lat_value_table[,"longitude"])
  long_lat_value_table[,"longitude_diff"] <- with(long_lat_value_table,abs(longitude-min_lng))
  long_lat_value_table[,"latitude_diff"] <- with(long_lat_value_table,abs(latitude-min_lat))
  x_tiles_no <- floor(abs((max_lng-min_lng)/distance_values[["x_per_long_degree"]]))+1
  y_tiles_no <- floor(abs((max_lat-min_lat)/((raster_square_height_m/raster_square_wdith_m)*distance_values[["x_per_lat_degree"]])))+1
  pts <- as.matrix(long_lat_value_table[,c("longitude","latitude","value")])
  colnames(pts) <- c('X', 'Y', 'Z')
  e <- raster::extent(pts[,c("X","Y")])
  r <- raster::raster(e, ncol=x_tiles_no, nrow=y_tiles_no)
  r <- raster::rasterize(pts[, c("X","Y")], r, pts[,"Z"], fun=mean)
  projection(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  rnc <- raster::writeRaster(r, filename=file_name, format="CDF", overwrite=TRUE)
}

#' @export
get_circle_subset <- function(longitude_arg, latitude_arg, radius, data){
  cp <- calculateProperties(c(data[,"lat"],latitude_arg),radius)
  x_per_long_degree <- cp[["x_per_long_degree"]]
  x_per_lat_degree <- cp[["x_per_lat_degree"]]
  square_subset <- subset(data, lon < longitude_arg+x_per_long_degree
                          & lon > longitude_arg-x_per_long_degree
                          & lat < latitude_arg+x_per_lat_degree
                          & lat > latitude_arg-x_per_lat_degree)
  if(nrow(square_subset) == 0){
    return (square_subset)
  }
  square_subset$disthaversine <- apply(square_subset,1,function(cur_element){
    geosphere::distHaversine( c(longitude_arg, latitude_arg),
                              as.numeric(c(cur_element["lon"], cur_element["lat"]) ) )
  } )
  return( subset(square_subset,disthaversine <= radius) )
}

#' @export
generate_formula <- function(variables_table) {
  if (sum(variables_table$weight) > 0) {
    paste0("(",
           paste(
             paste(variables_table[variables_table[, "weight"] > 0, "weight"],
                   variables_table[variables_table[, "weight"] > 0, "technical_name"],
                   sep = "*")
             ,
             collapse = "+"
           ),
           ")/",
           ifelse(
             sum(variables_table$weight) == 0,
             1,
             sum(variables_table$weight)
           ))
  } else {
    "0"
  }
}
