#' Generate spatial buffers for input occurrence points
#'
#' @param occs_list A list containing occurrence datasets. Each occurrence set needs to have two columns named "long" and "lat", and in that order
#' @param envs RasterStack  (or RasterLayer) of input environmental data
#' @param buff_dist A numeric value given in meters. Circular dissolved buffers will be created based on this value
#' @returns A list of spatial buffers, with the order of species corresponding to the input species occurrence points
#' @examples
#' get.buff <- buffMaker(occs_list = occs_list, envs = envs, buff_dist = 700000)

buffMaker <- function(occs_list, envs, buff_dist) {
  require(sf, quietly = T)

  output <- list()

  for (i in 1:length(occs_list)) {
    occs.sf <- sf::st_as_sf(occs_list[[i]], coords = c('long', 'lat'), crs = raster::crs(envs))
    eq_area = '+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
    occs.sf <- sf::st_transform(occs.sf, crs = eq_area)

    buff <- sf::st_buffer(occs.sf, dist = buff_dist) %>%
      sf::st_union() %>%
      sf::st_sf() %>%
      sf::st_transform(crs = raster::crs(envs))

    output[[i]] <- buff
  }
  return(output)
}
