#' Spatial thinning of (multiple) species occurrence datasets, using the SDMtune package (Vignali et al. 2020)
#'
#' @param occs_list A list of data.frame objects for species occurrence points. Need to have "long" and "lat" columns
#' @param envs RasterLayer. The spatial resolution of this layer will be used as a spatial thinning distance
#' @param long Character. Name of the longitude column
#' @param lat Character. Name of the latitude column
#' @param spp_list A list of input species names
#' @returns A list of dataframes containing the thinned occurrence points for each input species
#' @examples
#' thin_occs <- occs_thinner(occs_list = occs_list, envs = envs, long = 'long', lat = 'lat', spp_list = spp_list)


occs_thinner <- function(occs_list, envs, long, lat, spp_list){
  output <- list()

  for (i in 1:length(occs_list)) {
    print(paste0('start thinning', spp_list[[i]]))
    occs <- occs_list[[i]]
    thin <- SDMtune::thinData(coords = occs, env = terra::rast(envs), x = long, y = lat, verbose = T, progress = T)
    output[[i]] <- thin
  }
  return(output)
  print('Done!')
}

