#' Calculate the size of spatial blocks to use for data partitioning based on the estimation of spatial autocorrelation
#'
#' @param points A list of point datasets, each in a data.frame format with two columns in the order of "long" and "lat"
#' @param raster RasterStack of environmental data to use for calculation
#' @param num_sample Integer. The number of sample points to use. See ?cv_spatial_autocor documentation of the blockCV package for details
#' @param crs A coordinate reference system (CRS). Use the 4 digit numbers of EPSG codes
#' @returns A list of numbers indicating the size of spatial blocks to use
#' @examples
#' get.block.size <- block_size(points = list(bg1, bg2, bg3), raster = envs, num_sample = 5000, crs = 4326)


block_size <- function(points, raster, num_sample, crs) {
  require(blockCV)
  require(sf)

  block_size_list <- list()

  block_prep <- for (i in 1:length(points)) {
    points[[i]]$occ <- rep(0, nrow(points[[i]]))
    pts.sf <- sf::st_as_sf(points[[i]], coords = c('long', 'lat'), crs = crs)
    auto  <- blockCV::cv_spatial_autocor(x = pts.sf, r = raster, num_sample = num_sample, column = 'occ', progress = T)

    block_size_list[[i]] <- auto$range
  }
  return(block_size_list)
}
