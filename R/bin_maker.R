#' Convert continuous ENM predictions into binary presence/absence maps
#'
#' @param preds RasterStack object containing continuous ENM predictions
#' @param th A list of numeric threshold values
#' @returns RasterStack of binary maps
#' @examples
#' get.binary <- bin_maker(preds = enm.preds, th = list(0.1, 0.23, 0.43))

bin_maker <- function(preds, th) {
  require(ecospat)
  require(terra)
  require(raster)

  binary.maps <- list()

  for (i in 1:nlayers(preds)) {
    bin <- ecospat::ecospat.binary.model(Pred = terra::rast(preds[[i]]), Threshold = th[[i]]) %>% raster::raster()
    binary.maps[[i]] <- bin
    binary.out <- raster::stack(binary.maps)
  }
  return(binary.out)
}
