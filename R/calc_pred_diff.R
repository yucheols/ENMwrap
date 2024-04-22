#' Calculate the difference between ENM predictions
#'
#' @param layer.a RasterLayer of first prediction map
#' @param layer.b RasterLayer of second prediction map
#' @param type The specific method of calculating prediction differences. If method = 'Dubos', conduct calculation based on the equation provided in Dubos et al. (2023).
#' If method = 'Schoener', use the ENMTools package to first calculate the Schoener's D metric, and then use the equation (1 - D) * 100 to calculate the difference between predictions
#' @param verbose print out messages when method = 'Schoener' is used.
#' @returns A percent value of difference between the two input RasterLayers
#' @examples
#' diff <- calc_pred_diff(layer.a = pred.a, layer.b = pred.b, method = 'Dubos')


calc_pred_diff <- function(layer.a, layer.b, method, verbose = F) {
  if (method == 'Dubos') {
    sum.a <- cellStats(x = layer.a, stat = 'sum', na.rm = T)
    sum.b <- cellStats(x = layer.b, stat = 'sum', na.rm = T)
    diff <- (abs(sum.a - sum.b)/sum.a) * 100
  }
  else if (method == 'Schoener') {
    require(ENMTools, quietly = T)
    sim <- ENMTools::raster.overlap(x = layer.a, y = layer.b, verbose = verbose)
    diff <- (1 - sim$D) * 100
  }
  return(diff)
}
