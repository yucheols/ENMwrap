#' Get MaxEnt threshold values from a single prediction or multiple continuous ENM predictions
#'
#' @param preds RasterLayer (single prediction) or RasterStack (multiple predictions) object containing continuous ENM predictions
#' @param occs A list containing two column data frames of species occurrence points. The length of this list should correspond to the number of layers contained in the "preds" object.
#' For thresholding a single prediction for one species, input "occs.list = list(occs.sp1)". For thresholding multiple predictions for a single species, input "occs.list = list(occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1)".
#' For multiple species, input "occs.list = list(occs.sp1, occs.sp2, occs.sp3, occs.sp4, occs.sp5, occs.sp6)"
#' @param type The type of thresholding method to use. If "mtp" is used, calculate the minimum presence (= zero omission) threshold. If "p10" is used, calculate the 10 percent presence threshold.
#' @returns A list containing the calculated threshold values
#' @examples
#' # For a single prediction for one species
#' thresh <- get_thresh(preds = pred, occs.list = list(occs), type = 'p10')
#'
#' # For multiple predictions for a single species
#' thresh <- get_thresh(preds = preds, occs.list = list(occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1, occs.sp1), type = 'p10')
#'
#' # For multiple predictions for multiple species
#' thresh <- get_thresh(preds = preds, occs.list = list(occs.sp1, occs.sp2, occs.sp3, occs.sp4, occs.sp5, occs.sp6), type = 'p10')

get_thresh <- function(preds, occs.list, type = 'mtp') {
  require(raster)

  output <- list()

  for (i in 1:nlayers(preds)) {
    pred.vals <- raster::extract(preds[[i]], occs.list[[i]])
    if(type == "mtp"){
      thresh <- min(na.omit(pred.vals))
    } else if(type == "p10"){
      if(length(pred.vals) < 10){
        p10 <- floor(length(pred.vals) * 0.9)
      } else {
        p10 <- ceiling(length(pred.vals) * 0.9)
      }
      thresh <- rev(sort(pred.vals))[p10]
    }
    preds_thresh <- preds[[i]]
    preds_thresh[preds_thresh < thresh] <- NA

    output[[i]] <- raster::minValue(preds_thresh)
  }
  return(output)
  }

