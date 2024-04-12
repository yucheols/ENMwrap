#' Predict ecological niche models to multiple datasets
#'
#' @param model A fitted ENM of the class MaxEnt. Can be extracted by accessing the "models" slot output from "test_multibg" or "test_multisp"
#' @param preds.list A list containing RasterStack objects of projection datasets
#' @param preds.names A vector of names corresponding to each dataset in "preds.list". Can be the name of a particular time period (e.g. LGM) or climate change scenario (e.g. SSP26)
#' @return RasterStack of ENM predictions
#' @examples
#' pred.enms <- model_predictr(model = mx_model, preds.list = list(lgm, mh, current), preds.names = c('LGM', 'Mid-Holocene', 'Current))

model_predictr <- function(model, preds.list, preds.names) {
  require(dismo)

  output <- list()

  for (i in 1:length(preds.list)) {
    make.pred <- dismo::predict(object = model, x = preds.list[[i]], progress = 'text')
    output[[i]] <- make.pred
  }
  output.pred <- raster::stack(output)
  names(output.pred) = pred.names
  return(output.pred)
}
