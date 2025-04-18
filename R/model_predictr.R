#' Predict ecological niche models to multiple datasets
#'
#' @param model A fitted ENM of the class MaxEnt. Can be extracted by accessing the "models" slot output from "test_multibg" or "test_multisp". If "method = multi2single", this object will be of the class "list"
#' @param preds.list A list containing RasterStack objects of projection datasets. If "method = multi2single", input RasterStack object
#' @param pred.names A vector of names corresponding to each dataset in "preds.list". Can be the name of a particular time period (e.g. LGM) or climate change scenario (e.g. SSP26)
#' @param method Specify the type of model prediction. If "single2multi", conduct model predictions to multiple past/future environment from a single model. If "multi2single", make predictions to a single past/future dataset from multiple current models
#' @returns RasterStack of ENM predictions
#' @examples
#' pred.singe2multi <- model_predictr(model = mx_model, preds.list = list(lgm, mh, current), preds.names = c('LGM', 'Mid-Holocene', 'Current'), method = 'single2multi')
#' pred.multi2single <- model_predictr(model = mx_models_multi, preds.list = future.envs, preds.names = 'SSP585', method = 'multi2single')

model_predictr <- function(model, preds.list, pred.names, method) {
  require(dismo)

  output <- list()

  if(method == 'single2multi') {
    for (i in 1:length(preds.list)) {
      pred_spat <- terra::rast(preds.list[[i]])
      make.pred <- predicts::predict(object = model, x = pred_spat)
      output[[i]] <- raster::raster(make.pred)
    }
  }
  else if(method == 'multi2single') {
    for (i in 1:length(model)) {
      make.pred <- predicts::predict(object = model[[i]], x = terra::rast(preds.list))
      output[[i]] <- raster::raster(make.pred)
    }
  }
  output.pred <- raster::stack(output)
  names(output.pred) = pred.names
  return(output.pred)
}
