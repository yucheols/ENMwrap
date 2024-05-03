#' MaxEnt model tuning with multiple background datasets
#'
#' @param taxon.name Character. The name of the taxon being used in niche modeling
#' @param occs Data frame of species occurrence points with two columns, in the order of "long" and "lat"
#' @param envs RasterStack of environmental data
#' @param bg.list A list of background points to test within the modeling tuning workflow. Each background set should be in a two-column data.frame format with column names matching the occurrence dataset
#' @param tune.args A named list containing feature class settings ("fc") and regularization values ("rm"). Use ?ENMevaluate for details
#' @param partitions Character. The name of data partitioning strategy to use for model evaluations. Can select from: "randomkfold", "block", "checkerboard1", "checkerboard2", "testing", and "user"
#' @param partition.settings A named list used to specify settings for the selected partitioning scheme
#' @param user.grp A named list designating user-specified cross-validation folds. Only required when "partition.settings" is set to "user"
#' @param type Character. Select a method for selecting the optimal parameter combinations. If "type1" is used, the optimal model is selected based on the minimum 10 percent omission rate as a primary criterion, minimum AUCDIFF value as a secondary criterion, and maximum validation AUC as a tertiary criterion.
#' If "type2" is used, the primary, secondary, and tertiary criteria for model selection would be delta.AICc smaller than 2, minimum 10 percent omission rate, and maximum validation AUC, respectively
#' @returns A named list with slots for model metrics of selected models, model objects, model predictions, and variable importance
#' @examples
#' test_bg <- test_multibg(taxon.name = 'Karsenia koreana', occs = occs, envs = envs, bg.list(bg1, bg2, bg3), tune.args = tune.args, partitions = 'randomkfold', partition.settings = list(kfolds = 10), type = 'type1')


test_multibg <- function(taxon.name, occs, envs, bg.list, tune.args, partitions, partition.settings = NULL, user.grp = NULL, type) {
  require(dplyr)
  require(ENMeval)
  require(raster)

  output <- list()
  models <- list()
  preds <- list()
  contrib <- list()

  if (type == 'type1') {
    for (i in 1:length(bg.list)) {

      # make models
      eval <- ENMeval::ENMevaluate(taxon.name = taxon.name, occs = occs, envs = envs, bg = bg.list[[i]],
                                   tune.args = tune.args, partitions = partitions, partition.settings = partition.settings,
                                   user.grp = user.grp[[i]], doClamp = T, algorithm = 'maxent.jar', parallel = T, parallelType = 'doSNOW')

      # get results
      eval.res <- ENMeval::eval.results(eval)

      # get optimal parameter combinations
      opt.param <- eval.res %>% dplyr::filter(or.10p.avg == min(or.10p.avg)) %>%
        dplyr::filter(auc.diff.avg == min(auc.diff.avg)) %>%
        dplyr::filter(auc.val.avg == max(auc.val.avg))

      output[[i]] <- opt.param
      metrics <- dplyr::bind_rows(output)

      # get optimal model per iteration
      opt.model <- ENMeval::eval.models(eval)[[opt.param$tune.args]]
      models[[i]] <- opt.model

      # get variable importance for each best model
      var.imp <- ENMeval::eval.variable.importance(eval)[[opt.param$tune.args]]
      contrib[[i]] <- var.imp

      # get optimal predictions per iteration
      opt.pred <- ENMeval::eval.predictions(eval)[[opt.param$tune.args]]
      preds[[i]] <- opt.pred
      preds.stack <- raster::stack(preds)
    }
  }
  else if (type == 'type2') {
    for (i in 1:length(bg.list)) {

      # make models
      eval <- ENMeval::ENMevaluate(taxon.name = taxon.name, occs = occs, envs = envs, bg = bg.list[[i]],
                                   tune.args = tune.args, partitions = partitions, partition.settings = partition.settings,
                                   user.grp = user.grp[[i]], doClamp = T, algorithm = 'maxent.jar', parallel = T, parallelType = 'doSNOW')

      # get results
      eval.res <- ENMeval::eval.results(eval)

      # get optimal parameter combinations
      opt.param <- eval.res %>% dplyr::filter(delta.AICc <= 2) %>%
        dplyr::filter(or.10p.avg == min(or.10p.avg)) %>%
        dplyr::filter(auc.val.avg == max(auc.val.avg))

      output[[i]] <- opt.param
      metrics <- dplyr::bind_rows(output)

      # get optimal model per iteration
      opt.model <- ENMeval::eval.models(eval)[[opt.param$tune.args]]
      models[[i]] <- opt.model

      # get variable importance for each best model
      var.imp <- ENMeval::eval.variable.importance(eval)[[opt.param$tune.args]]
      contrib[[i]] <- var.imp

      # get optimal predictions per iteration
      opt.pred <- ENMeval::eval.predictions(eval)[[opt.param$tune.args]]
      preds[[i]] <- opt.pred
      preds.stack <- raster::stack(preds)
    }
  }
  return(list(metrics = metrics, models = models, preds = preds.stack, contrib = contrib))
}
