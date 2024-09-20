#' Extract MaxEnt response curve data for further plot customization
#'
#' @param sp.name Character. The name of the taxon being used in niche modeling
#' @param model A fitted ENM of the class MaxEnt. Can be extracted by accessing the "models" slot output from "test_multibg" or "test_multisp"
#' @param names.var Character. Names of environmental variables used to generate the MaxEnt model
#' @returns A data.frame containing the response data that can be used to further customize the response curve plots
#' @examples
#' resp.data <- respDataPull(sp.name = 'Karsenia koreana', model = mx_model, var = c('bio1', 'bio12', 'slope'))


resp_data_pull <- function(sp.name, model, names.var) {
  require(dplyr)

  resp.data <- list()

  for (i in 1:length(names.var)) {
    resp <- as.data.frame(dismo::response(x = model, var = names.var[[i]]))
    colnames(resp) = c('x','y')
    resp$var = names.var[[i]]
    resp.data[[i]] <- resp
    resp.data.out <- dplyr::bind_rows(resp.data)
    resp.data.out$Species = sp.name
  }
  return(resp.data.out)
}
