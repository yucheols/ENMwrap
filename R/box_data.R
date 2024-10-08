#' Miscellaneous tool to aid data visualization: format environmental data extracted from rasters to make boxplots in ggplot2
#' @param sp.name Charcter. Input species name
#' @param envs RasterStack of environmental layers
#' @param pts A data.frame of species occurrence points having two columns, in the order of "long" and "lat"
#' @returns A data.frame of formatted for use in ggplot2
#' @examples
#' get.box.data <- box_data(sp.name = 'Karsenia koreana', envs = envs, pts = occs)

box_data <- function(sp.name, envs, pts) {
  require(dplyr)

  output <- list()

  for (i in 1:length(names(envs))) {
    val <- raster::extract(envs[[i]], pts) %>% as.data.frame()
    val$var = names(envs)[i]
    val$species = sp.name
    colnames(val) = c('val', 'var', 'Species')
    output[[i]] <- val
  }
  output <- dplyr::bind_rows(output)
  return(output)
}
