#' Conduct Multivariate Environmental Similiarity Surfaces (MESS) analysis across different projection scenarios
#'
#' @param proj.env RasterStack object of the porjction data
#' @param proj.names A vector of layer names for the projection data
#' @param ref.env RasterStack object of reference (= calibration) data
#' @param occs A data.frame of species occurrence points containing two columns, with the "long" column followed by "lat" column
#' @returns RasterStack of MESS layer for each projection scenario
#' @examples
#' get.mess <- do.mess(proj.env = proj.stack, proj.names = c('LGM', 'Current', '2050'), ref.env = ref.stack, occs = occs)

do_mess <- function(proj.env, proj.names, ref.env, occs) {
  require(dplyr)
  require(raster)
  require(dismo)

  output <- list()
  ref.env.val <- raster::extract(ref.env, occs) %>% as.data.frame()

  for (i in 1:length(proj.names)) {
    mess.calc <- dismo::mess(proj.env[[i]], ref.env.val, full = F)
    output[[i]] <- mess.calc
  }
  stack.mess <- raster::stack(output)
  names(stack.mess) = proj.names

  return(stack.mess)
}
