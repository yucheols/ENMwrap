#' Check the units of calibration and projection data, using the maximum value of extracted raster cell values
#'
#' @param ref.env RasterStack object of calibration data
#' @param proj.env RasterStack object of projection data
#' @param n Numeric. The number of points to use to sample the input raster cells. This argument uses the "randomPoints" function of the dismo package
#' @return A data.frame object containing the maximum extracted cell values for calibration and projection data, respectively
#' @example
#' a <- unitCheck(ref.env = ref.stack, proj.env = proj.stack, n = 10000)

unitCheck <- function(ref.env, proj.env, n) {
  require(dplyr)
  require(dismo)

  results <- list()

  name.ref.env <- sort(names(ref.env))
  name.proj.env <- sort(names(proj.env))

  pts <- randomPoints(mask = ref.env[[1]], n = n) %>% as.data.frame()

  check <- for (i in 1:length(name.ref.env)) {
    ref.ex <- raster::extract(ref.env, pts) %>% na.omit() %>% as.data.frame()
    proj.ex <- raster::extract(proj.env, pts) %>% na.omit() %>% as.data.frame()

    ref.max <- max(ref.ex[[name.ref.env[[i]]]])
    proj.max <- max(proj.ex[[name.proj.env[[i]]]])

    results[[i]] <- data.frame(ref.max, proj.max)
  }
  results <- dplyr::bind_rows(results)
  results$var.name = name.ref.env
  return(results)
}
