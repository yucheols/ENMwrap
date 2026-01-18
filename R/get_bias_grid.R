#' Generate a bias grid to use for spatial sampling bias correction
#'
#' @param targ.pts A data.frame of target group occurrence points containing two columns, in the order of "long" and "lat"
#' @param envs RasterStack of environmental data
#' @param poly SpatialPolygonsDataFrame object to mask the output bias grid
#' @returns RasterLayer of generated bias grid
#' @examples
#' bias <- get_bias_grid(targ.pts = targ.pts, envs = envs, poly = poly)

get_bias_grid <- function(targ.pts, envs, poly = NULL) {
  require(raster, quietly = T)
  require(rgdal, quietly = T)
  require(MASS, quietly = T)

  targ.ras <- rasterize(targ.pts, envs, 1)
  targ.pres <- which(values(targ.ras) == 1)
  targ.pres.locs <- coordinates(targ.ras)[targ.pres, ]

  targ.dens <- MASS::kde2d(targ.pres.locs[,1], targ.pres.locs[,2],
                           n = c(nrow(targ.ras), ncol(targ.ras)),
                           lims = c(extent(envs)[1], extent(envs)[2], extent(envs)[3], extent(envs)[4]))

  targ.dens.ras <- raster(targ.dens, envs)
  targ.dens.ras2 <- resample(targ.dens.ras, envs)

  bias.layer <- raster::mask(targ.dens.ras2, poly)
  return(bias.layer)
}
