#' Sample background points for (multiple) species to calibrate ecological niche models
#'
#' @param envs RasterLayer of environmetal variable from which to draw background points
#' @param n Numerical value for the desired number of background points to sample
#' @param occs_list A list containing occurrence datasets. Each occurrence set needs to have two columns named "long" and "lat", and in that order
#' @param buffer_list A list of spatial buffers. Only needed when "method = 'buffer'"is used
#' @param bias.grid RasterLayer of bias grid to correct for spatial sampling bias. Only needed when "method = 'bias.grid'" is used
#' @param excludep Logical. Decides whether or not to exclude occurrence points from background sampling. Only used for "random" and "buffer" methods
#' @param method Character. Method to use for background sampling. If "random" is used, the background points are sampled randomly from the provided raster layer.
#' If "buffer" is used, the background points are sampled from within the boundary of provided buffer(s). If "bias.grid" is used, sample bias-corrected background points
#' from the provided bias grid. The method is "random" by default
#' @returns A list containing sampled background points
#' @examples
#' bg <- bgSampler(envs = envs, n = 10000, occs_list = occs_list, method = 'random')

bgSampler <- function(envs, n, occs_list, buffer_list = NULL, bias.grid = NULL, excludep = NULL, method = 'random') {
  bg.out <- list()

  if (method == 'random') {
    for (i in 1:length(occs_list)) {
      bg <- dismo::randomPoints(mask = envs, n = n, p = occs_list[[i]], excludep = excludep) %>% as.data.frame()
      colnames(bg) = c('long', 'lat')

      bg.out[[i]] <- bg
    }
  }
  else if (method == 'buffer') {
    for (i in 1:length(buffers)) {
      mask <- raster::mask(envs, buffers[[i]])
      bg <- dismo::randomPoints(mask = envs, n = n, p = occs_list[[i]], excludep = excludep) %>% as.data.frame()
      colnames(bg) = c('long', 'lat')

      bg.out[[i]] <- bg
    }
  }
  else if (method == 'bias.grid') {
    bg <- xyFromCell(bias.grid, sample(which(!is.na(values(subset(envs, 1)))), n, prob = values(bias.grid)[!is.na(values(subset(envs, 1)))])) %>% as.data.frame()
    colnames(bg) = c('long', 'lat')
    bg.out <- bg
  }
  return(bg.out)
}
