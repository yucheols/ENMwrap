#' Generate spatial blocks based on the estimated block size
#'
#' @param occs A data.frame of species occurrence points containing two columns, in the order of "long" and "lat"
#' @param bg.list A list of background datasets, each dataset in data.frame format. The occurrence and background datasets must have matching column names
#' @param envs RasterStack of environmental variables
#' @param k Integer. The number of spatial folds to generate
#' @param block.size A list of block sizes output from the "block_size" function
#' @returns A list with two slots: one for occurrence partitions, another for background partitions
#' @examples
#' folds <- fold_maker(occs = occs, bg.list = list(bg1, bg2, bg3), envs = envs, k = 4, block.size = block.size)
#'
#' occs.folds <- folds[[1]]
#' bg.folds <- folds[[2]]

fold_maker <- function(occs, bg.list, envs, k, block.size) {
  require(dplyr)
  require(raster)
  require(sf)
  require(blockCV)

  occ.fold.out <- list()
  bg.fold.out <- list()

  fold.maker <- for (i in 1:length(bg.list)) {

    # bind occ & bg ::: both datasets should have matching column names
    occs_bg <- rbind(occs, bg.list[[i]])
    occs_bg$occ <- c(rep(1, nrow(occs)), rep(0, nrow(bg.list[[i]])))

    occs_bg_env <- raster::extract(envs, occs_bg[, c(1,2)]) %>% as.data.frame()
    occs_bg_env <- cbind(occs_bg_env, occs_bg) %>% na.omit()

    pts_cols <- occs_bg_env[, c('long', 'lat', 'occ')]
    pts_sf <- sf::st_as_sf(pts_cols, coords = c('long', 'lat'), crs = 4326)

    spat_folds <- blockCV::cv_spatial(x = pts_sf, column = 'occ', r = envs, k = k, hexagon = T, flat_top = F,
                                      size = block.size[[i]], selection = 'random', iteration = 100, seed = 333,
                                      progress = T, plot = F)

    folds_ids <- as.data.frame(spat_folds$folds_ids)
    colnames(folds_ids) = 'fold_id'

    folds <- cbind(pts_cols, folds_ids)

    occ_folds <- folds %>% dplyr::filter(occ == 1)
    bg_folds <- folds %>% dplyr::filter(occ == 0)

    occ.fold.out[[i]] <- occ_folds
    bg.fold.out[[i]] <- bg_folds

  }
  return(list(occ.fold.out, bg.fold.out))
}
