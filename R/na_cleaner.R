


na_cleaner <- function(pts, envs, x, y) {
  cleaned_pts <- list()

  if (length(pts) => 2) {
    for (i in 1:length(pts)) {
      ext <- raster::extract(envs, pts[[i]]) %>% as.data.frame()
      ext.bind <- cbind(ext, pts[[i]]) %>% na.omit() %>% dplyr::select(x, y)
      cleaned_pts[[i]] <- ext.bind
    }
  }

  if (length(pts) == 1) {
    ext <- raster::extract(envs, pts) %>% as.data.frame()
    cleaned_pts <- cbind(ext, pts) %>% na.omit() %>% dplyr::select(x, y)
  }
  return(cleaned_pts)
}
