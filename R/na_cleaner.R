#' Coordinate data cleaner to remove points that fall within pixels with no data (NA cells)
#'
#' If you are using user-specified folds (e.g. folds generated through blockCV, etc.), points that fall within pixels with no data can create errors when tuning the models
#' later on because the model tuning run will automatically drop these points but points sampled in the folds are not dropped. This creates a mismatch between the number of
#' samples included in the folds and the actual number of points used to generate the models. This function cleans the coordinate data and removes the points that fall within
#' NA pixels. The function returns cleaned coordinate data that can then be used to generate spatial CV folds.
#'
#' @param pts A list of coordinate data
#' @param envs A RasterStack of envirnonmental variables from which to sample pixel values
#' @param x Character. Longitude column name
#' @returns If more than one coordinate dataset was input as a list, a list of cleaned coordinate dataset is returned.
#' If one coordinate dataset is provided, a dataframe of cleaned dataset is returned.
#' @examples
#' clean.coords <- na_cleaner(pts = point.data, envs = envs, x = 'long', y = 'lat')


na_cleaner <- function(pts, envs, x, y) {
  cleaned_pts <- list()

  if (length(pts) >= 2) {
    for (i in 1:length(pts)) {
      ext <- raster::extract(envs, pts[[i]]) %>% as.data.frame()
      ext.bind <- cbind(ext, pts[[i]]) %>% na.omit() %>% dplyr::select(x, y)
      cleaned_pts[[i]] <- ext.bind
      print(paste0('The number of points after NA cleaning is   ', nrow(cleaned_pts[[i]])))
    }
  }

  if (length(pts) == 1) {
    pts <- dplyr::bind_rows(pts)
    ext <- raster::extract(envs, pts) %>% as.data.frame()
    cleaned_pts <- cbind(ext, pts) %>% na.omit() %>% dplyr::select(x, y)
    print(paste0('The number of points after NA cleaning is   ', nrow(cleaned_pts)))
  }
  print('NA cleaning completed!')
  return(cleaned_pts)
}
