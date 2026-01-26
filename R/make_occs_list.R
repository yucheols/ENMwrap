#' Format occurrence dataframe into a list for occurrence thinning
#'
#' @param occs_df Dataframe of occurrences merged across species
#' @param spp_list List of species names
#' @param long_col Character. Name of longitude column
#' @param lat_col Character. Name of latitude column
#' @examples
#' occs_list <- make_occs_list(occs_df = occs_df, spp_list = spp_list, long_col = 'long', latitude = 'lat')

make_occs_list <- function(occs_df, spp_list, long_col, lat_col) {
  out_list <- list()
  for (i in 1:length(spp_list)) {
    occs_put <- occs_df %>% dplyr::filter(species == spp_list[[i]]) %>% dplyr::select(long_col, lat_col)
    out_list[[i]] <- occs_put
  }
  return(out_list)
}

