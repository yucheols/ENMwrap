#' Calculate the range size of suitable habitats from binary presence/absence maps
#'
#' @param bin.stack RasterStack object containing presence/absence maps converted from continuous ENM predictions
#' @param bin.labs Vector of layer names corresponding to the presence/absence layers
#' @param digits Integer value used to round the results of range calculation
#' @returns A list where each slot contains the calculated range value for each presence/absence map
#' @examples
#' get_ranges <- calc_ranges(bin.stack = bin, bin.labs = names(bin), digits = 0)

calc_ranges <- function(bin.stack, bin.labs, digits) {
  require(raster)

  output <- list()

  for (i in 1:nlayers(bin.stack)) {
    # read raster
    r <- bin.stack[[i]]

    # range calc
    r[r < 1] <- NA
    r <- raster::area(r, na.rm = T, weights = F)
    r <- r[!is.na(r)]
    r_ext <- length(r) * median(r)
    output[[i]] <- paste(bin.labs[[i]], round(r_ext, digits = digits), 'km2')
  }
  return(output)
}

