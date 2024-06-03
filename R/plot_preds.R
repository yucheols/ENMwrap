#' Plot model prediction outputs
#'
#' @param preds RasterStack object of model prediction outputs
#' @param poly A polygon shapefile of area boundaries to overlay on top of the prediction rasters
#' @param ncol (optional) the number of columns to use in the plotting pane
#' @param nrow (optional) the number of rows to use in the plotting pane
#' @param colors A color palette to use for plotting
#' @param high Numeric. A higher-end value of estimated suitability. 0.9 by default
#' @param low Numeric. A lower-end value of estimated suitability. 0.1 by default
#'
#' @returns A ggplot2-style plot of model prediction outputs
#' @examples
#' plot_current <- plot_preds(preds = preds, poly = poly, ncol = 2, nrow = 3, colors = c('#2b83ba', '#abdda4', '#ffffbf', '#fdae61', '#4f05d7'), high = 0.9, low = 0.1)

plot_preds <- function(preds, poly, ncol = NULL, nrow = NULL, colors = c('#2b83ba', '#abdda4', '#ffffbf', '#fdae61', '#4f05d7'),
                       high = 0.9, low = 0.1, pred.names) {
  require(rasterVis)
  require(ggplot2)

  names(preds) = pred.names

  gplot(preds) +
    geom_tile(aes(fill = value)) +
    coord_equal() +
    facet_wrap(~ variable, ncol = ncol, nrow = nrow) +
    scale_fill_gradientn(colors =  colors,
                         na.value = NA,
                         name = 'Suitability',
                         breaks = c(low, high),
                         labels = c(paste0('Low: ', low), paste0('High: ', high))) +
    xlab('Longitude') + ylab('Latitude') +
    geom_polygon(data = poly, aes(x = long, y = lat, group = group), color = 'black', linewidth = 0.5, linetype = 'solid', fill = NA) +
    theme_bw() +
    theme(strip.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold', margin = margin(b = 10)),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = 'bold'),
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.title.y = element_text(margin = margin(r = 15)),
          axis.text = element_text(size = 12))
}
