#' Plot MaxEnt response curves
#'
#' @param resp.data A data.frame object output from "respDataPull" function
#' @param ncol The number of columns to use for ggplot2 facetting. Default is NULL
#' @param nrow The number of rows to use for ggplot2 facetting. Default is NULL
#' @param color Color palette to use for response curves
#' @param fill Color fill to represent the variance between replicate models
#' @param linewidth Line width for response curves. Default is 1.2
#' @param alpha Transparency. Default is 0.4
#'
#' @returns A ggplot2-style facetted plot of the MaxEnt response curves
#' @examples
#' resp.plot <- plot_response(resp.data = resp.data, ncol = 2, nrow = 3, color = 'cornflowerblue', fill = 'grey', linewidth = 1.2, alpha = 0.4)

plot_response <- function(resp.data, ncol = NULL, nrow = NULL, color = 'cornflowerblue', fill = 'grey', linewidth = 1.2, alpha = 0.4) {
  require(ggplot2)

  resp.data.out <- ggplot(data = resp.data, aes(x = x, y = y)) +
    facet_wrap(~ var, scales = 'free', ncol = ncol, nrow = nrow) +
    geom_line(color = color, linewidth = linewidth) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = fill, alpha = alpha) +
    xlab('Variable') + ylab('Suitability') +
    theme_bw()

  return(resp.data.out)
}
