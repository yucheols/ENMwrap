#' Plot MaxEnt response curves
#'
#' @param resp.data A data.frame object output from "respDataPull" function
#' @param ncol The number of columns to use for ggplot2 facetting. Default is NULL
#' @param nrow The number of rows to use for ggplot2 facetting. Default is NULL
#' @param color Color palette to use for response curves
#' @param linewidth Line width for response curves. Default is 1.2
#' @param alpha Transparency. Default is 0.4
#'
#' @returns An object of the class ggplot (a ggplot2-style facetted plot of the MaxEnt response curves). The plot can be further customized by appending the ggplot2 "theme()" function to the plot object output from "plot_response"
#' @examples
#' # Basic plotting
#' resp.plot <- plot_response(resp.data = resp.data, ncol = 2, nrow = 3, color = 'cornflowerblue', linewidth = 1.2, alpha = 0.4)
#'
#' # Further customization
#' resp.plot <- plot_response(resp.data = resp.data, ncol = 2, nrow = 3, color = 'cornflowerblue', linewidth = 1.2, alpha = 0.4) +
#'   theme(axis.title = element_text(size = 14, face = 'bold'),
#'         axis.title.x = element_text(margin = margin(t = 20)),
#'         axis.title.y = element_text(margin = margin(r = 20)),
#'         axis.text = element_text(size = 12),
#'         strip.text = element_text(size = 12),
#'         legend.title = element_text(size = 14, face = 'bold'),
#'         legend.text = element_text(size = 14, face = 'italic'),
#'         legend.position = 'top')

plot_response <- function(resp.data, ncol = NULL, nrow = NULL, color = 'cornflowerblue', linewidth = 1.2, alpha = 0.4) {
  require(ggplot2)

  resp.data.out <- ggplot(data = resp.data, aes(x = x, y = y)) +
    facet_wrap(~ var, scales = 'free', ncol = ncol, nrow = nrow) +
    geom_line(color = color, linewidth = linewidth) +
    xlab('Variable') + ylab('Suitability') +
    theme_bw()

  return(resp.data.out)
}
