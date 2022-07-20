#' Plots waterfall bar plot
#'
#' \code{plot_waterfall} returns a ggplot object which is the typical waterfall bar
#' plot with patients' best tumor size change sorted from largest to smallest.
#'
#' @param x Vector of best tumor size changes.
#' @param title Title of the plot.
#'
#' @return A ggplot bar plot.
#'
#' @examples
#' x <- sample(-100:100, 25)
#' plot_waterfall(x)
#'
#' @importFrom ggplot2 ggplot aes .data geom_rect geom_hline labs
#'   scale_x_reverse expansion guides theme_bw theme element_blank element_text
#'   element_line
#'
#' @export
plot_waterfall <- function(x, title = "") {
  n <- length(x)
  df <- data.frame(x = sort(x, decreasing = TRUE),
                   ind1 = (1-0:(n-1)/n)*100,
                   ind2 = (1-(1:n)/n)*100,
                   ind3 = paste0(n:1, "/", n))
  p <- ggplot(df, aes(xmin = .data$ind1, xmax = .data$ind2,
                      ymin = 0, ymax = .data$x,
                      text = paste0(.data$ind3, ": ", x, "%"))) +
    geom_rect(colour = "black", fill = "gray80",
              size = 0.05) +
    geom_hline(yintercept = c(0)) +
    labs(x = "", y = "Change from baseline (%)", title = title) +
    scale_x_reverse(expand = expansion(mult = 0.01)) +
    guides(fill = "none") +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(colour = "black", size = 11),
          axis.title = element_text(size = 12),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5))
  p
}

#' Plots waterfall curve
#'
#' \code{plot_waterfall_curve} returns a ggplot object which presents best tumor
#' size change as a step function rather than the typical waterfall bar plot.
#'
#' @param x1 Vector of best tumor size changes for treatment 1.
#' @param x2 Vector of best tumor size changes for treatment 2.
#' @param names
#' @param hline Y-coordinate for dotted line.
#'
#' @return A ggplot step plot.
#'
#' @examples
#' x1 <- sample(-100:100, 25)
#' x2 <- sample(-100:100, 20)
#' plot_waterfall_curve(x1, x2)
#'
#' @importFrom ggplot2 ggplot aes .data geom_rect geom_hline labs
#'   scale_x_reverse expansion guides theme_bw theme element_blank element_text
#'   element_line
#'
#' @export
plot_waterfall_curve <- function(x1, x2 = NULL, hline = 0) {
  ggplot(df, aes(ind, PCHG, colour = Treatment)) +
    geom_step(size = 0.8) +
    scale_colour_manual(values = c("#6585b4", "#aacc6f")) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = hline, linetype = 2, size = 0.2) +
    scale_x_reverse(limits = c(100, 0)) +
    scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) +
    coord_cartesian(ylim = c(-100, 100), clip = "off") +
    labs(y = "Change from Baseline (%)",
         x = "Proportion of Patients (%)", fill = "",
         colour = "") +
    theme_bw(base_size = 16, base_family = "sans") +
    theme(legend.position = c(0.8, 0.88),
          plot.margin = margin(7, 10, 7, 7),
          panel.grid = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 14),
          axis.text = element_text(colour = "black", size = 14),
          axis.title = element_text(size = 16),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5))
}
