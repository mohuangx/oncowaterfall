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
#'   scale_x_reverse scale_y_continuous expansion guides theme_bw theme
#'   element_blank element_text element_line margin unit
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
    scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
                       limits = c(-100, 100)) +
    guides(fill = "none") +
    theme_bw(base_size = 14) +
    theme(legend.position = c(0.8, 0.88),
          plot.margin = margin(7, 10, 7, 7),
          panel.grid = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          axis.text = element_text(colour = "black", size = 11),
          axis.title = element_text(face = "bold", size = 12),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5))
  p
}

#' Plots waterfall curve
#'
#' \code{plot_waterfall_curve} returns a ggplot object which presents best tumor
#' size change as a step function rather than the typical waterfall bar plot.
#'
#' @param x1 Vector of best tumor size changes for treatment 1
#' @param x2 Vector of best tumor size changes for treatment 2
#' @param name1 Name for treatment 1
#' @param name2 Name for treatment 2
#' @param plot_ci1 Whether to plot confidence interval for x1. Default is FALSE.
#' @param plot_ci2 Whether to plot confidence interval for x2. Default if FALSE.
#' @param hline Y-coordinate for dotted line
#' @param include_diff Whether to include difference in proportions plot
#' @param conf_level Confidence level for difference in proportions plot
#'
#' @return A ggplot step plot.
#'
#' @examples
#' x1 <- sample(-100:100, 25)
#' x2 <- sample(-100:100, 20)
#' plot_waterfall_curve(x1, x2)
#'
#' @importFrom ggplot2 ggplot aes .data geom_step scale_colour_manual geom_hline
#'   scale_x_reverse scale_y_continuous coord_cartesian labs theme_bw theme
#'   element_blank element_text element_line margin unit geom_ribbon
#' @importFrom dplyr .data bind_rows mutate group_by slice n
#' @importFrom stats setNames
#' @importFrom patchwork plot_layout
#'
#' @export
plot_waterfall_curve <- function(x1, x2 = NULL, name1 = "Treatment",
                                 name2 = "Control", plot_ci1 = FALSE,
                                 plot_ci2 = FALSE, hline = 0,
                                 include_diff = TRUE, conf_level = 0.95) {
  x1 <- sort(x1, decreasing = TRUE)
  x2 <- sort(x2, decreasing = TRUE)
  df <- bind_rows(setNames(list(data.frame(PCHG = x1), data.frame(PCHG = x2)),
                     c(name1, name2)),
            .id = "Treatment") %>%
    mutate(Treatment = factor(.data$Treatment,
                              levels = c(name1, name2))) %>%
    group_by(.data$Treatment) %>%
    slice(c(1:n(), n())) %>%
    mutate(ind = round((1-0:(n()-1)/(n()-1))*100, 1),
           PCHG = round(.data$PCHG))
  p1 <- ggplot(df) +
    geom_step(aes(.data$ind, .data$PCHG, colour = .data$Treatment), size = 0.8) +
    scale_colour_manual(values = c("#6585b4", "#aacc6f")) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = hline, linetype = 2, size = 0.2) +
    scale_x_reverse(limits = c(100, 0)) +
    scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) +
    coord_cartesian(ylim = c(-100, 100), clip = "off") +
    labs(y = "Change from Baseline (%)",
         x = "Proportion of Patients (%)", fill = "",
         colour = "") +
    theme_bw(base_size = 14) +
    theme(legend.position = c(0.8, 0.88),
          plot.margin = margin(7, 10, 7, 7),
          panel.grid = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          axis.text = element_text(colour = "black", size = 11),
          axis.title = element_text(face = "bold", size = 12),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5))
  if (plot_ci1) {
    ci1 <- ecdf_ci(x1)
    p1 <- p1 + geom_ribbon(data = ci1,
                           aes(.data$Est, y = .data$Value, xmin = .data$LCL,
                               xmax = .data$UCL),
                           fill = "#6585b4", alpha = 0.2)
  }
  if (plot_ci2) {
    ci2 <- ecdf_ci(x2)
    p1 <- p1 + geom_ribbon(data = ci2,
                           aes(.data$Est, y = .data$Value, xmin = .data$LCL,
                               xmax = .data$UCL),
                           fill = "#aacc6f", alpha = 0.2)
  }
  if (include_diff & !is.null(x2)) {
    p2 <- plot_waterfall_diff(x1, x2, name1, name2, conf_level, hline)
    p <- p1 + p2 + plot_layout(widths = c(2, 1))
    p
  } else {
    p1
  }
}


#' Plots rate difference between waterfall curves
#'
#' \code{plot_waterfall_diff} returns a ggplot object which is the rate
#' difference of two waterfall curves across the range of best tumor size
#' change.
#'
#' @param x1 Vector of best tumor size changes for treatment 1
#' @param x2 Vector of best tumor size changes for treatment 2
#' @param name1 Name for treatment 1
#' @param name2 Name for treatment 2
#' @param conf_level Confidence level
#' @param hline Y-coordinate for dotted line
#'
#' @return A ggplot step plot.
#'
#' @examples
#' x1 <- sample(-100:100, 25)
#' x2 <- sample(-100:100, 20)
#' plot_waterfall_diff(x1, x2)
#'
#' @importFrom ggplot2 ggplot aes .data geom_step geom_ribbon annotate arrow
#'   scale_colour_manual geom_hline geom_vline scale_x_continuous
#'   scale_y_continuous coord_flip labs theme_bw theme element_blank
#'   element_text element_line margin unit
#' @importFrom dplyr .data bind_rows mutate group_by slice n across
#' @importFrom stats setNames
#'
#' @export
plot_waterfall_diff <- function(x1, x2, name1 = "Treatment", name2 = "Control",
                                conf_level = 0.95, hline = 0) {
  df_diff <- ecdf_diff(x1, x2, conf_level = conf_level) %>%
    mutate(across(.data$Est:.data$UCL, ~ round(.x, 1)))
  xmin <- -max(abs(df_diff$LCL), abs(df_diff$UCL))
  xmax <- -xmin
  ggplot(df_diff) + geom_step(aes(.data$Value, .data$Est), size = 0.8) +
    geom_ribbon(aes(.data$Value, ymin = .data$LCL, ymax = .data$UCL),
                alpha = 0.3) +
    labs(x = "",
         y = paste(name1, "\u2013", name2, "(%)"),
         title = "Difference in Proportions") +
    scale_x_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) +
    scale_y_continuous(limits = c(xmin, xmax),
                       expand = expansion(mult = 0)) +
    coord_flip(xlim = c(-100, 100), clip = "off") +
    geom_vline(xintercept = 0, size = 0.6) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.6) +
    geom_vline(xintercept = hline, linetype = 2, size = 0.2) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(5.5, 5.5, 5.5, 0),
          panel.grid = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          axis.text = element_text(colour = "black", size = 11),
          axis.title = element_text(face = "bold", size = 12),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5),
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
}
