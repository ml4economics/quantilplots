library(ggplot2)
library(patchwork)
suppressMessages({library(qqplotr)})
options(warn = -1)

#' Partial Application of Function 
#' 
#' Creates a new function by applying a subset of parameters
partial_func <- function(func, ...) {
  fixed_args <- list(...)
  function(...) {
    do.call(func, c(fixed_args, list(...)))
  }
}

#' Simple Q-Q Plot
#' 
#' Helper function to create a simple ggplot2 Plot
simple_ggplot <- function(data, xlab, ylab, color, title, size=1) {
  ggplot(data, 
         aes(x = x, y = y)) + 
         geom_line(color = color, size=size) +
         xlab(xlab) + 
         ylab(ylab) + 
         ggtitle(title)
}

#' Empty Q-Q Plot
#' 
#' Creates an empty plot that can be used for plot arrangements.
empty_plot <- function() {
  ggplot() + theme_void() + theme(aspect.ratio = 1)
}

#' Theme for Square Size Plot
#' 
#' Convenience theme to configure a square sized plot.
theme_squared <- function() {
  theme(aspect.ratio = 1)
}

#' Theme for Centered Title
#' 
#' Convenience theme to configure a centered plot title
theme_centered_title <- function(size=30) {
  theme(plot.title = element_text(size=size, , color = "black", hjust = 0.5))
}

#' Text Size Theme
#' 
#' Convenience theme to configure sizes of axis and legend texts.
theme_text <- function(axis_title_size=25, legend_title_size=30) {
  theme(axis.title = element_text(size = axis_title_size, color = "black"),        
        axis.text = element_text(size = axis_title_size-5, color = "black"),
        legend.title = element_text(size = legend_title_size, color = "black"),
        legend.text = element_text(size = legend_title_size-5, color = "black"))
}

#' Jupyter Theme
#' 
#' Theme that configures text sizes and colors such that display nicely in a Jupyter notebook.
theme_jupyter <- function(axis_title_size=25, legend_title_size=30) {
  theme_bw() +
  theme_centered_title() +
  theme_text(axis_title_size = axis_title_size, legend_title_size = legend_title_size)
}

#' Display the Quantile of a Distribution
#' 
#' Helper function to draw the quantile for an arbitrary distribution
#'
#' @param pfunc distribution function
#' @param qfunc quantile function
#' @param p probability for quantile to be displayed
#' @param distribution distribution name for label
cdf_with_quantile_plot <- function(pfunc, qfunc, 
                                   x_low, x_high, 
                                   p, 
                                   distribution = "Verteilungsfunktion",
                                   cdf_color = "blue",
                                   quantile_color = "red",
                                   label_size = 4) {
  x <- seq(x_low, x_high, length.out = 100)
  cdf <- pfunc(x)
  quantile <- qfunc(p)
  df <- data.frame(x = x, prob = cdf)
  
  plot <- ggplot(df, aes(x = x, y = prob)) + geom_line(color = cdf_color, linewidth=1) + 
    geom_segment(x = quantile, y = 0, xend = quantile, yend = p, color = quantile_color, linewidth=1) + 
    geom_segment(x = x_low, y = p, xend = quantile, yend = p, color = quantile_color, linewidth=1) +
    annotate("text", 
             x = quantile, y = -0.05, 
             label = sprintf("Q(%.1f)=%.2f", p, quantile), 
             color = quantile_color, vjust = 1, size = label_size) + 
    annotate("text", 
             x = x_low, y = p, 
             label = sprintf("%.1f", p), 
             color = quantile_color, hjust = 1, size = label_size) +
    xlab("x") + ylab("CDF")
  
  if ( ! is.null(distribution) ) {
    plot <- plot + labs(title = sprintf("%.1f-Quantil einer %s", p, distribution))
  }
  plot
}