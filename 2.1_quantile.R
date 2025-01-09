source("util.R")

use_gamma <- FALSE
p <- 0.3

if (use_gamma == TRUE) {
  shape <- 1
  scale <- 2
  x_low <- 0
  x_high <- 5
  pfunc <- partial_func(pgamma, shape = shape, scale = scale)
  qfunc <- partial_func(qgamma, shape = shape, scale = scale)
  distribution <- sprintf("Γ(%d,%d)-Verteilung", shape, scale)
} else {
  df <- 2
  x_low <- -2
  x_high <- 2
  pfunc <- partial_func(pt, df = df)
  qfunc <- partial_func(qt, df = df)
  distribution <- sprintf("t-Verteilung (df=%d)", df)
}

plot <- cdf_with_quantile_plot(pfunc, qfunc, 
                               x_low, x_high, 
                               p, 
                               NULL, 
                               label_size = 6) +
        theme(aspect.ratio=0.7) + 
        theme_text(axis_title=20) + 
        coord_cartesian(clip = "off")

# Display the plot
print(plot)
#ggsave(filename = tex_figures_path("quantil_simple.png"), plot = plot)

