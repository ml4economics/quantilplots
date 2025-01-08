source("util.R")

single_plot <- function(data, color, title = NULL, aspect_ratio=1) {
  gg <- ggplot(data, aes(x = x, y = y)) +
    geom_line(color = color) +
    labs(x = NULL, y = NULL)
  if ( !is.null(title) ) {
    gg <- gg + ggtitle(title)
  }
  
  gg <- gg + theme_bw() +    
             theme(plot.title = element_text(size=15, color = "black", hjust = 0.5), 
                   axis.text = element_text(size = 10, color = "black"),
                   aspect.ratio=aspect_ratio)
}

combined_plot <- function(distribution, pdf_data, cdf_data, quantile_data) {
  pdf_graph <- single_plot(pdf_data, "blue", "Dichte")
  cdf_graph <- single_plot(cdf_data, "green", "Verteilung")
  quantile_graph <- single_plot(quantile_data, "red", "Quantilplot")
  
  (pdf_graph | cdf_graph | quantile_graph) +
    plot_annotation(title = distribution, 
                    theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
}

combined_plot2 <- function(distribution, x, dfunc, pfunc, qfunc) {
  pdf_data  <- data.frame(x = x, y = dfunc(x))
  cdf_data  <- data.frame(x = x, y = pfunc(x))
  p <- seq(0, 1, length=100)
  quantile_data  <- data.frame(x = p, y = qfunc(p))
  
  combined_plot(distribution, pdf_data, cdf_data, quantile_data)
}

# Beispiel 1 : Normalverteilung
qplot_normal = combined_plot2(NULL, 
                              x <- seq(-3, 3, length=100), 
                              dnorm, 
                              pnorm, 
                              qnorm)

print(qplot_normal)
#ggsave(filename = tex_figures_path("qplot_normal.png"), plot = qplot_normal)

# Beispiel 2 : Chi-Quadrat-Verteilung - Rechtsschief
df <- 5
qplot_chi_square <- combined_plot2(NULL, 
                                   seq(0, 20, length=100), 
                                   partial_func(dchisq, df=df), 
                                   partial_func(pchisq, df=df),  
                                   partial_func(qchisq, df=df))

print(qplot_chi_square)
#ggsave(filename = tex_figures_path("qplot_chi_square.png"), plot = qplot_chi_square)

# Beispiel 3 : Mischverteilung
mu1 <- 0
sigma1 <- 1
mu2 <- 6
sigma2 <- 1

a <- 0.5
b <- 0.5

n <- 100000
set.seed(123)
component1 <- rnorm(n, mean = mu1, sd = sigma1)
component2 <- rnorm(n, mean = mu2, sd = sigma2)
mixture_data <- ifelse(runif(n) < 0.5, component1, component2)

# Empirische Dichtefunktion
density_estimate <- density(mixture_data)
empirical_pdf <- approxfun(density_estimate$x, density_estimate$y)

# Empirische Verteilungsfunktion
empirical_cdf = ecdf(mixture_data)

# Dichte & Verteilung
x <- seq(-3, 10, length=100)
pdf_data <- data.frame(x = x, y = empirical_pdf(x))
# alternativ : pdf <- a * dnorm(x, mean = mu1, sd = sigma1) + b * dnorm(x, mean = mu2, sd = sigma2)
cdf_data <- data.frame(x = x, y = empirical_cdf(x))
# alternativ : cdf <- a * pnorm(x, mean = mu1, sd = sigma1) + b * pnorm(x, mean = mu2, sd = sigma2)

# Quantile
p <- seq(0, 1, length=100)
quantile_data <- data.frame(x = p, y = quantile(mixture_data, p))

# Plot
qplot_bimodal <- combined_plot(NULL, pdf_data, cdf_data, quantile_data)
print(qplot_bimodal)
#ggsave(filename = tex_figures_path("qplot_bimodal.png"), plot = qplot_bimodal)

