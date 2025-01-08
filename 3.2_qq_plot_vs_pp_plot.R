source("util.R")

mu <- 0  
sd_values <- c(0.5, 1, 2) 

# Dichtefunktionen
x <- seq(-6, 6, length.out = 200)

plot_data <- data.frame(
  x = rep(x, times = length(sd_values)),
  y = c(dnorm(x, mean = mu, sd = sd_values[1]),
        dnorm(x, mean = mu, sd = sd_values[2]),
        dnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)

p1 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(linewidth = 1) +
  ggtitle("Normalverteilungen") +
  xlab("Wert") +
  ylab("Dichte") +
  scale_color_discrete(name = "Standardabweichung")

# PP Plot
x <- seq(-10, 10, length.out = 200)
ref = pnorm(x, mean = mu, sd = sd_values[2])
plot_data <- data.frame(
  x = rep(ref, times = length(sd_values)),
  y = c(pnorm(x, mean = mu, sd = sd_values[1]),
        ref,
        pnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)
p2 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(linewidth = 1, show.legend=FALSE) +
  ggtitle("P-P Plot") +
  xlab("Standardnormalverteilung") +
  ylab("Normalverteilungen")

# QQ Plot
x <- seq(0, 1, length.out = 50)
ref = qnorm(x, mean = mu, sd = sd_values[2])
plot_data <- data.frame(
  x = rep(ref, times = length(sd_values)),
  y = c(qnorm(x, mean = mu, sd = sd_values[1]),
        ref,
        qnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)

p3 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_point(size=1, show.legend=FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("Q-Q Plot") +
  xlab("Standardnormalverteilung") +
  ylab("Normalverteilungen") +
  xlim(-2.5, 2.5) +
  ylim(-2.5, 2.5) 

plot_theme = theme_minimal() + 
             theme(plot.title = element_text(hjust = 0.5))

p1_themed <- p1 + plot_theme
p2_themed <- p2 + plot_theme
p3_themed <- p3 + plot_theme

gg <- p1_themed / (p2_themed + p3_themed) + plot_layout(guides = 'collect')

print(gg)

#ggsave(filename = tex_figures_path("pp_plot_vs_qq_plot.png"), plot = gg, width = 8, height = 6, dpi = 300)
