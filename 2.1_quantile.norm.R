source("util.R")

# Daten
mean <- 0
sd <- 1
x <- seq(-3, 3, length.out = 1000)
y <- dnorm(x, mean, sd)
data <- data.frame(x = x, y = y)

quantiles <- c(-2, -1, 0, 1, 2)
quantile_values <- pnorm(quantiles)
quantile_data <- data.frame( quantiles = quantiles, values = quantile_values, labels = paste0(quantiles, "σ") ) 
quantile_data$labels <- gsub("1σ", "σ", quantile_data$labels)
quantile_data$labels <- gsub("0σ", "0", quantile_data$labels)

# Dichteplot
dist_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = quantiles, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  # interval [-σ,σ]
  annotate("segment", x = -1, xend = 1, y = -0.05, yend = -0.05, 
           arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "red") +
  annotate("text", x = 0, y = -0.055, label = "68.2%", color = "red", vjust = 1, hjust = 0.5) +
  # interval [-2σ,2σ]
  annotate("segment", x = -2, xend = 2, y = -0.1, yend = -0.1, 
           arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "red") +
  annotate("text", x = 0, y = -0.11, label = "95.4%", color = "red", vjust = 1, hjust = 0.5) +
  # σ labels
  annotate("text", x = quantiles, y = rep(0, length(quantiles)), 
           label = quantile_data$labels, color = "red", vjust = 1, hjust = -0.3) +
  labs(title = "Konfidenzintervalle Normalverteilung",
       x = NULL,
       y = "p") +
  scale_y_continuous(breaks = seq(0,0.4,0.1), minor_breaks = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Quantilplot
custom_breaks <- quantile_values
custom_labels <- format(round(quantile_values, 3), drop0trailing = TRUE)

quantile_plot <- ggplot(quantile_data, aes(x = quantiles, y = values)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = quantile_values, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = quantiles, labels = quantile_data$labels) +
  scale_y_continuous(labels = custom_labels, breaks = custom_breaks, minor_breaks = NULL) +
  labs(title = "p Werte der σ-Werte",
       x = "Quantile",
       y = "CDF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

combined_plot <- dist_plot | quantile_plot
print(combined_plot)
#ggsave(filename = tex_figures_path("quantil_conf_interval.png"), plot = combined_plot)
