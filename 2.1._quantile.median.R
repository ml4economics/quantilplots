source("util.R")

#Daten
df <- 5
x_values <- seq(0, 20, length.out = 1000)
density_data <- data.frame(x = x_values, y = dchisq(x_values, df = df))
cdf_data <- data.frame(x = x_values, y = pchisq(x_values, df = df))

median <- qchisq(0.5, df = df)
# Mittelwert einer Chi-Quadrat Verteilung = df
mean <- df

density_plot <- ggplot(density_data, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = median, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = mean, linetype = "dashed", color = "red") +
  annotate("text", x = median, y = 0.03, label = "Median", hjust = 1.1, color = "blue", size=3.5) +
  annotate("text", x = mean, y = 0.01, label = "Erwartungswert", hjust = -0.1, color = "red", size=3.5) +
  labs(title="Dichte", x = "x", y = "p") +
  theme(aspect.ratio=0.7, plot.title = element_text(hjust = 0.5))

cdf_plot <- ggplot(cdf_data, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = median, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean, color = "red", linetype = "dashed") +
  annotate("text", x = median, y = 0.75, label = "Median", hjust = 1.1, color = "blue", size=3.5) +
  annotate("text", x = mean, y = 0.25, label = "Erwartungswert", hjust = -0.1, color = "red", size=3.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(title="Verteilung", x = "x", y = "p") +
  theme(aspect.ratio=0.7, plot.title = element_text(hjust = 0.5))

median_plot = density_plot + cdf_plot
print(median_plot)

#ggsave(filename = tex_figures_path("median.png"), plot = median_plot)