source("util.R")

AGE_LOW = 50
AGE_HIGH = 70

default_theme <- function(text_size = 10) {
  theme_bw() + 
    theme_centered_title() +    
    theme(axis.title = element_text(size=text_size+5, color = "black"), 
          axis.text = element_text(size = text_size, color = "black"))
}

psa_hist_plot <- function(data, hist_x_title) {
  ggplot(data, aes(x = PSA)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  xlab(hist_x_title) +
  ylab("Häufigkeit")
}

psa_qq_plot <- function(data, band_type) {
  ggplot(data, aes(sample = PSA)) +
  stat_qq_band(mapping = aes(fill = "red", alpha = 0.5), bandType= band_type, show.legend = FALSE) +
  stat_qq_point() +
  stat_qq_line() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Theoretische Quantile") +
  ylab("Empirische Quantile") 
}

hist_qqplot <- function(data, predicate, band_type = "ell", hist_x_title = "PSA Wert") {

  hist_plot <- psa_hist_plot(data, hist_x_title) + default_theme()
  qq_plot <- psa_qq_plot(data, band_type) + default_theme()

  hist_plot | qq_plot
}

psa_predicate <- function(data, age_min, age_max, psa_max) {
  data$Age >= age_min & data$Age <= age_max & data$PSA < psa_max
}

psa_filter <- function(age_min, age_max, psa_max) {
  partial_func(psa_predicate, age_min = age_min, age_max = age_max, psa_max = psa_max)
}

psa <- read.csv("psa.csv", header = TRUE)
psa_subset <- subset(psa, psa_filter(AGE_LOW, AGE_HIGH, 6)(psa))
psa_plot <- hist_qqplot(psa_subset, band_type = "ell", hist_x_title = "PSA Wert")
print(psa_plot)
#ggsave(filename = tex_figures_path("psa.png"), plot = psa_plot)

log_psa <- data.frame(Age = psa$Age, PSA = log(psa$PSA))
log_psa_subset <- subset(log_psa, psa_filter(AGE_LOW, AGE_HIGH, 4)(log_psa))
log_psa_plot <- hist_qqplot(log_psa_subset, band_type = "ell", hist_x_title = "log(PSA Wert)")
print(log_psa_plot)
#ggsave(filename = tex_figures_path("log_psa.png"), plot = log_psa_plot)

psa_vs_log_psa <- psa_plot / log_psa_plot
print(psa_vs_log_psa)
#ggsave(filename = tex_figures_path("psa_vs_log_psa.png"), plot = psa_vs_log_psa)

log_psa_qqplot_ks <- psa_qq_plot(log_psa_subset, "ks") + default_theme()
print(log_psa_qqplot_ks)
#ggsave(filename = tex_figures_path("log_psa_qqplot_ks.png"), plot = log_psa_qqplot_ks)

