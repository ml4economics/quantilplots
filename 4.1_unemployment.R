source("util.R")
library(fpp3)
library(forecast)

# draw the time series plot of the 'us_change' data set from fpp3
unemployment_ts_plot <- us_change |> autoplot(Unemployment)
unemployment_ts_plot <- unemployment_ts_plot + theme_bw()
print(unemployment_ts_plot)
#ggsave(filename = tex_figures_path("unemployment_ts.png"), plot = unemployment_ts_plot)

# fit the ARIMA model to the 'us_change' data set
fit_unemployment <- us_change |> model(ARIMA(Unemployment))
report(fit_unemployment)

# determine residuals
residuals <- residuals(fit_unemployment)

default_theme <- function(text_size = 10) {
  theme_bw() + 
  theme_centered_title() +    
  theme(axis.title = element_text(size=text_size+5, color = "black"), 
        axis.text = element_text(size = text_size, color = "black"))
}

residuals_ts_plot <- ggplot(residuals, aes(x = Quarter, y = .resid)) +
                     geom_line(color = "red") +
                     xlab("Quarter") +
                     ylab("Residuen") +
                     default_theme()

acf_plot <- ggAcf(residuals$.resid, lag.max = 10) +
            ggtitle(NULL) +
            default_theme()

residuals_plot <- residuals_ts_plot | acf_plot
print(residuals_plot)
#ggsave(filename = tex_figures_path("unemployment_residuals_ts.png"), plot = residuals_plot)

hist_plot <- ggplot(residuals, aes(x = .resid)) +
             geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
             xlab("Residuen") +
             ylab("Häufigkeit") +
             default_theme()

qqplot <- ggplot(residuals, aes(sample = .resid)) +
                 stat_qq_band(bandType = "ell", mapping = aes(fill = "red", alpha = 0.5), show.legend = FALSE) +
                 stat_qq_point() +
                 stat_qq_line() +
                 geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
                 xlab("Theoretische Quantile") +
                 ylab("Quantile Residuen") +
                 default_theme()

residuals_qqplot <- hist_plot | qqplot
print(residuals_qqplot)
#ggsave(filename = tex_figures_path("unemployment_residuals_qqplot.png"), plot = residuals_qqplot)
