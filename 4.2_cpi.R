source("util.R")
library(quantmod)
library(forecast)
library(fpp3)

# Retrieve CPI inflation data from FRED
cpi_data <- getSymbols("CPIAUCNS", 
                       src = "FRED", 
                       from='1971-01', to='2016-12', 
                       auto.assign = FALSE)

# Calculate the inflation rate
inflation_rate <- diff(log(cpi_data))[-1] * 400
quarterly_inflation_rate <- apply.quarterly(inflation_rate, mean)
quarters <- yearquarter(zoo::index(quarterly_inflation_rate))
df <- data.frame(Quarter=quarters, coredata(quarterly_inflation_rate$'CPIAUCNS'))
tsibble_data <- as_tsibble(df, index = Quarter)
cpi_ts_plot <- autoplot(tsibble_data) + theme_bw()
print(cpi_ts_plot)
#ggsave(filename = tex_figures_path("cpi_ts.png"), plot = cpi_ts_plot)

fit_cpi <- tsibble_data |> model(ARIMA(CPIAUCNS))
report(fit_cpi)

# Extract residuals from the fitted model
residuals <- residuals(fit_cpi)

default_theme <- function(text_size = 10) {
  theme_bw() + 
    theme_centered_title() +    
    theme(axis.title = element_text(size=text_size+5, color = "black"), 
          axis.text = element_text(size = text_size, color = "black"))
}

residuals_df <- data.frame(Date = df$Quarter, 
                           Residuals = as.numeric(residuals$.resid))

# Create Time Series Plot of Residuals
residuals_ts_plot <- ggplot(residuals_df, aes(x = Date, y = Residuals)) +
                     geom_line(color = "red") +
                     xlab("Quarter") +
                     ylab("Residuen") +
                     default_theme()

acf_plot <- ggAcf(residuals$.resid, lag.max = 10) +
            ggtitle(NULL) +
            default_theme()

residuals_plot <- residuals_ts_plot | acf_plot
print(residuals_plot)
#ggsave(filename = tex_figures_path("cpi_residuals_ts.png"), plot = residuals_plot)

hist_plot <- ggplot(residuals, aes(x = .resid)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
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
#ggsave(filename = tex_figures_path("cpi_residuals_qqplot.png"), plot = residuals_qqplot)

