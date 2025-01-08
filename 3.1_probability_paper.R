source("util.R")

n=25
inc = 1.0/n
probabilities <- seq(inc, 1-inc, by = inc)
theoretical_quantiles <- qnorm(probabilities)

set.seed(123)
data <- data.frame(samples = rnorm(length(probabilities)))
sample_quantiles <- quantile(data$samples, probabilities)

data.frame(theoretical_quantiles, sample_quantiles)

prob_paper <- ggplot(data, aes(x = theoretical_quantiles, y = sample_quantiles)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  #ggtitle("Wahrscheinlichkeitspapier") +
  xlab("Theoretische Quantile Q(i/n) Normalverteilung") +
  ylab("Beobachtete Quantile") +
  theme_bw() +    
  theme(axis.title = element_text(size=20, color = "black"), 
        axis.text = element_text(size = 15, color = "black"))


for (q in theoretical_quantiles) {
  prob_paper <- prob_paper + geom_vline(xintercept = q, linetype = "dashed", color = "blue")
}  

print(prob_paper)
#ggsave(filename = tex_figures_path("probability_paper.png"), plot = prob_paper)

