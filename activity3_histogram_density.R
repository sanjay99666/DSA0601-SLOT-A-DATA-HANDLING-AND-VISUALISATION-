# Activity 3: Histogram & Density Plot - Website Response Time

x <- c(210,225,230,240,250,260,270,280,285,290,300,305,310,315,320,330,340,350,360,380)

# skewness (Fisher-Pearson coefficient, sample)
skewness <- function(v) {
  n <- length(v)
  m <- mean(v)
  s <- sd(v)
  (sum((v - m)^3) / n) / (s^3)
}
skew_val <- round(skewness(x), 3)
skew_desc <- if (skew_val > 0.1) "right (positively) skewed" else if (skew_val < -0.1) "left (negatively) skewed" else "approximately symmetric"

mean_x <- round(mean(x), 1)
median_x <- round(median(x), 1)
sd_x <- round(sd(x), 1)
common_low <- round(mean_x - sd_x)
common_high <- round(mean_x + sd_x)

answers <- c(
  "Q1. Histogram constructed (see left panel).",
  "Q2. Density plot constructed (see right panel).",
  paste0("Q3. Skewness = ", skew_val, " -> distribution is ", skew_desc, "."),
  paste0("     (mean = ", mean_x, " ms, median = ", median_x, " ms)"),
  paste0("Q4. Common range (mean +/- 1 SD): approx ", common_low, "-", common_high, " ms"),
  "Q5. The density plot better represents the overall distribution shape since",
  "     it is smooth and not sensitive to bin-width choice, unlike the histogram."
)

png("activity3_histogram_density.png", width = 1200, height = 1300, res = 130)
layout(matrix(c(1,2,3,3), nrow = 2, byrow = TRUE), heights = c(2.2, 1.3))

par(mar = c(4, 4, 3, 1))
hist(x, breaks = 6, col = "#8dd3c7", border = "white",
     main = "Histogram: Website Response Time", xlab = "Response Time (ms)")

par(mar = c(4, 4, 3, 1))
d <- density(x)
plot(d, main = "Density Plot: Website Response Time", xlab = "Response Time (ms)",
     col = "#377eb8", lwd = 2)
polygon(d, col = adjustcolor("#377eb8", alpha.f = 0.3), border = NA)
abline(v = mean_x, col = "red", lty = 2)
abline(v = median_x, col = "darkgreen", lty = 3)
legend("topright", legend = c("mean", "median"), col = c("red", "darkgreen"), lty = c(2,3), cex = 0.8)

par(mar = c(0.5, 1, 0.5, 1))
plot.new()
text(0, 1, "Answers:", adj = c(0, 1), font = 2, cex = 1.1)
text(0, 0.85, paste(answers, collapse = "\n"), adj = c(0, 1), cex = 0.85, family = "mono")
dev.off()
cat("done\n")
