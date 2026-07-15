# Activity 5: Histogram, Density & Boxplot - Daily Steps

df <- data.frame(
  Age_20_30 = c(9500,10200,9800,11000,10500,9700,10100,10800,11200,9900),
  Age_31_45 = c(8200,8600,8400,9000,8900,8500,8700,9100,9200,8600),
  Age_46_60 = c(6500,6700,6900,7200,7000,6800,7100,7300,7400,6950)
)
groups <- c("20-30", "31-45", "46-60")
names(df) <- groups

medians <- sapply(df, median)
highest_median_group <- names(which.max(medians))
highest_median_val <- round(max(medians), 0)

answers <- c(
  "Q1. Histograms drawn (top row).",
  "Q2. Density curves plotted (top row, overlaid).",
  "Q3. Boxplots created (bottom-left panel).",
  paste0("Q4. Highest median group: ", highest_median_group, " (median = ", highest_median_val, " steps)"),
  "Q5. Boxplot is the best overall choice here: it directly compares medians,",
  "     quartiles and outliers across the three age groups in one compact view,",
  "     which is exactly what the question set (group comparison) requires.",
  "     Histogram/density are useful for shape but harder to compare 3 groups at once."
)

png("activity5_hist_density_box.png", width = 1300, height = 1400, res = 130)
layout(matrix(c(1,2,3,4,5,5), nrow = 2, byrow = TRUE), heights = c(1.6, 1.6), widths = c(1,1,1))

cols <- c("#66c2a5", "#fc8d62", "#8da0cb")

# Histogram + density overlay for each group
for (i in seq_along(groups)) {
  par(mar = c(4, 4, 3, 1))
  v <- df[[i]]
  hist(v, breaks = 5, freq = FALSE, col = cols[i], border = "white",
       main = paste0("Steps: Age ", groups[i]), xlab = "Daily Steps")
  lines(density(v), col = "black", lwd = 2)
}

# Boxplot comparing all groups
par(mar = c(4, 4, 3, 1))
boxplot(df, col = cols, main = "Boxplot: Daily Steps by Age Group", ylab = "Daily Steps")

# Answers panel
par(mar = c(0.5, 1, 0.5, 1))
plot.new()
text(0, 1, "Answers:", adj = c(0, 1), font = 2, cex = 1.05)
text(0, 0.88, paste(answers, collapse = "\n"), adj = c(0, 1), cex = 0.8, family = "mono")
dev.off()
cat("done\n")
