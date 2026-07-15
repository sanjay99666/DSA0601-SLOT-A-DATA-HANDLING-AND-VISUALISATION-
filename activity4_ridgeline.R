# Activity 4: Ridgeline Plot - Electricity Consumption

df <- data.frame(
  Jan = c(220,240,230,215,225,245,235,228,238,232),
  Feb = c(235,245,238,228,240,250,242,236,246,239),
  Mar = c(250,260,255,248,258,265,259,252,262,256),
  Apr = c(275,285,280,270,282,290,284,278,287,281),
  May = c(295,305,300,292,298,310,302,297,307,301)
)
months <- names(df)

month_avg <- sapply(df, mean)
month_range <- sapply(df, function(v) max(v) - min(v))

highest_avg_month <- names(which.max(month_avg))
highest_avg_val <- round(max(month_avg), 1)

widest_spread_month <- names(which.max(month_range))
widest_spread_val <- round(max(month_range), 1)

answers <- c(
  "Q1. Ridgeline plot constructed (see plot above).",
  paste0("Q2. Highest average month: ", highest_avg_month, " (avg = ", highest_avg_val, " units)"),
  paste0("Q3. Widest spread: ", widest_spread_month, " (range = ", widest_spread_val, " units)"),
  "Q4. Trend: Electricity consumption rises steadily from January through May",
  "     across all houses, consistent with increasing summer cooling demand.",
  "Q5. Ridgeline plot is chosen because it stacks each month's distribution",
  "     with a slight vertical offset, making it easy to compare how the shape",
  "     and central tendency of consumption shifts across many groups (months)",
  "     within a single compact, uncluttered chart."
)

png("activity4_ridgeline.png", width = 1100, height = 1300, res = 130)
layout(matrix(c(1,2), nrow = 2), heights = c(2.4, 1.1))

par(mar = c(4, 6, 3, 2))
overlap <- 1.4
all_vals <- unlist(df)
xr <- range(all_vals) + c(-15, 15)
n <- length(months)

plot(NULL, xlim = xr, ylim = c(0, n * overlap + 1.2), yaxt = "n",
     xlab = "Electricity Consumption (units)", ylab = "",
     main = "Ridgeline Plot: Monthly Electricity Consumption")
cols <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"))(n)

for (i in seq_along(months)) {
  v <- df[[months[i]]]
  d <- density(v, from = xr[1], to = xr[2])
  base <- (i - 1) * overlap
  scaled_y <- d$y / max(d$y) * overlap * 0.9
  polygon(d$x, base + scaled_y, col = adjustcolor(cols[i], alpha.f = 0.75), border = cols[i])
  lines(d$x, base + scaled_y, col = cols[i])
}
axis(2, at = (seq_along(months) - 1) * overlap, labels = months, las = 1)

par(mar = c(0.5, 1, 0.5, 1))
plot.new()
text(0, 1, "Answers:", adj = c(0, 1), font = 2, cex = 1.1)
text(0, 0.85, paste(answers, collapse = "\n"), adj = c(0, 1), cex = 0.85, family = "mono")
dev.off()
cat("done\n")
