# Activity 2: Violin Plot & Boxplot - Employee Salary Analysis

df <- data.frame(
  HR        = c(4.2,4.5,4.7,4.1,4.6,4.3,4.8,4.4,4.9,4.5),
  IT        = c(6.5,7.0,7.4,6.8,8.2,7.6,8.5,7.1,8.8,7.3),
  Finance   = c(5.8,6.2,6.0,5.9,6.7,6.4,6.9,6.1,7.2,6.3),
  Marketing = c(5.1,5.3,5.5,5.4,5.8,5.6,5.9,5.2,6.0,5.4)
)

depts <- names(df)
sds <- sapply(df, sd)
medians <- sapply(df, median)

largest_var_dept <- names(which.max(sds))
largest_var_val <- round(max(sds), 3)

highest_median_dept <- names(which.max(medians))
highest_median_val <- round(max(medians), 2)

answers <- c(
  "Q1. Violin plots constructed (see left panel).",
  "Q2. Boxplots constructed (see right panel).",
  paste0("Q3. Largest variation (highest SD): ", largest_var_dept, " (SD = ", largest_var_val, ")"),
  paste0("Q4. Highest median salary: ", highest_median_dept, " (median = ", highest_median_val, " LPA)"),
  "Q5. Boxplot better shows median/quartiles/outliers concisely; violin plot",
  "     better represents the full shape and density of the distribution.",
  "     Together they complement each other for distribution analysis."
)

# simple base-R violin function using density()
draw_violin <- function(values, at, width = 0.35, col = "lightblue") {
  d <- density(values)
  d$y <- d$y / max(d$y) * width
  polygon(c(at - d$y, rev(at + d$y)), c(d$x, rev(d$x)), col = col, border = "steelblue")
}

png("activity2_violin_boxplot.png", width = 1200, height = 1300, res = 130)
layout(matrix(c(1,2,3,3), nrow = 2, byrow = TRUE), heights = c(2.2, 1.3))

# Violin plot panel
par(mar = c(4, 4, 3, 1))
plot(NULL, xlim = c(0.5, length(depts) + 0.5), ylim = range(unlist(df)) + c(-0.5, 0.5),
     xaxt = "n", xlab = "", ylab = "Salary (LPA)", main = "Violin Plot: Salary by Department")
axis(1, at = 1:length(depts), labels = depts)
palette_cols <- c("#a6cee3", "#b2df8a", "#fdbf6f", "#fb9a99")
for (i in seq_along(depts)) {
  draw_violin(df[[i]], at = i, col = palette_cols[i])
}

# Boxplot panel
par(mar = c(4, 4, 3, 1))
boxplot(df, col = palette_cols, main = "Boxplot: Salary by Department",
        ylab = "Salary (LPA)")

# Answers panel
par(mar = c(0.5, 1, 0.5, 1))
plot.new()
text(0, 1, "Answers:", adj = c(0, 1), font = 2, cex = 1.1)
text(0, 0.85, paste(answers, collapse = "\n"), adj = c(0, 1), cex = 0.85, family = "mono")
dev.off()
cat("done\n")
