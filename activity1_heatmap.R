# Activity 1: Heatmap - Student Performance Analysis

df <- data.frame(
  Mathematics = c(92,76,81,65,90,58,84,71,95,79),
  Physics     = c(88,72,78,70,94,62,86,75,91,81),
  Chemistry   = c(84,69,83,68,91,60,82,73,93,77),
  Programming = c(95,80,85,72,96,65,88,77,98,83),
  English     = c(81,74,79,75,89,70,80,72,94,78)
)
rownames(df) <- paste0("S", 1:10)
mat <- as.matrix(df)

# --- Calculations ---
student_avg <- rowMeans(mat)
subject_avg <- colMeans(mat)

top_student <- names(which.max(student_avg))
top_student_score <- round(max(student_avg), 2)

top_subject <- names(which.max(subject_avg))
top_subject_score <- round(max(subject_avg), 2)

support_needed <- names(student_avg[student_avg < 70])
if (length(support_needed) == 0) support_needed <- "None (all averages >= 70)"
support_needed_txt <- paste(support_needed, collapse = ", ")

answers <- c(
  "Q1. Heatmap constructed (see plot above).",
  paste0("Q2. Highest overall performer: ", top_student, " (avg = ", top_student_score, ")"),
  paste0("Q3. Highest average subject: ", top_subject, " (avg = ", top_subject_score, ")"),
  paste0("Q4. Students needing support (avg < 70): ", support_needed_txt),
  "Q5. Heatmap advantage: Encodes magnitude as color, allowing instant visual",
  "     comparison across many students and subjects simultaneously, quickly",
  "     revealing patterns, clusters, and outliers that raw tables hide."
)

# --- Plot: Heatmap on top, answers panel below ---
png("activity1_heatmap.png", width = 1100, height = 1300, res = 130)
layout(matrix(c(1,2), nrow = 2), heights = c(2.2, 1.3))

par(mar = c(5, 5, 4, 6))
n_row <- nrow(mat); n_col <- ncol(mat)
image(1:n_col, 1:n_row, t(mat[n_row:1, ]), axes = FALSE,
      col = colorRampPalette(c("#2c7bb6", "#ffffbf", "#d7191c"))(100),
      xlab = "", ylab = "", main = "Activity 1: Student Performance Heatmap")
axis(1, at = 1:n_col, labels = colnames(mat), las = 2, cex.axis = 0.9)
axis(2, at = 1:n_row, labels = rev(rownames(mat)), las = 1, cex.axis = 0.9)
for (i in 1:n_row) {
  for (j in 1:n_col) {
    text(j, n_row - i + 1, mat[i, j], cex = 0.75)
  }
}
image_legend_vals <- pretty(range(mat))
legend("topright", inset = c(-0.001, 0), legend = NA, bty = "n")

par(mar = c(0.5, 1, 0.5, 1))
plot.new()
text(0, 1, "Answers:", adj = c(0, 1), font = 2, cex = 1.1)
text(0, 0.85, paste(answers, collapse = "\n"), adj = c(0, 1), cex = 0.85, family = "mono")
dev.off()
cat("done\n")
