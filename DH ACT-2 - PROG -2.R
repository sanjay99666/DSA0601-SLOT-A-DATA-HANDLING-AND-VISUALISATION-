library(ggplot2)

salary <- data.frame(
Package = c(3.2,4.5,5.0,6.8,7.2,
            8.5,4.8,5.5,6.0,7.8,
            9.2,10.5)
)

ggplot(salary,
       aes(x = Package)) +

geom_histogram(
binwidth = 1,
fill = "steelblue",
color = "black"
) +

geom_density(
aes(y = after_stat(count)),
color = "red",
linewidth = 1.2
) +

labs(
title="Distribution of Salary Packages",
x="Salary Package (LPA)",
y="Frequency"
) +

theme_minimal(base_size = 14)