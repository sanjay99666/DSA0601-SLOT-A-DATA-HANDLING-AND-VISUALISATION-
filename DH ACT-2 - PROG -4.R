library(ggplot2)

data <- data.frame(

Advertising = c(5,8,10,12,15,18,20,22,25,28),

Sales = c(42,55,63,70,82,91,98,110,120,132)

)

ggplot(data,
       aes(Advertising,
           Sales)) +

geom_point(
size=4,
color="blue"
) +

geom_smooth(
method="lm",
color="red",
linewidth=1.2,
se=FALSE
)+

labs(

title="Advertising Cost vs Sales Revenue",

x="Advertising Cost (₹ Lakhs)",

y="Sales Revenue (₹ Lakhs)"

)+

theme_minimal(base_size=14)