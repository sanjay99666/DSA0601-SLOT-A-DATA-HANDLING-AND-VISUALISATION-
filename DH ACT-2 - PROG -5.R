library(ggplot2)

energy <- data.frame(

Month=factor(c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec"),

levels=c("Jan","Feb","Mar","Apr","May","Jun",
         "Jul","Aug","Sep","Oct","Nov","Dec")),

Generation=c(420,450,490,530,560,600,
             580,590,610,630,590,540)
)

ggplot(energy,
       aes(Month,
           Generation,
           group=1)) +

geom_line(
color="darkgreen",
linewidth=1.3
)+

geom_point(
size=4,
color="orange"
)+

geom_text(
aes(label=Generation),
vjust=-0.8,
size=3.5
)+

labs(

title="Monthly Solar Energy Generation (2025)",

x="Month",

y="Energy Generated (MWh)"

)+

theme_minimal(base_size=14)