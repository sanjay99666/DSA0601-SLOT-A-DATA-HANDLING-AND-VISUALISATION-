library(ggplot2)

budget <- data.frame(

Channel = c("Social Media",
            "Television",
            "Print Media",
            "Radio",
            "Email Marketing",
            "Influencer Marketing",
            "SEO",
            "Events"),

Percent = c(28,22,10,8,12,9,6,5)
)

ggplot(budget,
       aes(x="",
           y=Percent,
           fill=Channel)) +

geom_col(width=1,
         color="white") +

coord_polar("y") +

geom_text(
aes(label=paste0(Percent,"%")),
position=position_stack(vjust=0.5),
size=4
)+

labs(
title="Marketing Budget Allocation"
)+

theme_void() +

theme(
plot.title=element_text(face="bold",
hjust=0.5)
)