library(ggplot2)

sales <- data.frame(
  Category = c("Groceries","Dairy","Fruits","Vegetables",
               "Bakery","Beverages","Snacks",
               "Personal Care","Household Items",
               "Frozen Foods"),

  Sales = c(120,95,80,75,68,90,110,55,70,60)
)

ggplot(sales,
       aes(x = reorder(Category, Sales),
           y = Sales,
           fill = Sales)) +

geom_col(width = 0.75,
         color = "black") +

coord_flip() +

geom_text(aes(label = Sales),
          hjust = -0.2,
          size = 4) +

scale_fill_gradient(low = "skyblue",
                    high = "darkblue") +

labs(
title="Monthly Sales Revenue by Product Category (June 2026)",
x="Product Category",
y="Sales (₹ Lakhs)"
) +

theme_minimal(base_size = 14) +

theme(
plot.title = element_text(face="bold",hjust=0.5),
legend.position="none"
)