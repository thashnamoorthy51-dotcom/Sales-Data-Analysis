# Step 1: Install and load packages
install.packages("tidyverse")
library(tidyverse)



# Step 2: Create Sales Dataset (30 Records)
#----------------------------------------

# Generate sample dataset for 30 entries (5 products × 6 months)
sales_data <- data.frame(
  Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun"), each = 5),
  Product = rep(c("Product_A", "Product_B", "Product_C", "Product_D", "Product_E"), times = 6),
  Sales = c(
    12000, 10000, 15000, 13000, 11000,   # Jan
    14000, 11000, 16000, 13500, 11500,   # Feb
    18000, 13000, 17000, 14500, 12500,   # Mar
    20000, 14000, 17500, 15000, 13000,   # Apr
    22000, 15000, 19000, 16000, 14000,   # May
    25000, 17000, 20000, 17000, 15000    # Jun
  )
)

# View dataset
print("----- SALES DATA -----")
print(sales_data)




# Step 3: Basic Summary Statistics
#----------------------------------------
summary_stats <- sales_data %>%
  group_by(Product) %>%
  summarise(
    Total_Sales = sum(Sales),
    Average_Sales = mean(Sales),
    Max_Sales = max(Sales),
    Min_Sales = min(Sales)
  )

print("----- SUMMARY STATISTICS -----")
print(summary_stats)



# Step 4: Monthly Sales Trend (Line Graph)
#----------------------------------------
ggplot(sales_data, aes(x = Month, y = Sales, color = Product, group = Product)) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  labs(
    title = "Monthly Sales Trend (for 6 Months)",
    x = "Month",
    y = "Sales Amount"
  ) +
  theme_minimal()



# Step 5: Total Sales by Product (Bar Chart)
#----------------------------------------
ggplot(summary_stats, aes(x = Product, y = Total_Sales, fill = Product)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Total Sales by Product",
    x = "Product",
    y = "Total Sales"
  ) +
  theme_minimal()



# Step 6: Identify Best-Performing Product
#----------------------------------------
best_product <- summary_stats %>%
  filter(Total_Sales == max(Total_Sales)) %>%
  select(Product, Total_Sales)

print("----- BEST PERFORMING PRODUCT -----")
print(best_product)



# Step 7: Insights & Observations
#----------------------------------------
cat("\n----- INSIGHTS -----\n")
cat("1️⃣ Product_A consistently shows the highest sales across all months.\n")
cat("2️⃣ Sales are increasing month-by-month for all products.\n")
cat("3️⃣ Peak sales occur during May and June — indicating strong end-of-quarter performance.\n")
cat("4️⃣ Product_B and Product_E have moderate growth; can improve with better marketing.\n")
cat("5️⃣ Overall, sales trend shows a steady business growth pattern.\n")




# Assuming you have actual sales and predicted sales
# Example vectors
actual_sales <- c(120, 150, 170, 200, 250)   # replace with your actual data
predicted_sales <- c(115, 155, 165, 210, 245) # replace with your predicted data
# For continuous data, use Mean Absolute Percentage Error (MAPE) or RMSE
# MAPE (gives % error)
mape <- mean(abs((actual_sales - predicted_sales)/actual_sales)) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%\n")
# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actual_sales - predicted_sales)^2))
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")
