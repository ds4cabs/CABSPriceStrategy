# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)

# Example data: Historical data on attendance and pricing
# Assuming data frame 'conference_data' with columns: 'Price', 'Attendance', 'Segment' (e.g., Member, Non-Member), 'Category' (e.g., Industry, Academia)

# Sample data (replace this with your actual data)
conference_data <- data.frame(
  Price = c(60, 90, 100, 130, 40, 70, 80, 110),
  Attendance = c(200, 150, 180, 120, 220, 160, 190, 130),
  Segment = c('Member', 'Non-Member', 'Member', 'Non-Member', 'Member', 'Non-Member', 'Member', 'Non-Member'),
  Category = c('Industry', 'Industry', 'Academia', 'Academia', 'Industry', 'Industry', 'Academia', 'Academia')
)

# Exploratory Data Analysis
summary(conference_data)
ggplot(conference_data, aes(x = Price, y = Attendance, color = Segment)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ Category) +
  labs(title = "Price vs. Attendance by Segment and Category", x = "Ticket Price", y = "Attendance")

# Demand Modeling: Linear regression to estimate demand based on price
demand_model <- lm(Attendance ~ Price + Segment + Category, data = conference_data)
summary(demand_model)

# Predict attendance for different pricing scenarios
new_prices <- data.frame(
  Price = c(60, 80, 100, 120),
  Segment = c('Member', 'Member', 'Non-Member', 'Non-Member'),
  Category = c('Industry', 'Academia', 'Industry', 'Academia')
)
predicted_attendance <- predict(demand_model, newdata = new_prices)
new_prices$PredictedAttendance <- predicted_attendance

# Print predicted attendance
print(new_prices)

# Revenue Optimization
# Function to calculate revenue
calculate_revenue <- function(price, attendance) {
  return(price * attendance)
}

# Optimizing prices (simple grid search for demonstration)
price_range <- seq(50, 150, by = 10)
best_revenue <- 0
best_price <- NA

for (price in price_range) {
  new_prices <- data.frame(
    Price = rep(price, 4),
    Segment = c('Member', 'Member', 'Non-Member', 'Non-Member'),
    Category = c('Industry', 'Academia', 'Industry', 'Academia')
  )
  predicted_attendance <- predict(demand_model, newdata = new_prices)
  revenue <- sum(calculate_revenue(new_prices$Price, predicted_attendance))
  
  if (revenue > best_revenue) {
    best_revenue <- revenue
    best_price <- price
  }
}

cat("Optimal Price: ", best_price, "\n")
cat("Expected Revenue: ", best_revenue, "\n")

# Plotting the revenue optimization results
revenues <- sapply(price_range, function(price) {
  new_prices <- data.frame(
    Price = rep(price, 4),
    Segment = c('Member', 'Member', 'Non-Member', 'Non-Member'),
    Category = c('Industry', 'Academia', 'Industry', 'Academia')
  )
  predicted_attendance <- predict(demand_model, newdata = new_prices)
  sum(calculate_revenue(new_prices$Price, predicted_attendance))
})

revenue_data <- data.frame(Price = price_range, Revenue = revenues)
ggplot(revenue_data, aes(x = Price, y = Revenue)) +
  geom_line() +
  geom_point() +
  labs(title = "Revenue Optimization", x = "Ticket Price", y = "Revenue")

