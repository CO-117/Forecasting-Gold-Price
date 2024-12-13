library(readxl)
library(forecast)
library(Metrics)
library(ggplot2)
library(caret)

# Load the data
gold_data 

# Ensure Date column is in Date format
gold_data$Date <- as.Date(gold_data$Date)

# Split the data into training (2010-2020) and testing (2021-2022) sets
train_data <- subset(gold_data, Date < as.Date("2021-01-01"))
test_data <- subset(gold_data, Date >= as.Date("2021-01-01"))

# Convert Date to numeric for the regression model
train_data$Date_numeric <- as.numeric(train_data$Date)
test_data$Date_numeric <- as.numeric(test_data$Date)

# Simple Linear Regression
lm_model <- lm(Price ~ Date_numeric, data=train_data)
lm_forecast <- predict(lm_model, newdata=test_data)
lm_rmse <- rmse(test_data$Price, lm_forecast)
lm_mape <- mape(test_data$Price, lm_forecast)
lm_mse <- mse(test_data$Price, lm_forecast)
pred_linear <- lm_forecast

# Multiple Linear Regression (assuming you have additional predictors)
multi_lm_model <- lm(Price ~ Inflation + Interest_Rate + Crude_Oil, data=train_data)
multi_lm_forecast <- predict(multi_lm_model, newdata=test_data)
multi_lm_rmse <- rmse(test_data$Price, multi_lm_forecast)
multi_lm_mape <- mape(test_data$Price, multi_lm_forecast)
multi_lm_mse <- mse(test_data$Price, multi_lm_forecast)
summary(multi_lm_model)

# ARIMA(1,1,1) model
arima_model <- Arima(train_ts, order=c(1,1,1), include.drift = TRUE)
arima_forecast <- forecast(arima_model, h=nrow(test_data))$mean
arima_rmse <- rmse(test_data$Price, arima_forecast)
arima_mape <- mape(test_data$Price, arima_forecast)
arima_mse <- mse(test_data$Price, arima_forecast)
pred_arima <- arima_forecast

# Least Squares Method
ls_model <- lm(Price ~ poly(Date_numeric, 2), data=train_data)
ls_forecast <- predict(ls_model, newdata=test_data)
ls_rmse <- rmse(test_data$Price, ls_forecast)
ls_mape <- mape(test_data$Price, ls_forecast)
ls_mse <- mse(test_data$Price, ls_forecast)
pred_ls <- ls_forecast

# Create a table of results
results <- data.frame(
  Technique = c('Simple Linear Regression', 'Multiple Linear Regression', 'ARIMA', 'Least Squared Method'),
  RMSE = c(lm_rmse, multi_lm_rmse, arima_rmse, ls_rmse),
  MAPE = c(lm_mape, multi_lm_mape, arima_mape, ls_mape),
  MSE = c(lm_mse, multi_lm_mse, arima_mse, ls_mse)
)

print(results)


# Create the plot
plot <- ggplot() +
  geom_line(data = train_data, aes(x = Date, y = Price), color = "blue") +
  geom_line(data = test_data, aes(x = Date, y = Price), color = "red") +
  geom_line(data = test_data, aes(x = Date, y = pred_linear, color = "Simple Linear")) +
  geom_line(data = test_data, aes(x = Date, y = multi_lm_forecast, color = "Multiple Linear")) +
  geom_line(data = test_data, aes(x = Date, y = pred_arima, color = "Arima")) +
  geom_line(data = test_data, aes(x = Date, y = pred_ls, color = "Least Squared")) +
  labs(title = "Price Forecasting with Various Methods", x = "Date", y = "Price") +
  scale_color_manual(name = "Method",    # Legend title
                     values = c("Train Data" = "blue", "Test Data" = "red", "Simple Linear" = "brown", "Multiple Linear" = "orange", "Arima" = "purple", "Least Squared" = "green"))

# Print the plot with legend
print(plot)


# Load necessary libraries
library(dplyr)

# Filter test_data for December
test_data_december <- test_data %>% filter(format(Date, "%Y-%m") == "2022-12")

# Ensure predictions are aligned with test_data_december
pred_linear_december <- pred_linear[format(test_data$Date, "%Y-%m") == "2022-12"]
pred_multi_december <- multi_lm_forecast[format(test_data$Date, "%Y-%m") == "2022-12"]
pred_arima_december <- pred_arima[format(test_data$Date, "%Y-%m") == "2022-12"]
pred_ls_december <- pred_ls[format(test_data$Date, "%Y-%m") == "2022-12"]

# Combine actual and predicted values into a data frame
results_december <- data.frame(
  Date = test_data_december$Date,
  Actual_Price = test_data_december$Price,
  Simple_Linear = pred_linear_december,
  Multiple_Linear = pred_multi_december,
  ARIMA = pred_arima_december,
  Least_Squared = pred_ls_december
)

# Print the table
print(results_december)

