library(fpp3)
library(ggplot2)
library(tseries)
library(tidyverse)

#read data
dt <- read.csv(file.choose(), header=T)
head(dt)

#create tsibble
emas <- dt %>% mutate(Date=as_date(Date)) %>%
  as_tsibble(index=Date) %>% select(Date,oz) 
head(emas)

#plot time series
ggplot(emas, aes(Date, oz)) + geom_line() +labs(title = "Time Series Plot of Gold Price Jan 2010 - April 2023", 
                                                x = "Date", y = "Gold Price")
autoplot(emas)+labs(title = "Time Series Plot of Kijang Emas Jan 2010 - April 2023", 
                    x = "Date", y = "Kijang Emas Price (1oz)")
########
# Check the structure of the tsibble
glimpse(emas)

#Determine the split index based on 70% of the data
n_train <- as.integer(nrow(emas) * 0.75)  #70% for training

#Split the data based on the calculated index
train_data <- emas[1:n_train, ]  #First 70% of the data for training
test_data <- emas[(n_train + 1):nrow(emas), ]  #Last 30% for testing

#Output the lengths to verify the split
cat("Training Data Length:", nrow(train_data), "\n")
cat("Testing Data Length:", nrow(test_data), "\n")

#Check the first few rows of training and testing sets
head(train_data)
head(test_data)

# Optional: Plot to visualize the split
library(ggplot2)
ggplot() +
  geom_line(data = train_data, aes(x = Date, y = oz), color = 'blue', size = 1) +
  geom_line(data = test_data, aes(x = Date, y = oz), color = 'red', size = 1) +
  labs(title = "Kijang Emas (Train vs Test Data)",
       x = "Date", y = "Kijang Emas (oz)") +
  scale_color_manual(values = c("Training" = "blue", "Testing" = "red")) +
  theme_minimal()

#log returns plot
plot(diff(log(train_data$oz)),type='l', main='log returns plot')

# Check for stationarity and difference the series if needed
library(tseries)
adf.test(diff(log(train_data$oz)), alternative="stationary", k=0)

# Train the ARIMA model using auto.arima
library(forecast)
fitARIMA <- auto.arima(diff(log(train_data$oz)), trace=TRUE)

# Print model summary
summary(fitARIMA)

# Forecasting on the test set
forecast_length <- length(test_data$oz)
arima_forecast <- forecast(fitARIMA, h = forecast_length)

# Extract predicted values
predicted_values <- as.numeric(arima_forecast$mean)

# Create a comparison DataFrame
comparison_df <- data.frame(
  Date = test_data$Date,
  `Actual Price` = test_data$oz,
  `Predicted Price` = predicted_values
)

# Display comparison
print(head(comparison_df, 10))

# Plot actual vs predicted values
plot(test_data$Date, test_data$oz, type = "l", col = "blue", lwd = 2, xlab = "Date", ylab = "Price", main = "Actual vs Predicted Prices (ARIMA)")
lines(test_data$Date, predicted_values, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)

# Calculate evaluation metrics
mse <- mean((test_data$Price - predicted_values)^2)
mae <- mean(abs(test_data$Price - predicted_values))
rmse <- sqrt(mse)

cat("MSE:", mse, "\nMAE:", mae, "\nRMSE:", rmse, "\n")
