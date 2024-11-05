library(fpp3)
library(ggplot2)
library(tseries)
library(tidyverse)

#read data
dt <- read.csv(file.choose(), header=T)
tail(dt)

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
set.seed(100)
ind <- sample(nrow(emas), size = nrow(emas)*0.70)
ind
#train
emas_train <- emas[ind, ]
#test
emas_test <- emas[-ind, ]

# Output the lengths to verify the split
cat("Training Data Length:", nrow(emas_train), "\n")
cat("Testing Data Length:", nrow(emas_test), "\n")

# Optional: Plot to visualize the split
library(ggplot2)
ggplot() +
  geom_line(data = emas_train, aes(x = Date, y = oz), color = 'blue', size = 1) +
  geom_line(data = emas_test, aes(x = Date, y = oz), color = 'red', size = 1) +
  labs(title = "Kijang Emas (Train vs Test Data)",
       x = "Date", y = "Kijang Emas (oz)") +
  scale_color_manual(values = c("Training" = "blue", "Testing" = "red")) +
  theme_minimal()

#log returns plot
plot(diff(log(emas_train$oz)),type='l', main='log returns plot')

#ADF test
library(tseries)
adf.test(diff(log(emas_train$oz)), alternative="stationary", k=0)

#check ACF & PACF plot
acf(diff(log(emas_train$oz)))
pacf(diff(log(emas_train$oz)))

#build ARIMA model
library(forecast)
fitARIMA <- auto.arima(diff(log(emas_train$oz)), trace=TRUE)
(fit <- arima(diff(log(emas_train$oz)), c(1,0,0)))
AIC(fit)
BIC(fit)

# Generate predictions on the test set
forecasted_values <- forecast(fitARIMA, h = length(emas_test))
predicted_values <- forecasted_values$mean

# Step 5: Calculate evaluation metrics
# Actual values from the test set
actual_values <- emas_test

# Convert actual_values and predicted_values to numeric vectors
actual_values2 <- as.numeric(unlist(actual_values))
predicted_values <- as.numeric(predicted_values)

# Mean Squared Error (MSE)
mse <- mean((actual_values2 - predicted_values)^2)

# Mean Absolute Error (MAE)
mae <- mean(abs(actual_values2 - predicted_values))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Display results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot(actual_values, type="l", col="blue", main="Actual vs Predicted Values", xlab="Time", ylab="Values")
lines(predicted_values, col="red", lty=2)  # Adding predicted values as a red dashed line
legend("topleft", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1:2)


# Load necessary library
library(forecast)
# Check if dates, emas_test, and predicted_values are vectors
str(dates)
str(emas_test)
str(predicted_values)

dates <- time(emas_test) # Extracts dates if emas_test is a time series with a date index.

# Convert dates to a vector, if necessary.
dates <- as.Date(dates) # Ensures dates are in Date format.
emas_test <- as.vector(as.numeric(unlist(emas_test)))
predicted_values <- as.vector(as.numeric(unlist(predicted_values)))

# Create the comparison DataFrame
comparison_df <- data.frame(
  Date = dates,
  `Actual Price` = emas_test,
  `Predicted Price` = predicted_values
)

# Display the first 10 rows
print(head(comparison_df, 10))

# Display the last 10 rows
print(tail(comparison_df, 10))


