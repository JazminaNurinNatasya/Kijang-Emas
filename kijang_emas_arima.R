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
autoplot(emas)+labs(title = "Time Series Plot of Kijang Emas Jan 2010 - April 2023", 
                    x = "Date", y = "Kijang Emas Price (1oz)")

#---------------- Splitting Data ----------------------------#
## 70/30 ##
#Determine the split index based on 70% of the data
n_train1 <- as.integer(nrow(emas) * 0.70)  #70% for training

#Split the data based on the calculated index
train_data1 <- emas[1:n_train1, ]  #First 70% of the data for training
test_data1 <- emas[(n_train1 + 1):nrow(emas), ]  #Last 30% for testing

#Output the lengths to verify the split
cat("Training Data Length:", nrow(train_data1), "\n")
cat("Testing Data Length:", nrow(test_data1), "\n")

## 75/25 ##
#Determine the split index based on 75% of the data
n_train2 <- as.integer(nrow(emas) * 0.75)  #70% for training

#Split the data based on the calculated index
train_data2 <- emas[1:n_train2, ]  #First 75% of the data for training
test_data2 <- emas[(n_train2 + 1):nrow(emas), ]  #Last 25% for testing

#Output the lengths to verify the split
cat("Training Data Length:", nrow(train_data2), "\n")
cat("Testing Data Length:", nrow(test_data2), "\n")

## 80/20 ##
#Determine the split index based on 80% of the data
n_train3 <- as.integer(nrow(emas) * 0.80)  #70% for training

#Split the data based on the calculated index
train_data3 <- emas[1:n_train3, ]  #First 80% of the data for training
test_data3 <- emas[(n_train3 + 1):nrow(emas), ]  #Last 20% for testing

#Output the lengths to verify the split
cat("Training Data Length:", nrow(train_data3), "\n")
cat("Testing Data Length:", nrow(test_data3), "\n")

#Optional: Plot to visualize the split (change the train and test)
library(ggplot2)
ggplot() +
  geom_line(data = train_data3, aes(x = Date, y = oz), color = 'blue', size = 1) +
  geom_line(data = test_data3, aes(x = Date, y = oz), color = 'red', size = 1) +
  labs(title = "Kijang Emas (Train vs Test Data)",
       x = "Date", y = "Kijang Emas (oz)") +
  scale_color_manual(values = c("Training" = "blue", "Testing" = "red")) +
  theme_minimal()

#------------------------ Stationary Test ----------------------#
#stationarity test
adf.test((train_data3$oz), alternative="stationary", k=0)
kpss.test(train_data3$oz)

#Differencing if Non-Stationary
diff_data1 <- diff(log(train_data1$oz))
diff_data2 <- diff(log(train_data2$oz))
diff_data3 <- diff(log(train_data3$oz))
adf.test(diff_data1)  # Check stationarity again
adf.test(diff_data2)  # Check stationarity again
adf.test(diff_data3)  # Check stationarity again

#log returns plot
plot(diff_data1, type='l', main='log returns plot 70:30')
plot(diff_data2, type='l', main='log returns plot 72:25')
plot(diff_data3, type='l', main='log returns plot 80:20')

#----------------------- Model Selection ----------------------#
library(forecast)
#Model Selection with AutoARIMA
model1 <- auto.arima((train_data1$oz), trace=TRUE)
#Model Selection with AutoARIMA
model2 <- auto.arima((train_data2$oz), trace=TRUE)
#Model Selection with AutoARIMA
model3 <- auto.arima((train_data3$oz), trace=TRUE)

# Print model summary
summary(model1)  #check AIC,BIC
summary(model2)  #check AIC,BIC
summary(model3)  #check AIC,BIC

#----------------------- Model Prediction --------------------#
# Forecasting
forecasted_values <- forecast(model1, h = 10)
print(forecasted_values)
plot(forecasted_values, main = "ARIMA Forecast for Kijang Emas")

# Evaluation
mse <- mean((forecasted_values$Point_Forecast - test_data)^2)
rmse <- sqrt(mse)

# Extract predicted values
predicted_values <- as.numeric(forecasted_values)

# Create a comparison DataFrame
comparison_df <- data.frame(
  Date = test_data$Date,
  `Actual Price` = test_data$oz,
  `Predicted Price` = forecasted_values)

# Display comparison
print(head(comparison_df, 10))

# Plot actual vs predicted values
plot(test_data$Date, test_data$oz, type = "l", col = "blue", lwd = 2, xlab = "Date", ylab = "Price", main = "Actual vs Predicted Prices (ARIMA)")
lines(test_data$Date, forecasted_values, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)

# Calculate evaluation metrics
mse <- mean((test_data$Price - predicted_values)^2)
mae <- mean(abs(test_data$Price - predicted_values))
rmse <- sqrt(mse)

cat("MSE:", mse, "\nMAE:", mae, "\nRMSE:", rmse, "\n")
