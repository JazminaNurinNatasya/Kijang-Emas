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
autoplot(emas)+labs(title = "Plot Siri Masa Kijang Emas Jan 2010 - April 2023", 
                    x = "Tarikh", y = "Harga Kijang Emas (1oz)")

#----------------------------- Splitting Data --------------------------------#
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
n_train3 <- as.integer(nrow(emas) * 0.80)  #80% for training

#Split the data based on the calculated index
train_data3 <- emas[1:n_train3, ]  #First 80% of the data for training
test_data3 <- emas[(n_train3 + 1):nrow(emas), ]  #Last 20% for testing

#Output the lengths to verify the split
cat("Training Data Length:", nrow(train_data3), "\n")
cat("Testing Data Length:", nrow(test_data3), "\n")

#Optional: Plot to visualize the split (change the train and test)
library(ggplot2)
ggplot() +
  geom_line(data = train_data2, aes(x = Date, y = oz), color = 'blue', size = 1) +
  geom_line(data = test_data2, aes(x = Date, y = oz), color = 'red', size = 1) +
  labs(title = "Kijang Emas (Data Latihan vs Data Ujian)",
       x = "Tarikh", y = "Harga Kijang Emas (1oz)") +
  scale_color_manual(values = c("Training" = "blue", "Testing" = "red")) +
  theme_minimal()

#--------------------------- Stationary Test ----------------------------------#
layout(matrix(1:2,ncol=2))
acf(train_data3$oz)
pacf(train_data3$oz)

#stationarity test
adf.test((train_data3$oz), alternative="stationary", k=0)
kpss.test(train_data1$oz)

#Differencing if Non-Stationary
diff_data1 <- diff(log(train_data1$oz))
diff_data2 <- diff(log(train_data2$oz))
diff_data3 <- diff(log(train_data3$oz))
adf.test(diff_data1)  # Check stationarity again
adf.test(diff_data2)  # Check stationarity again
adf.test(diff_data3)  # Check stationarity again

#log returns plot
layout(matrix(1:1,ncol=2))
plot(diff_data1, type='l', main='log returns plot 70:30')
plot(diff_data2, type='l', main='log returns plot 72:25')
plot(diff_data3, type='l', main='log returns plot 80:20')

#check ACF & PACF plot
layout(matrix(1:2,ncol=2))
acf(diff_data3)
pacf(diff_data3)

#----------------------- Model Selection ---------------------------------#
library(forecast)
#Model Selection with AutoARIMA
model1 <- auto.arima((train_data1$oz), trace=TRUE)
fit1 <- arima((train_data1$oz), c(0,1,0))
#Model Selection with AutoARIMA
model2 <- auto.arima((train_data2$oz), trace=TRUE)
fit2 <- arima((train_data2$oz), c(2,1,1))
#Model Selection with AutoARIMA
model3 <- auto.arima((train_data3$oz), trace=TRUE)
fit3 <- auto.arima((train_data3$oz))

# Print model summary
summary(model1)  #check AIC,BIC
AIC(fit1)
BIC(fit1)
summary(model2)  #check AIC,BIC
AIC(fit2)
BIC(fit2)
summary(model3)  #check AIC,BIC
AIC(fit3)
BIC(fit3)

#-------------------------- Residual Checking -------------------------------#
#residual
checkresiduals(model3)
# Perform Ljung-Box test on residuals
Box.test(residuals(model3), lag = 10, type = "Ljung-Box")

#-------------------------- Model Testing ----------------------------------#
# Prediction
# Forecast on test data (using the length of test data)
test_forecast <- forecast(model3, h = length(test_data3$oz))

# Extract actual and predicted values
str(test_data3)
actual_values <- as.numeric(test_data3$oz)
class(actual_values)  
head(actual_values)   
predicted_values <- as.numeric(test_forecast$mean)
head(predicted_values)
tail(predicted_values)

# Create a data frame for visualization and analysis
results_df <- data.frame(
  Date = seq(as.Date("2023-01-01"), by = "day", length.out = length(test_data)),
  Actual = actual_values,
  Predicted = predicted_values
)

# Visualization: Actual vs Predicted
ggplot(results_df, aes(x = test_data3$Date)) +
  geom_line(aes(y = Actual, color = "Nilai Sebenar")) +
  geom_line(aes(y = Predicted, color = "Nilai Ramalan"), linetype = "dashed") +
  labs(title = "Nilai Sebenar vs Nilai Ramalan", y = "Harga Kijang Emas", x = "Masa") +
  scale_color_manual(values = c("Nilai Sebenar" = "blue", "Nilai Ramalan" = "red")) +
  theme_minimal()

#-------------------------- Correlation Plot -----------------------------#
# Compute the correlation between actual and predicted values
correlation <- cor(results_df$Actual, results_df$Predicted)
# Print the correlation
cat("Correlation coefficient (Actual vs Predicted):", correlation, "\n")

ggplot(data = NULL, aes(x = actual_values, y = predicted_values)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add regression line
  labs(
    title = "Plot Korelasi ARIMA : Sebenar vs Ramalan",
    x = "Harga Sebenar",
    y = "Harga Ramalan") + annotate("text",
    x = max(actual_values) * 0.8,
    y = min(predicted_values) * 1.2,
    label = paste("Correlation:", round(correlation, 4)),
    color = "darkred") + theme_minimal()

#------------------------- Model Evaluation -----------------------------#
# Calculate evaluation metrics
mse <- mean((test_data3$oz - predicted_values)^2)
mae <- mean(abs(test_data3$oz - predicted_values))
mape <- mean(abs(test_data3$oz - predicted_values))/100
rmse <- sqrt(mse)

cat("MSE:", mse, "\nMAE:", mae, "\nMAPE:", mape, "\nRMSE:", rmse, "\n")
