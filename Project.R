library(readr)
library(forecast)
library(lmtest) 
library(DIMORA)
library(fpp2)
library(ggplot2)
library(e1071)
library(readxl)
library(tseries)
library(torch)
library(tidyverse)
library(corrplot)
library(reshape2)


data <- read_csv("C:\\Users\\User\\Desktop\\BFED_project\\BFED_project\\revenues.csv")
copperPrice <- read.csv("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\copper_prices.csv", sep = ",", dec = ",", header = TRUE)
cpi <- read_csv("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\CPI.csv")
interest_rate <- read_csv("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\interest_rate.csv")
unemployment_rate <- read.csv("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\unemployment_rate.csv", sep = ",", dec = ",", header = TRUE)
usd_try_currency <- read.csv("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\usd_try_currency.csv", sep = ",", dec = ",", header = TRUE)


# Fixing Revenue values to integer
data$Revenue <- gsub("\\.", "", data$Revenue)
data$Revenue <- gsub(",", ".", data$Revenue)
data$Revenue <- as.integer(data$Revenue)

# Check format of "Date" column
class(data$Date)

# Adjust the format based on your data
data$Date <- as.Date(paste0("01.", data$Date), format = "%d.%m.%Y")

#Drop 11.2024 because the sales data for November 2024 was incomplete at the time data was collected.
data <- subset(data, Date != "2024-11-01")

# Scale Revenue to millions
data$Revenue <- data$Revenue / 1e6

revenue <- data$Revenue
date <- data$Date
tt <- 1:NROW(data)

# Plot the data
plot(date, revenue, type = "o", col = "blue",
     xlab = "Date", ylab = "Revenue (in millions TL)", main = "Revenues Over Time")


# Transform the data into a 'ts' object
revenue_ts <- ts(revenue, start = c(2017, 1), frequency = 12)
# Seasonal plot
seasonplot(revenue_ts,
           main = "Seasonal Plot of Revenues",
           col = rainbow(12),
           year.labels = TRUE,
           year.labels.left = FALSE,
           type = "o",
           ylab = "Revenue (in millions TL)")

# Check the autocorrelation
acf(revenue)    # base r autocorrelation
Acf(revenue)    # coming from forecast package

# acf - Autocorrelation Function-  graph used to analyze the correlation of a time series 
# understanding and interpreting an acf graph is essential for identifying patterns in the data 
# such as seasonality and trends. 
# By looking acf graph of  revenue data revenue values are strongly correlated with immediate previous values
# The autocorrelation decreases gradually as the lag increases but also it remains significant (bc the bar 
# which represent autocorrelation outside the blue line ) for several lags.
# we could say this pattern is typically of non stationary time series which mayy have trends 
# or dependences over time 

# Fit linear regression model
fit1 <- lm(revenue~tt)
summary(fit1)
anova(fit1)

# Min: -10.489
# 1st Quart: -4.650
# Median: -1.281
# 3rd Quart: 2.998
# Max: 24.698
# The residuals are not perfectly centered around zero but the median which is close to zero indicates 
# that the models predictions are reasonably balanced 
# p<0.05 there is strong evidence that the tt affects revenue. 58.63% of the variablity in revenue is explained by the predictor tt


# anova provides the analysis of variance table for the linear regression model -fit1- where revenue is modeled as a function of tt. 
# For that the predictior oo explains a significant amount of the variation in revenue (p<0.001)
# the variation explained by tt 5758.1
# the variation unexplained which is called residuals 4063.4


# Plot the model
plot(tt, revenue, xlab = "Time", ylab = "Revenue (in millions TL)")
abline(fit1, col = 3)

# Check the residuals
dwtest(fit1)
resfit1 <- residuals(fit1)
plot(resfit1, xlab = "Time", ylab = "Residuals (LM)")
plot(resfit1, type = "o", col = "blue",
     xlab = "Time", ylab = "Residuals", main = "Residual Plot (LM)")
abline(h = 0, col = "red", lty = 2)

# The Durbin watson test is used to check for autocorrelation in the residuals of a regression model. 
# DW = 0.48582
# 0.48582 is very low, indicating strong positive autocorrelation in the residuals
# p-value is extremely small meaning the test strongly rejects the null hypothesis of no autocorrelation

# Fit a linear model with the tslm function
fitts<- tslm(revenue_ts~trend+season)
summary(fitts)

#  There is a strong positive trend in revenue over time, most seasonal coefficient are not statistically 
#  significant, maybe seasonality may not have a meaningful impact on revenue 
#  model explains 62.79% of the variability in revenue, indicating a moderately good fit
#  some variation remains unexplained, as indicated by the residuals standard error which is 6.717


# Plot the original time series
plot(revenue_ts, type = "o", col = "blue", 
     ylab = "Revenue", xlab = "Time", 
     main = "Observed vs Fitted Values")

# Add fitted values as a line
lines(fitted(fitts), col = "red", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

# Check the residuals again
dwtest(fitts)
resfit2 <- residuals(fitts)
plot(resfit2, type = "o", col = "blue",
     xlab = "Time", ylab = "Residuals", main = "Residual Plot (TSLM)")
abline(h = 0, col = "red", lty = 2)

# ACF of Residuals
acf(resfit2, main = "ACF of Residuals (TSLM)")

#The model might not fully capture the underlying structure of the data (e.g., trends or seasonality).
#Standard errors of the coefficients may be underestimated, leading to incorrect p-values and confidence intervals.
fore <- forecast(fitts)
plot(fore)
print(fore)


#ARIMA Models
#steps according to arima slide pg.27
#step 1: plot the data
autoplot(revenue_ts) + 
  ggtitle("Monthly Revenue Data") + 
  ylab("Revenue") + 
  xlab("Year")

# this plot shows the monthly revenue data over time. The revenue data shows a strong upwords trend 
# particularly starting around 2021. While not strongly obvious there might be seasonality in the data
# especially strating from 2022. These could correspond to seasonal effects for higher revenue during certain months 
# it is obvious that aroundlate 2023 to early 2024 there is a rapid spike in revenue, reaching a peak above 40 units.
# This could be due to market change vs vs vs -- araþtýrýlmasý yapýlacak ileride---



#step 2: checking the outliers and trying log transformations
boxplot(revenue_ts,main="Boxplot of Revenue Data", ylab="Revenue")
hist(revenue_ts)
skewness <- skewness(revenue_ts)
print(skewness) #skewness is 2.025721 we can use log transformations
log_revenue_ts = log(revenue_ts)
autoplot(log_revenue_ts) + ggtitle("Log-Transformed Monthly Revenue")+ylab("Log Revenue")+xlab("Year")

#step 3 & step 4: checking if data is stationary by examining ACF and PACF
Acf(log_revenue_ts)
Pacf(log_revenue_ts)
tsdisplay(log_revenue_ts)

#step 5: differencing is needed values are highly correlated
##first difference
diff1 = diff(log_revenue_ts)
###seasonal difference
diff12<- diff(log_revenue_ts, lag=12) 
tsdisplay(diff1)
tsdisplay(diff12)
#few value still higher correlation so taking the second difference of the first difference
diff2 = diff(diff1)
tsdisplay(diff2)
#Step 6: Fit Arima model using auto.arima
revenue_arima = auto.arima(log_revenue_ts)
summary(revenue_arima)

# The ARIMA(0,1,1)(0,1,1)[12] model is a good fit for the log-transformed data, accounting for 
# both regular and seasonal patterns. The residuals show minimal autocorrelation, indicating that 
# the model captures the structure of the series well.

#step 7: checking residuals
checkresiduals(revenue_arima)
# Ljung-Box test p-value = 0.0493 less than 0.05 there is some evidence of autocorrelation int he residuals 
# the arima model may not fully capture all the structure in the data, particularly at the higher lags 


#step 8:trying other values to see if we can achieve better model than the auto model
another_model<- Arima(log_revenue_ts, order=c(0,1,1), seasonal=c(0,1,2))
fit<- fitted(another_model)
res_another_model<- residuals(another_model)
tsdisplay(res_another_model)
checkresiduals(another_model)
summary(another_model)
#Auto model has better AIC value
AIC(revenue_arima,a4)
#step 9: forecasting with auto arima model
forecast_log <- forecast(revenue_arima, h = 12)  # Forecast next 12 months
forecast_original <- exp(forecast_log$mean)  # Back-transform forecasts to original scale

# Plot forecasts in original scale
autoplot(forecast_log) + ggtitle("Log-Transformed Revenue Forecast") + ylab("Log Revenue") + xlab("Year")
plot(forecast_original, type = "l", main = "Revenue Forecast (Original Scale)", ylab = "Revenue", xlab = "Year")


##Exponential Smoothing methods

# simple exponential methods 
autoplot(revenue_ts) + ylab("Revenue") + xlab("Year") + ggtitle("Revenue Over Time")

fit1 <- ses(revenue_ts, alpha=0.2, initial = "simple", h=5)
autoplot(fit1) + ylab("Revenue Forecast") + xlab("Time")
fit2 <- ses(revenue_ts, alpha=0.6, initial = "simple", h=5)
autoplot(fit2) + ylab("Revenue Forecast") + xlab("Time")
fit3 <- ses(revenue_ts, h=5)
autoplot(fit3) + ylab("Revenue Forecast") + xlab("Time")

# Code above ses is simple exponential smoothing model to a time series. alpha is the smoothing operator 
# we apply two different smoothing operator which are 0.2 and 0.6 Alpha could be between 0 and 1. 0.2 means more smoothing 
# h indicates the period show us 5 more month in the forecast. 

plot(revenue_ts, ylab="Revenue", xlab="Year")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
legend("topleft", 
       legend=c("Fit 1 (alpha=0.2)", "Fit 2 (alpha=0.6)", "Fit 3 (auto alpha)"), 
       col=c("blue", "red", "green"), 
       lty=1, 
       pch=1, 
       xpd=TRUE, 
       inset=c(0, 0))

fc<- ses(revenue_ts, h=5)
round(accuracy(fc), 2)
summary(fc)

autoplot(fc)+ autolayer(fitted(fc), series="Fitted")+ylab("Revenue")+xlab("Year")

# The model used is simple expponential smoothing with alpha 0.5323, training set accuracy shows that 
# the model has reasonable performance but MAPE value 24.23% indicates need to improve
# the forecast for next 5 period gives a point forecast of 40.63 for each period 

# if we look at the plot Forecast from SES by year the black line shows the actual historical data over time 
# the red line represents the fitted values from the SES model. These values estimated based on past trends 
# The blue shaded region indicates the forecasted range for the upcoming period.   


# Holt Method - Trend Methods
fc <- holt(revenue_ts, h=15)
fc2 <- holt(revenue_ts, damped = T, phi = 0.9, h=15)
autoplot(revenue_ts)+ autolayer(fc, series = "Holt's Method", PI=F) +
autolayer(fc2, series = "Damped Holt's Method", PI = F)

# The plot shows a comparison between holts method and damped holts method for the time series forecasting 
# Holts method assumes an exponential growth trend leading to a more aggressive upward forecast, which may overestimate future
# values if the trend does not sustain indefinitely. On the other  hand Damped Holts method incorporates a damping parameter that moderates the 
# growth rate of the trend, resulting in a more conservative and realistic forecast. This comparision highlights the importance of selecting an appropriate 
# forecasting method based on the expected behaviour of the data, particularly when the long term estimate is involved 


# Trend and seasonality methods (Holt Winters Method)
autoplot(revenue_ts)
fit1 <- hw(revenue_ts, seasonal = "additive")
fit2 <- hw(revenue_ts, seasonal = "multiplicative")
autoplot(revenue_ts)+ autolayer(fit2, series = "Holt-Winter's Method", PI = F)

# The plot demonstrates the application of Holt Winters Method for the time series forecasting which incorporates level, trend and seasonality components.
# The black line represents the historical data while the red line illlustrates the models forecast fpr the future periods. the method effectively captures 
# both the upwards trend and recurring seasonal variation in the data ,making it suitable for the time series with regular seasonal patterns. the forecast highlights 
# a  continuously  increasing  trend with periodic peaks and troughs, reflecting the seasonal nature of the data  

## ARIMA Models 

###The parameters of the ARIMA model are defined as follows:
#p: The number of lag observations included in the model, also called the lag order.
#d: The number of times that the raw observations are differenced, also called the degree of difference.
#q: The size of the moving average window, also called the order of moving average.

plot(revenue_ts, main = "Original Time Series" )
Acf(revenue)
Pacf(revenue)
adf.test(revenue_ts)

##  time series is non-stationary because the p-value = 0.99 is much
## greater than the significance threshold (typically 0.05). This means
## the null hypothesis of "the series is non-stationary" cannot be rejected.

revenue_diff <- diff(revenue_ts, differences = 1)
adf.test(revenue_diff)

plot(revenue_diff, main = "First Differenced Series")
acf(revenue_diff, main = "ACF of Differenced Series")
pacf(revenue_diff, main = "PACF of Differenced Series")

fit <- auto.arima(revenue_diff)
summary(fit)

model1 <- Arima(revenue_diff, order = c(1,0,0))
summary(model1)

model2 <- Arima(revenue_diff, order = c(1,0,2))
summary(model2)

revenue_diff_1 <- diff(revenue_ts, differences = 1)  # Regular differencing
revenue_diff_1_seasonal <- diff(revenue_diff_1, lag = 12)  # Seasonal differencing after regular differencing
plot(revenue_diff_1_seasonal)
adf.test(revenue_diff_1_seasonal)

model3_seasonal <- Arima(revenue_diff_1_seasonal, order = c(1,0,0), seasonal = c(1, 0, 1))
summary(model3_seasonal)

model4 <- Arima(revenue_diff_1_seasonal, order = c(1,0,0))  # Without seasonal AR component
summary(model4)

model5_seasonal <- Arima(revenue_diff_1_seasonal, order = c(1,0,0), seasonal = c(0,1,1))  # Adjusted seasonal part
summary(model5_seasonal)

checkresiduals(model5_seasonal)
#the residuals are not white noise pvalue<0,05
# This means there may still be some autocorrelation in the residuals 
# that the model has not captured, that the model not fully explained the patterns in the data.

plot(model5_seasonal$residuals)
Acf(model5_seasonal$residuals)
Pacf(model5_seasonal$residuals)

model6_seasonal <- Arima(revenue_diff_1_seasonal, order = c(1,1,0), seasonal = c(1,0,0))  
summary(model6_seasonal)
checkresiduals(model6_seasonal)

model7_seasonal <- Arima(revenue_diff_1_seasonal, order = c(1,0,0), seasonal = c(0,0,1))  
summary(model7_seasonal)
checkresiduals(model7_seasonal)
Acf(model7_seasonal$residuals)
Pacf(model7_seasonal$residuals)


AIC(model1, model2,model3_seasonal, model4, model5_seasonal,model6_seasonal,model7_seasonal) #A lower AIC value indicates a better model fit
BIC(model1, model2,model3_seasonal, model4, model5_seasonal,model6_seasonal, model7_seasonal)  #Similar to AIC but penalizes more complex models

# aýc is a measure of the relative quality of statistical models. itt balanced the good of a fit of the model with its complexity 
# Lower aýc indicate a better mdoel as it suggests a mdoel that fits the data well while keeping the number of parameters as low as possible 
# model5_seasonal is the best model in terms of AIC, suggesting that it fits the data better than the others. It has the lowest AIC and seems 
# to balance model complexity and goodness of fit effectively.

forecast_values <- forecast(model5_seasonal, h = 12)
plot(forecast_values)

#Auto Arima Model 
modelarima <- auto.arima(revenue_diff_1_seasonal)
summary(modelarima)
checkresiduals(modelarima)
forecastValues <- forecast(modelarima, h=12)
autoplot(forecastValues) + ggtitle("ARIMA Forecast") + xlab("Time") + ylab("Revenue")

### Auto arima ile aldýðým sonuç ile kendi denediðim arima modellerini karþýlaþtýrdýðýmda daha model5 de daha iyi sonuç elde
### ettim bu yðzden ikisinin karþýlaþtýrmasýný yapýyorum aþaðýda belki ekleyebiliriz ya da bize bilgi olur 

# Auto arima model function automatically identifies the best ARIMA model based on the data, adjusting for seasonal effects if needed. 
# The choosen model ARIMA(1,0,0)(0,0,1)[12]  indicate AR(1) - first order autoaggresive term, SMA(1) - a first order seasonal moving average term, and 
# seasonality of 12 period 

# On the other hand model5 manually specified ARIMA model with seasonality adjustments ARIMA(1,0,0)(0,1,1)[12]
# AR(1) - a first order autoaggresive term , SMA(1) - a first order seasonal moving average term , Seasonal differencing (d=1): A seasonal difference of 1 to account for seasonal patterns.
# also has 12 period seasonality 

# Model 5 has a lower AIC than autoarima , it is fit the data better by balancing the model complexity and goodness of a fit. 
# Both models have comparable error metrics, with model5_seasonal performing slightly better (lower MAE and MAPE), indicating a better fit on the training data.
# Best Fit (AIC and BIC): Based on the AIC and BIC values, model5 is the better model as it has a lower AIC and BIC, indicating a better balance between complexity and fit.
# Forecast Smoothing: If you prioritize smooth forecasts with less fluctuation, the Auto ARIMA model might be preferable, as it has smoother forecasts and a more automated seasonal structure.
# However, if capturing seasonal effects is critical for your problem (as seen in the model5), this model may give a more sensitive fit but with more fluctuations.

# Model5 kullanmak daha iyi gibi ama siz de bakýnýz :)))

##

ggplot(data = data, aes(as.Date(Date), revenue)) +
  xlab("Date")+
  ylab("Revenue")+
  ggtitle("Sales")+
  geom_line() 

ggplot(data, aes(x = Date, y = Revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Monthly Revenue Trend",
       x = "Date",
       y = "Revenue(TL)") +
  theme_minimal()

# Additive decomposition
decomposed_additive <- decompose(revenue_ts, type = "additive")
# Multiplicative decomposition
decomposed_multiplicative <- decompose(revenue_ts, type = "multiplicative")
# Plot additive decomposition
plot(decomposed_additive)
# Plot multiplicative decomposition
plot(decomposed_multiplicative)

# Additive Decomposition: In an additive model, the seasonal component is assumed to add 
# to the trend component at each point in time. This model is typically used when the seasonal 
# variation is constant throughout the series Additive model assumes that the seasonal variation
# is constant in magnitude and doesn't depend on the level of the time series.
# Multiplicative Decomposition: In a multiplicative model, the seasonal component multiplies
# the trend component at each time point. This model is used when the seasonal variation increases 
# or decreases proportionally to the level of the time series. Multiplicative model assumes
# that the seasonal variation is proportional to the level of the time series. 
# Trend: The long-term movement in the data.
# Seasonal: The repeating seasonal pattern in the data.
# Residual: The noise or random fluctuations left after removing the trend and seasonality.

# By running both additive and multiplicative decompositions, you can compare the two approaches
# and understand whether the seasonal effects in your data are constant or proportional to the trend.
# Depending on the nature of your data, you can then choose the appropriate decomposition model for 
# further modeling or forecasting.

# BUNU LABDA GÖRDÜM AMA NASIL YORUMLANACAK ANLAMADIM :)))

ggAcf(revenue)+
  ggtitle("Acf Function for Revenue Data")

ggPacf(revenue)+
  ggtitle("Partial Acf Function for Revenue Data")





### LAGGED REGRESSOR 
# we have a monthly data, Revenue_t1 is revenue from the previous month. Revenue_t2 is revenue from two months ago. 
# ýn this case Revenue_t1 and Revenue_t2 are lagged regressors. 


data$Revenue_t1 <- dplyr::lag(data$Revenue, 1)
data$Revenue_t2 <- dplyr::lag(data$Revenue, 2)

# create lagged variables which represent previous time steps in the data Revenuet1 is the lagged value of the revenue column by 1 period 
# revenuet2 is the lagged value of the revenue column by 2 periods 
# lagged variables helps capture the relationship between current values and previous periods which is important for time series 
# forecasting or regression modeling 

# Plot actual revenue and lagged regressors
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), size = 1) +
  geom_line(aes(y = Revenue_t1, color = "Lag 1 Month"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Revenue_t2, color = "Lag 2 Months"), size = 1, linetype = "dotted") +
  labs(title = "Revenue and Lagged Regressors",
       x = "Date",
       y = "Revenue") +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Lag 1 Month" = "red", "Lag 2 Months" = "green")) +
  theme_minimal()

# the plot indicates that the actual revenue is strongly correlated with its lagged values
# which can be useful for forecasting future revenue trends. The close alignment of the lagged
# regressors with the actual revenue suggests that past revenue data can be a good predictor of future revenue.
# The red line closely follows the blue line, indicating a strong correlation between the actual revenue and its 
# value from one month prior also has strong correlation between the actual revenue and its value from two months prior.


# split data into train and test
train_size <- as.integer(0.8 * nrow(data))  # Convert to integer
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# fit a linear model with lagged variables
model <- lm(Revenue ~ Revenue_t1 + Revenue_t2, data = train_data)
# predict future values
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((predictions - test_data$Revenue)^2))
mae <- mean(abs(predictions - test_data$Revenue))
mape <- mean(abs((predictions - test_data$Revenue) / test_data$Revenue)) * 100

metrics_table <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE"),
  Value = c(rmse, mae, mape))
print(metrics_table)

# fit a linear regression kodel with the dependent variable being revenue and the independent variables being the lagged regressors which they are revenuet1 and revenuet2
# this model assumes the past revenues helps explain the current revenue 
# RMSE: the average magnitude of error is 7.44 units of revenue with larger errors being penalized more due to the squaring of the differences.
# MAE: on average the models predictions are off by about 5.82 units of the revenue, unlike RMSE all errors are treated equally in MAE 
# MAPE: on average the models predictions are off by 22.28% of the actual revenue. This metric provides insights into the accuracy in relative terms. 
# These metrics indicates how well the model performed on the test data 
# lower values are preferred as they indicate better model accuracy 

#want to forecast the next n periods 
n <- 12
forecast_data <- tail(data, n = n)  # Get the last n rows of actual data
forecast_data$Revenue_t1 <- lag(forecast_data$Revenue, 1)
forecast_data$Revenue_t2 <- lag(forecast_data$Revenue, 2)
forecast_data <- forecast_data[complete.cases(forecast_data), ] #remove NA values (rows)

forecasted_values <- predict(model, newdata = forecast_data)
cat("Forecasted Revenue for the next", n, "months:", forecasted_values, "\n")

forecast_df <- data.frame(
  Date = seq.Date(from = tail(data$Date, 1) + 1, by = "month", length.out = n),
  Actual_Revenue = rep(NA, n),
  Forecasted_Revenue = forecasted_values)

# next 12 period we want to forecast -n-, in this model lagged variables serve as the predictior for the regression model.
# Row with missing values -NA- are removed to ensure the forecast model only uses complete data 
# 

ggplot() +
  geom_line(data = data, aes(x = Date, y = Revenue, color = "Actual"), size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecasted_Revenue, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(title = "Actual vs Forecasted Revenue", x = "Date", y = "Revenue") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_minimal()

# the forecasted revenue continues the upward trend observed in the actual revenue data
# this suggests that the forecasting model is optimistic about future revenue growth
# as we can see from the plot forecast is pretty similar with last 12 period , if this situation happens
# the model is capturing the recent trend accurately. This can be a good thing if the recent trend is expected 
# to continue however we should consider a few  external factors that might impact revenue.   



### Graph For Other Explanotary Variables ####


unemployment_rate$Date <- as.Date(unemployment_rate$Date, format = "%d.%m.%Y")
usd_try_currency$Date <- as.Date(usd_try_currency$Date, format = "%d.%m.%Y")
copperPrice$Date <- as.Date(copperPrice$Date, format = "%d.%m.%Y")
interest_rate$Date  <- as.Date(paste0(interest_rate$Date, "-01"), format = "%Y-%m-%d")
cpi$Date <- as.Date(cpi$Date, format = "%d.%m.%Y")


ggplot(unemployment_rate, aes(x = unemployment_rate$Date, y = unemployment_rate$Unemployment.Rate...)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + 
  labs(title = "Unemployment Rate", x = "Date", y = "Unemployment Rate") +
  theme_minimal()



ggplot(usd_try_currency, aes(x = usd_try_currency$Date, y = usd_try_currency$Price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + 
  labs(title = " USD Currency", x = "Date", y = "USD") +
  theme_minimal()


ggplot(interest_rate, aes(x = interest_rate$Date, y = interest_rate$`Interest Rate (%)`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + 
  labs(title = "Interest Rate", x = "Date", y = "Interest Rate (%) ") +
  theme_minimal()


ggplot(cpi, aes(x = cpi$Date, y = cpi$`CPI(Base=01.2017)`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + 
  labs(title = "CPI", x = "Date", y = "CPI ( Base 01.2017) ") +
  theme_minimal()



ggplot(copperPrice, aes(x = copperPrice$Date, y = copperPrice$Price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) + 
  labs(title = "Copper Price", x = "Date", y = "Price") +
  theme_minimal()





merged_data <- data %>%
  left_join(copperPrice, by = "Date") %>%
  left_join(cpi, by = "Date") %>%
  left_join(interest_rate, by = "Date") %>%
  left_join(unemployment_rate, by = "Date") %>%
  left_join(usd_try_currency, by = "Date")

merged_data <- merged_data %>%
  select(-Revenue_t1, -Revenue_t2)

correlation_matrix <- merged_data %>%
  select_if(is.numeric) %>% 
  cor() 

corrplot(correlation_matrix, method = "circle", type = "upper")
heatmap(correlation_matrix, main = "Correlation Between Variables", scale = "none")

#

cor_matrix <- cor(merged_data %>% select_if(is.numeric))

cor_matrix_melted <- melt(cor_matrix)

ggplot(data = cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Between Variables", x = "", y = "")





##########################################
###some simple plots
plot(revenue_ts, type="b")
plot(cumsum(revenue_ts), type="b")


###we estimate a simple Bass Model 
bm_revenue<-BM(revenue_ts,display = T)
summary(bm_revenue)
residuals(bm_revenue)
plot(residuals(bm_revenue))

####GBM with exponential shocks
GBM_mixed<- GBM(revenue_ts,shock = "exp",nshock = 1,prelimestimates = c(2.050144e+05, 3.005624e-06, 5.186099e-02,40,-0.1,0.1))
summary(GBM_mixed)
residuals(GBM_mixed)
plot(residuals(GBM_mixed))
pred_GBM_mixed<- predict(GBM_mixed, newx=c(1:94))
pred_GBM_mixed.inst<- make.instantaneous(pred_GBM_mixed)
class(revenue_ts)
pred_GBM_mixed.inst <- ts(pred_GBM_mixed.inst, start=start(revenue_ts), frequency=frequency(revenue_ts))

plot(cumsum(revenue_ts), type= "b",xlab="Months", ylab="Cumulative revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,94), ylim=c(0,1000))
lines(pred_GBM_mixed, lwd=2, col=2)

plot(revenue_ts, type= "b",xlab="Years", ylab="Revenues",  pch=16, lty=3, cex=0.6)
lines(pred_GBM_mixed.inst, col=2 , lwd=2)

#Comparing Bass Model and Generilazed Bass Model 
r_square_tilda =  (0.999186-0.995344)/(1-0.995344)#0.8251718 > 0.3 GBM is more significant



