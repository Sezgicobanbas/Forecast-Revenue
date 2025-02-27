merged <- data %>%
  left_join(copperPrice, by = "Date") %>%
  left_join(cpi, by = "Date") %>%
  left_join(interest_rate, by = "Date") %>%
  left_join(unemployment_rate, by = "Date") %>%
  left_join(usd_try_currency, by = "Date")

merged <- merged %>%
  filter(!is.na(Revenue_t1), !is.na(Revenue_t2))

LAG_MAX = 6  # Maximum lag for analysis
view(merged)
revenue_12 <- merged[81:92, "Revenue"]
copper_12 <- merged[81:92, "Price.x"]
cpi_12 <- merged[81:92, "CPI(Base=01.2017)"]
interestRate_12 <- merged[81:92, "Interest Rate (%)" ]
unemployment_12 <- merged[81:92, "Unemployment.Rate..."]
usd_12 <- merged[81:92, "Price.y"]


# Check correlation between Revenue  and predictors with lags up to LAG_MAX

ccf(revenue_12$Revenue, copper_12$Price.x, lag.max = LAG_MAX, plot = TRUE)
lag_max_copper = 3
cor(revenue_12$Revenue, copper_12$Price.x)
#0.4290
# lag is 3 bc it is the max positive value 
merged$copper_t1 <- dplyr::lag(merged$Price.x, 3)
cor(merged$Revenue[4:92], merged$copper_t1[4:92])
#0.5494

ccf(revenue_12$Revenue, cpi_12$`CPI(Base=01.2017)`, lag.max = LAG_MAX, plot = TRUE)
lag_max_cpi = 0
cor(revenue_12$Revenue, cpi_12$`CPI(Base=01.2017)`)
#0.8114
merged$cpi_t1 <- dplyr::lag(merged$`CPI(Base=01.2017)`, 0)
cor(merged$Revenue, merged$cpi_t1)
#0.9264

ccf(revenue_12$Revenue, interestRate_12$`Interest Rate (%)`, lag.max = LAG_MAX , plot=TRUE)
lag_max_interest = 3
cor(revenue_12$Revenue, interestRate_12$`Interest Rate (%)`)
#0.4422
merged$interest_t1 <- dplyr::lag(merged$`Interest Rate (%)`, 3)
cor(merged$Revenue[4:92], merged$interest_t1[4:92])
#0.8652

ccf(revenue_12$Revenue, unemployment_12$Unemployment.Rate..., lag.max = LAG_MAX , plot=TRUE)
cor(revenue_12$Revenue, unemployment_12$Unemployment.Rate...)
# -0.2893


ccf(revenue_12$Revenue, usd_12$Price.y, lag.max = LAG_MAX, plot=TRUE)
lag_max_usd = 0
cor(revenue_12$Revenue, usd_12$Price.y)
#0.7769
merged$usd_t1 <- dplyr::lag(merged$Price.y, 0)
cor(merged$Revenue, merged$usd_t1)
#0.8949


install.packages("openxlsx")
library(openxlsx)
selected_data <- merged[, c("Date", "Revenue", "usd_t1", "cpi_t1", "copper_t1", "interest_t1")]
write.xlsx(selected_data, "selected_data.xlsx")
write.xlsx(selected_data, "C:\\Users\\User\\Desktop\\BFED_project\\Datasets")


lagged_df <- read_excel("C:\\Users\\User\\Desktop\\BFED_project\\Datasets\\laggedData.xlsx")
head(lagged_df)
tail(lagged_df)
lagged_df = lagged_df[4:92,1:5]
sum(is.na(lagged_df))
#0
names(lagged_df)
#[1] "Revenue"     "usd_t1"      "cpi_t1"      "copper_t1"   "interest_t1"

lagged_df$Date <- as.Date(lagged_df$Date, format = "%d.%m.%Y")
class(lagged_df$Date)


revenue_lagged = ts(lagged_df$Revenue, frequency = 12)

tslm_l_full = tslm(revenue_lagged ~ trend + season + copper_t1 +
                     usd_t1+cpi_t1, data=lagged_df)
summary(tslm_l_full)
corrplot(cor(lagged_df[,-1]), method = "number")


lm_l_full = lm(Revenue~usd_t1+cpi_t1+copper_t1, data = lagged_df)
summary(lm_l_full)

library(regclass)
VIF(lm_l_full)

#remove copper 

step_lm_l = stats::step(lm_l_full, direction = "both")
ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = Revenue))+
  geom_point(color = "black")+
  geom_line(aes(y = step_lm_l$fitted.values), color = "red")+
  xlab("Date")+
  ggtitle("Real Data vs. Fitted Values with the Final Model")


ggplot(data = lagged_df,
       aes(x = as.Date(Date), 
           y = step_lm_l$residuals))+
  geom_point(color = "blue")+
  xlab("Date")+
  ylab("Model Residuals")+
  ggtitle("Residuals Plot")



## COMPOSITION OF MODELS
# We can try to model different parts of our series in different ways.
# In particular we will try to model the trend with a tslm model,
# using also the information of the other variables, then the
# seasonal component and the random one together will be modeled 
# by an Arima model.

parts = decompose(revenue_lagged, type = "multiplicative")
season_random = parts$seasonal * parts$random
trend_only = parts$trend

ggplot(data = lagged_df, aes(x = as.Date(Date),
                             y = trend_only))+
  geom_line(color = "blue")+
  xlab("Date")+
  ylab("Revenue")+
  ggtitle("Trend Representation of the Revenue series")


ggplot(data = lagged_df, aes(x = as.Date(Date),
                             y = season_random))+
  geom_line(color = "orange")+
  xlab("Date")+
  ylab("Revenue")+
  ggtitle("Seasonal+Random part of the Revenue series")




# TREND

tslm_trend = tslm(trend_only~usd_t1+cpi_t1+copper_t1, data = lagged_df)
summary(tslm_trend)

# usd is not significant 
# multiple R squared is 0.971
# Plotting in order to see only the trend values and then the 
# results obtained by the model.


ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = trend_only))+
  geom_line(color = "green")+
  xlab("Date")+
  ggtitle("Trend Component of the Revenue series")


ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = trend_only))+
  geom_line(color = "green")+
  geom_line(aes(y = tslm_trend$fitted.values), color = "red")+
  xlab("Date")+
  ylab("Revenue")+
  ggtitle("Real trend vs. Fitted trend")


ggplot(data = lagged_df,
       aes(x = as.Date(Date), 
           y = tslm_trend$residuals))+
  geom_point(color = "blue")+
  xlab("Date")+
  ylab("Residuals")+
  ggtitle("Residuals Plot of the TSLM for Trend")


Acf(tslm_trend$residuals) 
Pacf(tslm_trend$residuals) 



# SEASONALITY and RANDOM COMPONENT

arima_seas_rand = Arima(season_random,
                        order = c(1,0,1),
                        seasonal = c(0,0,0))
arima_seas_rand

# 11.16 (1,0,2)(0,0,0)
# 14.9 (1,1,2)(0,0,0)
# 12.77 (0,1,2)(0,0,0)
# 9.43 (1,0,1)(0,0,0)
# 12.66 (0,1,1)(0,0,0)

ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = season_random))+
  geom_line(color = "blue")+
  geom_line(aes(y = arima_seas_rand$fitted), color = "red")+
  xlab("Date")+
  ggtitle("Real Seasonality*Random vs. Fitted Seasonality*Random")


# Now that we have our model for this component of the series we 
# can try to put togeter the two modeled parts:

fitted_combined = tslm_trend$fitted.values * arima_seas_rand$fitted

ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = Revenue))+
  geom_line(color = "black")+
  geom_line(aes(y = fitted_combined), color = "red")+
  xlab("Date")+
  ggtitle("Real Data vs. Fitted combined values")

# We try now to add the residual component obtained during the
# tslm fitting phase.
# We try to add this information to the season_random part:
# (fitted+res)*season*random
# (fitted*season*random)+(res*season*random)

res_season_random = tslm_trend$residuals*season_random

arima_res_seas_ran = Arima(res_season_random,
                           order = c(1,0,1),
                           seasonal = c(1,1,1))
arima_res_seas_ran

ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = res_season_random))+
  geom_line(color = "black")+
  geom_line(aes(y = arima_res_seas_ran$fitted), color = "red")+
  xlab("Date")+
  ylab("Revenue")+
  ggtitle("Real Seasonal+Random component vs. Fitted values with Arima")

# Then we re-try to combine all together:

fitted_new_combo = fitted_combined + arima_res_seas_ran$fitted

ggplot(data = lagged_df,
       aes(x = as.Date(Date), y = Revenue))+
  geom_line(color = "black")+
  geom_line(aes(y = fitted_new_combo), color = "red")+
  xlab("Date")+
  ggtitle("Real Data vs. Fitted combined values")




## Forecast 

train_size <- as.integer(0.8 * nrow(lagged_df))  # Convert to integer
train_data <- lagged_df[1:train_size, ]
test_data <- lagged_df[(train_size + 1):nrow(lagged_df), ]


model <- lm(Revenue ~ usd_t1 + cpi_t1, data = train_data)
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((predictions - test_data$Revenue)^2))
mae <- mean(abs(predictions - test_data$Revenue))
mape <- mean(abs((predictions - test_data$Revenue) / test_data$Revenue)) * 100

metrics_table <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE"),
  Value = c(rmse, mae, mape))
print(metrics_table)

n <- 12
forecast_data <- tail(lagged_df, n = n)  # Get the last n rows of actual data
forecast_data <- forecast_data[complete.cases(forecast_data), ] #remove NA values (rows)

forecasted_values <- predict(model, newdata = forecast_data)
forecast_data$usd_t1 <- lag(forecast_data$usd_t1, 1)
forecast_data$Revenue_t2 <- lag(forecast_data$Revenue, 2)
cat("Forecasted Revenue for the next", n, "months:", forecasted_values, "\n")

forecast_df <- data.frame(
  Date = seq.Date(from = tail(data$Date, 1) + 1, by = "month", length.out = n),
  Actual_Revenue = rep(NA, n),
  Forecasted_Revenue = forecasted_values)


ggplot() +
  geom_line(data = lagged_df, aes(x = Date, y = Revenue, color = "Actual"), size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecasted_Revenue, color = "Forecast"), size = 1, linetype = "dashed") +
  labs(title = "Actual vs Forecasted Revenue", x = "Date", y = "Revenue") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_minimal()

