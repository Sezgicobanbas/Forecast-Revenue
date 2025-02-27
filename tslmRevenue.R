# Step 1: Prepare the data
# Ensure 'Date' is in Date format
merged_data$Date <- as.Date(merged_data$Date)

# create a ts model with 2023.11 bc with 12 month freq it was our last 12 time 

# Convert 'Revenue' to a time series object
revenue_ts <- ts(merged_data$Revenue, frequency = 12, start = c(2023, 11))
CPI_ts <- ts(merged_data$`CPI(Base=01.2017)`, frequency = 12, start = c(2023, 11))
unemployment_ts <- ts(merged_data$Unemployment.Rate..., frequency = 12, start = c(2023, 11))
copperPrice_ts <- ts(merged_data$Price.x, frequency = 12, start = c(2023, 11))
USDcurrency_ts <- ts(merged_data$Price.y, frequency = 12, start = c(2023, 11))
interestRate_ts <- ts(merged_data$`Interest Rate (%)`, frequency = 12, start = c(2023, 11))  


## CPI 

tslm_revenue_cpı = tslm(revenue_ts ~ trend + season + CPI_ts)
summary(tslm_revenue_cpı)

revenue_12 <- (merged_data[83:94, "Revenue"])
str(revenue_12)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_revenue_cpı$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)

library(ggplot2)

# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))


# Calculate residuals
revenue_plot_data$Residuals <- revenue_plot_data$Revenue - revenue_plot_data$Fitted_Revenue

# the residuals plot

ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Unemployment 

tslm_revenue_unemployment = tslm(revenue_ts ~ trend + season + unemployment_ts)
summary(tslm_revenue_unemployment)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_revenue_unemployment$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)


# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))

# the residuals plot

ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Copper Price 

tslm_revenue_copperPrice = tslm(revenue_ts ~ trend + season + copperPrice_ts)
summary(tslm_revenue_copperPrice)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_revenue_copperPrice$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)


# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))

# the residuals plot

ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## USD 

tslm_revenue_usd = tslm(revenue_ts ~ trend + season + USDcurrency_ts)
summary(tslm_revenue_usd)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_revenue_usd$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)


# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))

# the residuals plot

ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## Interest Rate

tslm_revenue_interestRate = tslm(revenue_ts ~ trend + season + interestRate_ts)
summary(tslm_revenue_interestRate)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_revenue_interestRate$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)


# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))

# the residuals plot

ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Full TSLM
tslm_Revenue_full = tslm(revenue_ts ~ trend + season + interestRate_ts + 
                           CPI_ts + unemployment_ts + copperPrice_ts + 
                           USDcurrency_ts)
summary(tslm_Revenue_full)

tslm_Revenue_full = tslm(revenue_ts ~ trend + interestRate_ts + 
                           CPI_ts + unemployment_ts + copperPrice_ts + 
                           USDcurrency_ts)
summary(tslm_Revenue_full)

# Extract the fitted values from the TSLM model
revenue_fitted_values <- tslm_Revenue_full$fitted.values

# Combine actual and fitted values into a data frame
revenue_plot_data <- data.frame(
  Date = seq(as.Date("2023-11-01"), as.Date("2024-10-01"), by = "month"), # Adjust date range as needed
  Revenue = revenue_12,
  Fitted_Revenue = tail(revenue_fitted_values, 12)  # Use the last 12 fitted values
)

# Plot using ggplot2
ggplot(data = revenue_plot_data, aes(x = as.Date(Date))) +
  geom_line(aes(y = Revenue, color = "Actual Revenue"), linewidth = 1.2) +
  geom_line(aes(y = Fitted_Revenue, color = "Fitted Revenue"), linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Actual vs Fitted Revenue",
    x = "Date",
    y = "Revenue",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual Revenue" = "blue", "Fitted Revenue" = "red"))

# the residuals plot
ggplot(data = revenue_plot_data, 
       aes(x = as.Date(Date), y = Revenue - Fitted_Revenue)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Model Residuals") +
  ggtitle("Residuals Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
