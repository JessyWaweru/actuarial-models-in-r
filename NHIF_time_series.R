# Load required libraries
library(ggplot2)
library(scales)
library(forecast) # Essential for Time Series Analysis (ARIMA)
library(gridExtra)

# 1. NHIF HISTORICAL DATA
# Annual contributions and claims, financial years 2016/17 to 2023/24
years <- 2017:2024 # Represents financial years ending 2017 to 2024
contributions <- c(35071.6, 45409.9, 56287.1, 59500.5, 60775.4, 78392.3, 81596.4, 72949.1)
claims <- c(26524.1, 39061.3, 53424.6, 53462.6, 54061.8, 63856.7, 67907.2, 68300.6)

df_history <- data.frame(Year = years, Contributions = contributions, Claims = claims)

# 2. CALCULATE THE LOSS RATIO TRAJECTORY
df_history$LossRatio <- df_history$Claims / df_history$Contributions

loss_ratio_plot <- ggplot(df_history, aes(x = Year, y = LossRatio)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "black", size = 2.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = years) +
  labs(
    title = "Historical NHIF Loss Ratio Trajectory (2016 - 2024)",
    subtitle = "Mathematical deterioration of the national risk pool prior to SHIF transition",
    x = "Financial Year",
    y = "Loss Ratio (Claims / Contributions)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# 3. TIME SERIES FORECASTING (ARIMA)
# Convert historical claims to a Time Series object
ts_claims <- ts(df_history$Claims, start = 2017, frequency = 1)

# Fit an Auto-ARIMA model to the historical data
fit_arima <- auto.arima(ts_claims)
# ADD THIS LINE TO SEE THE ARIMA(P, D, Q) VALUES:
print(fit_arima)
# Forecast the next 5 years (2025 - 2029) without SHIF shocks
forecast_claims <- forecast(fit_arima, h = 5)

# Convert forecast object to a dataframe for ggplot
df_forecast <- data.frame(
  Year = 2025:2029,
  Forecasted_Claims = as.numeric(forecast_claims$mean),
  Lower_80 = as.numeric(forecast_claims$lower[,1]),
  Upper_80 = as.numeric(forecast_claims$upper[,1])
)

# Plot the ARIMA Forecast
forecast_plot <- ggplot() +
  # Historical Data
  geom_line(data = df_history, aes(x = Year, y = Claims), color = "black", linewidth = 1) +
  geom_point(data = df_history, aes(x = Year, y = Claims), color = "black", size = 2) +
  # Forecasted Data
  geom_line(data = df_forecast, aes(x = Year, y = Forecasted_Claims), color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(data = df_forecast, aes(x = Year, y = Forecasted_Claims), color = "red", size = 2) +
  # Confidence Intervals
  geom_ribbon(data = df_forecast, aes(x = Year, ymin = Lower_80, ymax = Upper_80), fill = "red", alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2017:2029) +
  labs(
    title = "ARIMA Time Series Forecast: Expected Claims (2025-2029)",
    subtitle = "Projecting the baseline claim trajectory without SHIF intervention",
    x = "Year",
    y = "Benefits Paid Out (Ksh Millions)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# 4. SAVE BOTH GRAPHS
# This will save a combined high-res image to your VS Code folder
ggsave("NHIF_Historical_Analysis.png", 
       arrangeGrob(loss_ratio_plot, forecast_plot, ncol = 1), 
       width = 8, height = 10, dpi = 300, bg = "white")

print("Analysis complete. Check your folder for NHIF_Historical_Analysis.png")