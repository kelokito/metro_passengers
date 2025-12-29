library(forecast)
library(tseries)

data <- read.table("../data/metro.dat", header=FALSE)
serie <- ts(data[,1], start=c(1996, 1), frequency=12)
ln_serie <- log(serie)

# TRAIN / TEST SPLIT
n_omit <- 12
total_len <- length(ln_serie)

train_set <- subset(ln_serie, end = total_len - n_omit)

test_set <- subset(ln_serie, start = total_len - n_omit + 1)

# RE-ESTIMATE MODEL ON TRAINING DATA
mod_stability <- Arima(train_set,
                       order = c(0,1,1),
                       seasonal = list(order = c(0,1,1), period = 12),
                       include.mean = FALSE) # Usually FALSE for differenced data

# Print coefficients to check if they are stable compared to the full model
cat("\n--- Re-estimated Coefficients (1996-2018) ---\n")
print(mod_stability$coef)

# 4. FORECAST TEST SET (2019)
forecast_stability <- forecast(mod_stability, h = n_omit)

# RMSE & CI
errors <- test_set - forecast_stability$mean
rmse <- sqrt(mean(errors^2))

cat("\n--- Stability Metrics (Log Scale) ---\n")
cat("RMSE of prediction:", rmse, "\n")

lower_95 <- forecast_stability$lower[,"95%"]
upper_95 <- forecast_stability$upper[,"95%"]

inside_interval <- (test_set >= lower_95) & (test_set <= upper_95)
percent_coverage <- mean(inside_interval) * 100

cat("Percentage of 2019 observations inside 95% CI:", percent_coverage, "%\n")

if(percent_coverage < 95){
  cat("WARNING: Model may be underestimating uncertainty (Coverage < 95%)\n")
} else {
  cat("SUCCESS: Model stability confirmed (Coverage >= 95%)\n")
}

# PLOTTING
png("../results/figures/05_stability_check.png", width=800, height=500)

plot(forecast_stability,
     main = "Model Stability: 2019 Forecast vs Actual (Log Scale)",
     xlim = c(2017, 2020), # Zoom in on 2017-2019
     ylab = "Log(Passengers)",
     flty = 2,             # Dashed line for forecast
     lwd = 1.5)

# Add the actual observed data for 2019 (Red line)
lines(test_set, col="red", lwd=2)

legend("topleft",
       legend = c("Model Forecast (Train: 96-18)", "Actual Data (2019)"),
       col = c("blue", "red"),
       lty = c(2, 1),
       lwd = c(1.5, 2))

dev.off()

cat("\nStability plot saved to ../results/figures/05_stability_check.png\n")