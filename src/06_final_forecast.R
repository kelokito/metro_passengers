library(forecast)

data <- read.table("../data/metro.dat", header=FALSE)
serie <- ts(data[,1], start=c(1996, 1), frequency=12)
ln_serie <- log(serie)

# Fit the Final Chosen Model (Model 2: Airline Model)
final_model <- Arima(ln_serie,
                     order = c(0,1,1),
                     seasonal = list(order = c(0,1,1), period = 12))

# Forecast 12 months
h_steps <- 12
pred_log <- forecast(final_model, h = h_steps)

# BACK-TRANSFORMATION WITH BIAS CORRECTION
sigma2 <- final_model$sigma2
pred_mean_orig <- exp(pred_log$mean + 0.5 * sigma2)

# Confidence Intervals (95%)
lower_orig <- exp(pred_log$lower[,"95%"])
upper_orig <- exp(pred_log$upper[,"95%"])

# TABLE FOR THE REPORT
forecast_2020 <- data.frame(
  Month = format(seq(as.Date("2020-01-01"), by="month", length.out=12), "%B %Y"),
  Forecasted_Passengers = round(pred_mean_orig, 0),
  Lower_95_CI = round(lower_orig, 0),
  Upper_95_CI = round(upper_orig, 0)
)

print(forecast_2020)
write.csv(forecast_2020, "../results/models/final_forecast_2020.csv", row.names = FALSE)

# FINAL VISUALISATION
png("../results/figures/final_forecast_2020.png", width=900, height=500)
plot(pred_log, main="Barcelona Metro: 2020 Forecast (Projected)",
     ylab="Log(Passengers)", xlim=c(2015, 2021))
dev.off()