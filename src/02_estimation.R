library(forecast)

#--------------------------------------------------
# 1. Load data and previous transformations
#--------------------------------------------------
# Ensure the raw data is loaded and log-transformed as per 01_identification
serie <- window(
  ts(read.table("../data/metro.dat"),
     start = 1996,
     frequency = 12)
)
serie_log <- log(serie)

#--------------------------------------------------
# 2. Estimate Model 1: SARIMA(2,1,0) x (0,1,1)12
#--------------------------------------------------
# p=2, d=1, q=0 | P=0, D=1, Q=1
model1 <- Arima(serie_log, 
                order = c(2, 1, 0), 
                seasonal = list(order = c(0, 1, 1), period = 12),
                method = "ML")

sink("../results/models/model1_summary.txt")
cat("ESTIMATION: Model 1 - SARIMA(2,1,0)x(0,1,1)12\n")
print(summary(model1))
sink()

#--------------------------------------------------
# 3. Estimate Model 2: SARIMA(0,1,1) x (0,1,1)12
#--------------------------------------------------
# p=0, d=1, q=1 | P=0, D=1, Q=1
model2 <- Arima(serie_log, 
                order = c(0, 1, 1), 
                seasonal = list(order = c(0, 1, 1), period = 12),
                method = "ML")

sink("../results/models/model2_summary.txt")
cat("ESTIMATION: Model 2 - SARIMA(0,1,1)x(0,1,1)12\n")
print(summary(model2))
sink()

#--------------------------------------------------
# 4. Preliminary Comparison (AIC/BIC)
#--------------------------------------------------
sink("../results/models/estimation_comparison.txt")
cat("MODEL COMPARISON\n\n")
cat("Model 1 (2,1,0)x(0,1,1)12 AIC:", model1$aic, "\n")
cat("Model 1 (2,1,0)x(0,1,1)12 BIC:", model1$bic, "\n\n")
cat("Model 2 (0,1,1)x(0,1,1)12 AIC:", model2$aic, "\n")
cat("Model 2 (0,1,1)x(0,1,1)12 BIC:", model2$bic, "\n")
sink()

# Save model objects for use in 03_validation.R
save(model1, model2, file = "../results/models/estimated_models.RData")

cat("Estimation complete. Results saved in ../results/models/\n")
