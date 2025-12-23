# 04_prediction.R
library(forecast)

# Load the models and datasets saved in Step 3
load("../results/models/validated_models.RData")

# Define actual values (original scale) for comparison
# Note: serie_log was created in Step 3; we need the original scale for results
serie <- ts(read.table("../data/metro.dat"), start = 1996, frequency = 12)
obs <- window(serie, start = 2019, end = c(2019, 12))

# Function to perform predictions and compute accuracy metrics
predict_and_evaluate <- function(model, name, train_log, obs) {
  # 1. Forecast 12 months ahead in log scale
  pred_log <- predict(model, n.ahead = 12)
  
  # 2. Convert back to original scale (exp)
  pr <- exp(pred_log$pred)
  se <- pred_log$se
  
  # 95% Confidence Intervals (log scale then exponentiate)
  tl <- exp(pred_log$pred - 1.96 * se)
  tu <- exp(pred_log$pred + 1.96 * se)
  
  # 3. Compute Accuracy Metrics against observed test set
  error <- obs - pr
  rmse  <- sqrt(mean(error^2))
  mae   <- mean(abs(error))
  rmspe <- sqrt(mean((error/obs)^2))
  mape  <- mean(abs(error/obs))
  mean_ci_len <- mean(tu - tl)
  
  # 4. Save metrics to text file
  sink(paste0("../results/models/prediction_accuracy_", name, ".txt"))
  cat("=== PREDICTION ACCURACY (TEST SET 2019):", name, "===\n")
  print(data.frame(RMSE=rmse, MAE=mae, RMSPE=rmspe, MAPE=mape, Mean_CI_Length=mean_ci_len))
  cat("\nMonthly Comparison:\n")
  print(ts(data.frame(LowLim=tl, Predic=pr, UpperLim=tu, Observ=obs, 
                      Error=error, PercentError=error/obs), start=2019, freq=12))
  sink()
  
  # 5. Plot results
  png(paste0("../results/figures/prediction_plot_", name, ".png"), width=900, height=500)
  ultim <- c(2018, 12)
  # Plot last 2 years of training + 1 year of prediction
  ts.plot(serie, tl, tu, pr, lty=c(1,2,2,1), col=c(1,4,4,2), 
          xlim=c(2016, 2020), type="o", 
          main=paste("Forecast Performance:", name))
  abline(v=2016:2020, lty=3, col="gray")
  legend("topleft", legend=c("Observed", "Predicted", "95% CI"), 
         col=c(1, 2, 4), lty=c(1, 1, 2), bty="n")
  dev.off()
}

# Execute for both models
predict_and_evaluate(m1_train, "Model_1", train_log, obs)
predict_and_evaluate(m2_train, "Model_2", train_log, obs)
