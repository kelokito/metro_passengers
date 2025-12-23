# Typical reasonable candidates for this type of data:
model1 <- Arima(serie_log,
                order = c(1,d,1),
                seasonal = c(0,D,1))

model2 <- Arima(serie_log,
                order = c(2,d,1),
                seasonal = c(0,D,1))

model3 <- Arima(serie_log,
                order = c(1,d,2),
                seasonal = c(1,D,1))

sink("../results/models/sarima_candidates.txt")
cat("=== SARIMA Candidate Models ===\n")
cat("\nModel 1: SARIMA(1,",d,",1)(0,",D,",1)[12]\n")
print(model1)

cat("\nModel 2: SARIMA(2,",d,",1)(0,",D,",1)[12]\n")
print(model2)

cat("\nModel 3: SARIMA(1,",d,",2)(1,",D,",1)[12]\n")
print(model3)
sink()

#--------------------------------------------------
# 9. Save for estimation stage
#--------------------------------------------------

saveRDS(list(
  serie_raw = serie,
  serie_log = serie_log,
  serie_stationary = serie_diff_seas,
  d = d,
  D = D
), file="../results/models/preprocessed_data.rds")
