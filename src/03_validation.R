# 03_validation.R
library(forecast)

#--------------------------------------------------
# 1. Setup and Data Partitioning
#--------------------------------------------------
source("validation_function.R") # Load the function

# Load raw data and log transform
serie <- ts(read.table("../data/metro.dat"), start = 1996, frequency = 12)
serie_log <- log(serie)

# Reserve the last 12 observations for testing predictive capability
train_log <- window(serie_log, end = c(2022, 12)) 
test_log  <- window(serie_log, start = 2023)

#--------------------------------------------------
# 2. Re-estimate Models on Training Set
#--------------------------------------------------
# We must re-fit the models using only training data to validate stability
m1_train <- Arima(train_log, order = c(2, 1, 0), 
                  seasonal = list(order = c(0, 1, 1), period = 12), method = "ML")

m2_train <- Arima(train_log, order = c(0, 1, 1), 
                  seasonal = list(order = c(0, 1, 1), period = 12), method = "ML")

#--------------------------------------------------
# 3. Execute Complete Validation
#--------------------------------------------------
sink("../results/models/validation_m1.txt")
cat("MODEL 1 VALIDATION: (2,1,0)x(0,1,1)12\n")
validation(m1_train)
sink()

sink("../results/models/validation_m2.txt")
cat("MODEL 2 VALIDATION: (0,1,1)x(0,1,1)12\n")
validation(m2_train)
sink()

# Save training models for the Prediction step
save(m1_train, m2_train, train_log, test_log, file = "../results/models/validated_models.RData")