# 03_validation.R
library(forecast)

#--------------------------------------------------
# 1. Setup and Data Partitioning
#--------------------------------------------------
source("validation_function.R") 

# Load raw data and log transform
serie <- ts(read.table("../data/metro.dat"), start = 1996, frequency = 12)
serie_log <- log(serie)

# Divide into Train (before 2019) and Test (2019 onwards)
train_log <- window(serie_log, end = c(2018, 12)) 
test_log  <- window(serie_log, start = 2019)

#--------------------------------------------------
# 2. Re-estimate Models on Training Set
#--------------------------------------------------
m1_train <- Arima(train_log, order = c(2, 1, 0), 
                  seasonal = list(order = c(0, 1, 1), period = 12), method = "ML")

m2_train <- Arima(train_log, order = c(0, 1, 1), 
                  seasonal = list(order = c(0, 1, 1), period = 12), method = "ML")

#--------------------------------------------------
# 3. Execute Complete Validation
#--------------------------------------------------
# This will save images in ../results/figures/ and text in ../results/models/
sink("../results/models/validation_m1.txt")
validation(m1_train, "m1")
sink()

sink("../results/models/validation_m2.txt")
validation(m2_train, "m2")
sink()

# Save training models for the Prediction step
save(m1_train, m2_train, train_log, test_log, file = "../results/models/validated_models.RData")
