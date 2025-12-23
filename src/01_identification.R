# 01_identification.R
library(forecast)
library(tseries)
library(ggplot2)

#--------------------------------------------------
# 1. Load monthly metro passenger series
#--------------------------------------------------

serie <- window(
  ts(read.table("../data/metro.dat"),
     start = 1996,
     frequency = 12)
)

# Create folders if missing
dir.create("../results/models", showWarnings = FALSE, recursive = TRUE)
dir.create("../results/figures", showWarnings = FALSE, recursive = TRUE)

#--------------------------------------------------
# 2. Plot raw series
#--------------------------------------------------

png("../results/figures/raw_series.png", width=900, height=450)
autoplot(serie) +
  ggtitle("Barcelona Metro Passengers (Monthly)") +
  ylab("Thousands of passengers")
dev.off()

#--------------------------------------------------
# 3. Stationarity tests on raw series
#--------------------------------------------------

adf_raw  <- adf.test(serie)
kpss_raw <- kpss.test(serie)

sink("../results/models/stationarity_raw.txt")
cat("=== Stationarity Tests: RAW Series ===\n")
print(adf_raw)
print(kpss_raw)
sink()

# Conclusion:
# - ADF fails to reject unit root
# - KPSS rejects stationarity
# -> Series is NOT stationary

#--------------------------------------------------
# 4. Log transformation (variance stabilization)
#--------------------------------------------------

# --- Mean–Variance relationship (yearly blocks) ---
serie_mat <- matrix(serie, nrow = 12)

annual_means <- apply(serie_mat, 2, mean, na.rm = TRUE)
annual_vars  <- apply(serie_mat, 2, var,  na.rm = TRUE)

png("../results/figures/mean_variance_raw.png", width = 900, height = 450)
plot(annual_means, annual_vars,
     xlab = "Annual means",
     ylab = "Annual variances",
     main = "Mean–Variance Relationship (Raw Series)")
abline(lm(annual_vars ~ annual_means), col = 2, lty = 3, lwd = 2)
dev.off()

# --- Boxplot by year (raw series) ---
png("../results/figures/boxplot_raw_by_year.png", width = 900, height = 450)
boxplot(serie ~ floor(time(serie)),
        main = "Yearly Distribution of Raw Series",
        xlab = "Year",
        ylab = "Passengers (thousands)",
        las = 2)
dev.off()

# --- Log transformation ---
serie_log <- log(serie)

png("../results/figures/log_series.png", width = 900, height = 450)
autoplot(serie_log) +
  ggtitle("Log-transformed Metro Passengers") +
  ylab("Log passengers")
dev.off()

# --- Mean–Variance relationship after log ---
serie_log_mat <- matrix(serie_log, nrow = 12)
annual_means_log <- apply(serie_log_mat, 2, mean, na.rm = TRUE)
annual_vars_log  <- apply(serie_log_mat, 2, var,  na.rm = TRUE)

png("../results/figures/mean_variance_log.png", width = 900, height = 450)
plot(annual_means_log, annual_vars_log,
     xlab = "Annual means (log)",
     ylab = "Annual variances (log)",
     main = "Mean–Variance Relationship (Log Series)")
abline(lm(annual_vars_log ~ annual_means_log),
       col = 2, lty = 3, lwd = 2)
dev.off()

# Stationarity tests on log series
adf_log  <- adf.test(serie_log)
kpss_log <- kpss.test(serie_log)

sink("../results/models/stationarity_log.txt")
cat("=== Stationarity Tests: LOG Series ===\n")
print(adf_log)
print(kpss_log)
sink()

# Decomposition of log series
png("../results/figures/decomposition_log.png", width = 900, height = 650)
plot(decompose(serie_log), main = "Classical Decomposition of Log Series")
dev.off()

# Monthplot
png("../results/figures/monthplot_log.png", width = 900, height = 450)
monthplot(serie_log, main="Monthly Pattern (Log Series)")
dev.off()

#--------------------------------------------------
# 5. Seasonal differencing
#--------------------------------------------------

# Remove yearly seasonality
serie_log_d12 <- diff(serie_log, lag = 12)
png("../results/figures/seasonal_diff12.png", width = 900, height = 450)
autoplot(serie_log_d12) +
  ggtitle("Seasonal Differenced Series (12 months)") +
  ylab("Log passengers difference")
abline(h=0)
dev.off()

# Remove trend (first difference) after seasonal differencing
serie_log_d12_d1 <- diff(serie_log_d12, differences = 1)
png("../results/figures/diff_seasonal_trend.png", width = 900, height = 450)
autoplot(serie_log_d12_d1) +
  ggtitle("Differenced Series (Trend + Seasonal)") +
  ylab("Differenced log passengers") +
  geom_hline(yintercept = 0, color = "red", lwd = 1.2)
dev.off()

# Variance check
cat("Variance check:\n")
cat("Raw series variance:", var(serie_log), "\n")
cat("Seasonal differenced variance:", var(serie_log_d12), "\n")
cat("Differenced + seasonal variance:", var(serie_log_d12_d1), "\n")

#--------------------------------------------------
# 6. Final stationarity verification
#--------------------------------------------------

png("../results/figures/stationary_series.png", width = 900, height = 450)
autoplot(serie_log_d12_d1) +
  ggtitle("Final Stationary Series") +
  ylab("Differenced log-passengers")
dev.off()

adf_final  <- adf.test(na.omit(serie_log_d12_d1))
kpss_final <- kpss.test(na.omit(serie_log_d12_d1))

sink("../results/models/stationarity_final.txt")
cat("=== Stationarity Tests: FINAL Series ===\n")
print(adf_final)
print(kpss_final)
sink()

#--------------------------------------------------
# 7. ACF & PACF analysis
#--------------------------------------------------

png("../results/figures/acf_pacf.png", width=1200, height=600)
par(mfrow=c(1,2))
acf(na.omit(serie_log_d12_d1), main="ACF (Stationary Series)")
pacf(na.omit(serie_log_d12_d1), main="PACF (Stationary Series)")
dev.off()

# Interpretation guidelines:
# - PACF cutoff → AR order (p)
# - ACF cutoff → MA order (q)
# - Seasonal spikes at lag 12 → include seasonal AR/MA terms (P, Q)
