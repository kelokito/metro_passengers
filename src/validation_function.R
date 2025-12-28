validation=function(model, model_name){
  s=frequency(model$x)
  resid=model$residuals
  
  # a) Save Residual Analysis (4-panel plot)
  png(paste0("../results/figures/validation_", model_name, "_residuals.png"), width=900, height=800)
  par(mfrow=c(2,2), mar=c(4,4,3,3))
  # Residuals plot
  plot(resid, main=paste("Residuals:", model_name))
  abline(h=0)
  abline(h=c(-3*sd(resid),3*sd(resid)), lty=3, col=4)
  # Homoscedasticity
  scatter.smooth(sqrt(abs(resid)), main="Square Root of Absolute residuals", lpars=list(col=2))
  # Normality
  qqnorm(resid)
  qqline(resid, col=2, lwd=2)
  hist(resid, breaks=20, freq=FALSE, main="Histogram of Residuals")
  curve(dnorm(x, mean=mean(resid), sd=sd(resid)), col=2, add=TRUE)
  dev.off()
  
  # b) Save ACF & PACF of residuals
  png(paste0("../results/figures/validation_", model_name, "_acf_pacf.png"), width=1000, height=500)
  par(mfrow=c(1,2))
  acf(resid, ylim=c(-1,1), lag.max=60, col=c(2, rep(1, s-1)), lwd=1, main="ACF of Residuals")
  pacf(resid, ylim=c(-1,1), lag.max=60, col=c(rep(1, s-1), 2), lwd=1, main="PACF of Residuals")
  dev.off()
  
  # c) Save Ljung-Box p-values (tsdiag)
  png(paste0("../results/figures/validation_", model_name, "_tsdiag.png"), width=800, height=600)
  tsdiag(model, gof.lag=7*s)
  dev.off()

  # d) Save Stability plot (Inverse Roots / Unit Circle)
  png(paste0("../results/figures/validation_", model_name, "_unit_circle.png"), width=600, height=600)
  plot(model)
  dev.off()

  # --- Numerical Output (Text only) ---
  cat("\n--------------------------------------------------------------------\n")
  print(model)
  
  # 1. CAUSALITY AND INVERTIBILITY CHECK
  # Notes state roots of Phi(B) and Theta(B) must be > 1 in modulus
  cat("\n--- Causality & Invertibility (Roots Modulus) ---\n")
  if (length(model$model$phi) > 0) {
    cat("Modul of AR Roots: ", Mod(polyroot(c(1, -model$model$phi))), "\n")
  }
  if (length(model$model$theta) > 0) {
    cat("Modul of MA Roots: ", Mod(polyroot(c(1, model$model$theta))), "\n")
  }
  
  # 2. Psi and phi weights
  psis = ARMAtoMA(ar=model$model$phi, ma=model$model$theta, lag.max=12)
  cat("\nPsi-weights (MA-infinity representation):\n")
  print(round(psis, 4))

  pis=-ARMAtoMA(ar=-model$model$theta, ma=-model$model$phi, lag.max=12)
  cat("\nPi-weights (AR(inf))\n")
  print(pis[1:12])
   
  # Formal Tests
  suppressMessages(require(fBasics, quietly=TRUE))
  cat("\nDescriptive Statistics\n") 
  print(basicStats(resid))
  
  cat("\nNormality Tests\n")
  print(shapiro.test(resid))
  
  suppressMessages(require(lmtest, quietly=TRUE))
  cat("\nHomoscedasticity (Breusch-Pagan)\n")
  obs=model$x
  print(bptest(resid ~ I(obs-resid)))
  
  cat("\nIndependence (Ljung-Box)\n")
  lags_to_test = unique(c(1, 2, 3, s, 2*s))
  results = t(apply(matrix(lags_to_test), 1, function(el) {
  te = Box.test(resid, type="Ljung-Box", lag=el)
  c(lag=el, statistic=te$statistic[[1]], p.value=te$p.value)
  }))
  print(results)
}