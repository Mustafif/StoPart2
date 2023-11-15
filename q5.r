if (!requireNamespace("stats", quietly = TRUE)) {
    install.packages("stats")
}
if (!requireNamespace("forecast", quietly = TRUE)) {
    install.packages("forecast")
}

# install.packages("tseries")
library(tseries)
library(forecast)
library(stats)


data <- read.csv("Data3.csv", header = FALSE)
# a)
plot(data$V1,
    type = "l", xlab = "Index", ylab = "Values",
    main = "Series of Historical Values"
)
# b)
acf(data$V1, main = "Autocorrelation Function", lag.max = 20)
pacf(data$V1, main = "Partial Autocorrelation Function", lag.max = 20)

# c)
# chose p = 2 because lag 2
# has two spikes between acf and pacf

# chose d=0 because data does not show
# strong correlation at lag=1

# chose q=1 because signifcant spike at lag 1
# at acf and indicates some remaing autocorrelation
# that can be captured by MA(1)
arima_model <- arima(data$V1, order = c(2, 0, 1))
fitted_values <- fitted(arima_model)

# Plot original time series and fitted values
ts.plot(data$V1, fitted_values, col = c("black", "purple"), lty = 1, lwd = c(1, 2), main = "Original vs. Fitted Values")

# Add legend
legend("topright", legend = c("Original", "Fitted"), col = c("black", "purple"), lty = 1, lwd = c(1, 2))

# d) 
summary(arima_model)

# Coefficients given: 
# ar1: 0.5919 => \phi_1
# ar2: -0.0676 => \phi_2 
# ma1: 0.3314 => \theta_1 
# intercept: 2.0217 => c
# Formula: 
# Y_t = c + \phi_1 Y_{t-1} + \phi_2 Y_{t-2} + \theta_1 \epsilon_{t-1} + \epsilon_t