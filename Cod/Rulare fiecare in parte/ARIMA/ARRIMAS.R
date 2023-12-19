library(forecast)
library(tseries)
library(readr)

# Încărcarea datelor
ames <- read_csv("ORATS-OPT.csv")

# Conversia coloanei 'Date' în formatul de data
ames$Date <- as.Date(ames$Date, format = "%m/%d/%Y")

# Sortarea datelor după data
ames <- ames[order(ames$Date),]

# Crearea seriei de timp
timeseries <- ts(ames$stockpx, frequency = 252)  # Asumând 252 zile lucrătoare într-un an

# Determinarea lungimii seriei temporale
length_ts <- length(timeseries)

# Calculul indexului de împărțire
split_index <- round(length_ts * 0.8)

# Crearea seturilor de antrenament și test
train_timeseries <- window(timeseries, end = c(split_index / frequency(timeseries)))
test_timeseries <- window(timeseries, start = c((split_index + 1) / frequency(timeseries)))

# Ajustarea modelului ARIMA pe datele de antrenament
model_arima <- auto.arima(train_timeseries)

# Sumarul modelului ARIMA
summary(model_arima)

# Calcularea și afișarea rădăcinilor inverse AR
ar_coefs <- model_arima$coef[grep("ar", names(model_arima$coef))]
ar_roots <- polyroot(c(1, -ar_coefs))
inverse_ar_roots <- 1 / abs(ar_roots)

print("Inverse AR Roots:")
print(inverse_ar_roots)

# Efectuarea predicțiilor pe setul de test
forecasted_values <- forecast(model_arima, h = length(test_timeseries))

# Plotarea seriei de timp de antrenament, seriei de timp de test și a predicțiilor
plot(forecasted_values, main = "ARIMA Forecast vs Actual Data")
lines(train_timeseries, col = "blue")
lines(test_timeseries, col = "green")

# Calcularea MSE și R-squared pentru setul de test
actuals <- test_timeseries
fitted <- forecasted_values$mean
mse <- mean((actuals - fitted)^2)
r_squared <- 1 - sum((actuals - fitted)^2) / sum((actuals - mean(actuals))^2)

# Afișarea MSE și R-squared
print(paste('MSE:', mse))
print(paste('R-squared:', r_squared))

