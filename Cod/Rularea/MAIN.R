# Load necessary packages
#install.packages("ggplot2")
library(gapminder)
library(dplyr)
library(ggplot2)

OPT <- read.csv("ORATS-OPT.csv")


# Assuming OPT is your dataframe containing the data

# Convert the date column to a Date class if it's not already
OPT$date <- as.Date(OPT$date, format = "%m/%d/%Y")

# Filter data for the specified date range
filtered_data <- OPT %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2018-01-01"))

# Calculate mean and median stock prices by ticker
ticker_summary <- filtered_data %>%
  group_by(ticker) %>%
  summarize(mean_stock_price = mean(stockpx),
            median_stock_price = median(stockpx))

# Create a boxplot of stock prices by ticker
ggplot(filtered_data, aes(x = ticker, y = stockpx)) +
  geom_boxplot() +
  labs(title = "Stock Prices by Ticker (2007)",
       x = "Ticker",
       y = "Stock Price")

# Calculate summary statistics for stock prices by ticker
ticker_summary_stats <- filtered_data %>%
  group_by(ticker) %>%
  summarize(sd_stock_price = sd(stockpx),
            iqr_stock_price = IQR(stockpx),
            num_dates = n())

# Create a density plot of stock prices by ticker
ggplot(filtered_data, aes(x = stockpx, fill = ticker)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Stock Prices by Ticker",
       x = "Stock Price",
       y = "Density")

# Filter data for a specific condition (e.g., stockpx > 50)
high_stock_data <- filtered_data %>%
  filter(stockpx > 50)

# Calculate mean and standard deviation of stock prices for high stock data
mean_stock_price_high <- mean(high_stock_data$stockpx)
sd_stock_price_high <- sd(high_stock_data$stockpx)

# Create a density plot of population
ggplot(filtered_data, aes(x = stockpx)) +
  geom_density() +
  labs(title = "Density Plot of Stock Prices",
       x = "Stock Price",
       y = "Density")

# Transform data (e.g., log-transform population)
transformed_data <- filtered_data %>%
  mutate(log_stockpx = log(stockpx))

# Create a density plot of log-transformed stock prices
ggplot(OPT, aes(x = log(stockpx))) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of Log-Transformed Stock Prices",
       x = "Log(Stock Price)",
       y = "Density") +
  theme_minimal()

# Filter data based on a condition (e.g., stockpx < 50)
low_stock_data <- filtered_data %>%
  filter(stockpx < 50)

# Create a boxplot of stock prices by a condition
ggplot(low_stock_data, aes(x = "", y = stockpx)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stock Prices (Low Values)",
       x = "Condition",
       y = "Stock Price")

# Filter data based on a condition and create a boxplot with outliers removed
low_stock_data <- low_stock_data %>%
  mutate(is_outlier = ifelse(stockpx < 50, TRUE, FALSE))

ggplot(low_stock_data, aes(x = "", y = stockpx, fill = is_outlier)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stock Prices (Outliers Removed)",
       x = "Condition",
       y = "Stock Price") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Outlier")


#Scaterplot IV30 vs IV60

scatterplot <- ggplot(data, aes(x = iv30, y = iv60)) +
  geom_point() +
  labs(x = "IV30", y = "IV60") +
  ggtitle("Scatterplot IV30 vs. IV60") +
  geom_smooth(method = "lm", se = FALSE) # Adaugă o linie de regresie liniară
print(scatterplot)



# Crearea scatterplot-ului pentru variabilele "m1atmiv" și "m1dtex"
scatterplot <- ggplot(data, aes(x = m1atmiv, y = m1dtex)) +
  geom_point() + # Adăugați punctele pe grafic
  labs(x = "M1ATMIV", y = "M1DTEX") + # Adăugați etichete pentru axele x și y
  ggtitle("Scatterplot M1ATMIV vs. M1DTEX") # Adăugați un titlu graficului

# Afișarea scatterplot-ului
print(scatterplot)


# Transformarea variabilei 'stockpx' într-o variabilă binară logistică
threshold <- median(data$stockpx, na.rm = TRUE)
data$stockpx_binary <- ifelse(data$stockpx > threshold, 1, 0)

# Regresie Logistică Simplă
simple_logistic_model <- glm(stockpx_binary ~ iv30, data = data, family = binomial)

# Regresie Logistică Multiplă
multiple_logistic_model <- glm(stockpx_binary ~ iv30 + iv60 + iv90, data = data, family = binomial)

# Extragem coeficienții și statistici într-un tabel pentru modelul simplu
simple_model_summary <- broom::tidy(simple_logistic_model)

# Extragem coeficienții și statistici într-un tabel pentru modelul multiplu
multiple_model_summary <- broom::tidy(multiple_logistic_model)

# Crearea unui tabel comparativ
comparative_table <- bind_rows(Simple = simple_model_summary, Multiple = multiple_model_summary, .id = "Model")
comparative_table

# Specificați câte observații doriți să utilizați pentru antrenament (de exemplu, 80% din date)
procentaj_antrenament <- 0.8
numar_observatii_antrenament <- round(nrow(date_opt) * procentaj_antrenament)

# Selectați un eșantion aleator din datele de antrenament
date_opt_antrenament <- sample_n(date_opt, numar_observatii_antrenament)

# Curățarea datelor pentru a elimina rândurile cu valori lipsă
variabile_predictor <- c('Volume.y', 'slope_inf', 'deriv_inf', 'm1dtex', 'iv90', 'm3dtex')  # Predictor variables
date_opt_curatate <- na.omit(date_opt_antrenament[, c('stockpx', variabile_predictor)])

# Crearea modelului Random Forest pe setul de antrenament
set.seed(123)  # Pentru reproducibilitate
model_rf <- randomForest(stockpx ~ ., data = date_opt_curatate, importance = TRUE)

# Citirea setului de date independent 'ORATS-OP.csv'
date_op <- read.csv('/mnt/data/ORATS-OP.csv')
date_op_curatate <- na.omit(date_op[, c('stockpx', variabile_predictor)])

# Efectuarea predicțiilor pe setul de date independent
predictii_op <- predict(model_rf, newdata = date_op_curatate)

# Crearea graficului pentru setul de date independent
ggplot(date_op_curatate, aes(x = stockpx, y = predictii_op)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = 'black', linetype = "dashed") +
  labs(x = "Valori Reale ale Stockpx", y = "Predicții ale Stockpx", 
       title = "Validare Model Random Forest pe Set Independent") +
  theme_minimal()

# Calcularea MSE și R-squared pentru validare
mse_op <- mean((predictii_op - date_op_curatate$stockpx) ^ 2)
r2_op <- summary(lm(predictii_op ~ date_op_curatate$stockpx))$r.squared
accuracy_op <- mean(abs(predictii_op - date_op_curatate$stockpx) / date_op_curatate$stockpx)

# Afișarea evaluării pentru setul de date independent
print(paste('MSE pentru setul independent:', mse_op))
print(paste('R-squared pentru setul independent:', r2_op))
print(paste('Acuratețea pentru setul independent:', accuracy_op))

