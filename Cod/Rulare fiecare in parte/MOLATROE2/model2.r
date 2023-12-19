# Instalarea și încărcarea pachetelor necesare
library(tidyverse)
library(caret)
library(randomForest)

# Citirea datelor de antrenament
date_opt <- read.csv('ORATS-OPT.csv')

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

