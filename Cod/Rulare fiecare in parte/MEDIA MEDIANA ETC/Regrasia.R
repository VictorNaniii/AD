# Incarcarea bibliotecilor necesare
library(randomForest)
library(caret)
library(dplyr)

# Citirea datelor
data <- read.csv("D:/UTM AN3/SEM 1/AD/RAPID PENTRU ATESTARE FACEM DUPA SCHIMB/Modelele/ORATS-OPT.csv")

# Curatarea si pregatirea datelor
data <- na.omit(data) # Eliminarea randurilor cu valori lipsa

# Definirea variabilei tinta ca fiind 1 daca 'stockpx' este peste percentila 75, altfel 0
high_price_threshold <- quantile(data$stockpx, 0.75)
data$HighPrice <- ifelse(data$stockpx > high_price_threshold, 1, 0)

# Selectarea variabilelor de interes (ajustati acest lucru in functie de nevoile dvs.)
data_model <- data %>% select(stockpx, iv30, iv60, iv90, m1atmiv, m1dtex, HighPrice)

# Impartirea datelor in set de antrenament si test
set.seed(123) # Pentru reproductibilitate
indexes <- createDataPartition(data_model$HighPrice, p = 0.8, list = FALSE)
train_data <- data_model[indexes, ]
test_data <- data_model[-indexes, ]

# Antrenarea modelului RandomForest
rf_model <- randomForest(HighPrice ~ ., data = train_data)

# Antrenarea modelului Logistic Regression
lr_model <- glm(HighPrice ~ ., data = train_data, family = "binomial")

# Evaluarea modelelor pe setul de test
rf_predictions <- predict(rf_model, test_data, type = "class")
lr_predictions <- predict(lr_model, test_data, type = "response")
lr_predictions <- ifelse(lr_predictions > 0.5, 1, 0)

# Calcularea acuratetii
rf_accuracy <- mean(rf_predictions == test_data$HighPrice)
lr_accuracy <- mean(lr_predictions == test_data$HighPrice)

# Afișarea acurateții
print(paste("Random Forest Accuracy: ", rf_accuracy))
print(paste("Logistic Regression Accuracy: ", lr_accuracy))

