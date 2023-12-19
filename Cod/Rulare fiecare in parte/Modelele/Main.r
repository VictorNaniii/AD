library(dplyr)
library(caret)
library(pROC)

# Încărcarea datelor
data <- read.csv("ORATS-OPT.csv")

# Crearea variabilei țintă
data$Target <- ifelse(lag(data$Close.y) < data$Close.y, 1, 0)

# Curățarea și pregătirea datelor
data_clean <- data %>% select(stockpx, iv30, iv60, iv90, m1atmiv, m2atmiv, High.y, Low.y, Open.y, Volume.y, Target) %>%
  na.omit()

# Separarea datelor în set de antrenare și testare
set.seed(123)  # Pentru reproducibilitate
index <- createDataPartition(data_clean$Target, p = .8, list = FALSE)
train_data <- data_clean[index, ]
test_data <- data_clean[-index, ]

# Antrenarea modelului logistic
model <- glm(Target ~ ., data = train_data, family = "binomial")

# Prognozele pe setul de test
predict_proba <- predict(model, newdata = test_data, type = "response")

# Calculul AUC ROC
roc_result <- roc(test_data$Target, predict_proba)
auc(roc_result)

# Plotarea curbei ROC
plot(roc_result, main = "Curba ROC")


library(caret)

# Prognozele modelului pe setul de test (clasificare binară)
predict_class <- ifelse(predict(model, newdata = test_data, type = "response") > 0.5, 1, 0)

# Calculul Preciziei, Recall și Scorului F1
confusion_matrix <- confusionMatrix(factor(predict_class), factor(test_data$Target))

precision <- confusion_matrix$byClass['Precision']
recall <- confusion_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

# Afișarea rezultatelor
cat("Precizie: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("Scorul F1: ", f1_score, "\n")
summary(model)
