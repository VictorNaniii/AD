# Incarcam pachetele necesare
library(tidyverse)

# Citim datele
data <- read.csv("ORATS-OPT.csv") # Asigurati-va ca path-ul este corect

# Analiza corelatiilor
correlation_matrix <- cor(data %>% select_if(is.numeric))
correlations_with_stockpx <- correlation_matrix["stockpx",]
print(correlations_with_stockpx)

# Analiza de regresie liniara
# Presupunem ca toate celelalte variabile numerice sunt predictori
# Eliminam 'stockpx' din predictori pentru a fi variabila dependenta
numeric_vars <- names(data)[sapply(data, is.numeric)]
predictors <- setdiff(numeric_vars, "stockpx")

# Construim formula pentru regresie
formula <- as.formula(paste("stockpx ~", paste(predictors, collapse = " + ")))

# Ajustam modelul de regresie liniara
model <- lm(formula, data = data)
summary(model)

# Afisam coeficientii pentru a vedea care variabile au coeficienti semnificativi
print(coef(summary(model)))

