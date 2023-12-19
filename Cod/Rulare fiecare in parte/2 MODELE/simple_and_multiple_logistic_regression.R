library(tidyverse)
library(broom)

# Încărcarea datelor
data <- read.csv("ORATS-OPT.csv")

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

