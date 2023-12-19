# Incarcam pachetele necesare
library(tidyverse)

# Citim datele
data <- read.csv("ORATS-OPT.csv") # Inlocuiti cu calea corecta

# Sumarizam datele pentru a verifica completitudinea si posibile erori
summary(data)

# Verificam numarul de valori lipsa pentru fiecare coloana
sapply(data, function(x) sum(is.na(x)))

# Verificam consistenta datelor (de exemplu, variabile categorice cu categorii neasteptate)
sapply(data, function(x) if(is.character(x)) unique(x))

# Verificam pentru duplicate
sum(duplicated(data))

# Verificam distributia variabilelor numerice pentru a identifica posibile valori atipice
# Facem histograma pentru toate variabilele numerice
numeric_columns <- data %>% select_if(is.numeric)
histograms <- lapply(names(numeric_columns), function(column_name) {
  ggplot(data, aes_string(x = column_name)) + 
    geom_histogram(bins = 30, fill = "blue", color = "black") + 
    theme_minimal() +
    labs(title = paste("Distributia", column_name), x = column_name, y = "Frecventa")
})

# Vizualizam boxplot pentru a identifica valori atipice pentru variabilele numerice
boxplots <- lapply(names(numeric_columns), function(column_name) {
  ggplot(data, aes_string(y = column_name)) + 
    geom_boxplot(fill = "blue", color = "black") + 
    theme_minimal() +
    labs(title = paste("Boxplot", column_name), y = column_name)
})

# Afisam histograma si boxplot-ul pentru prima variabila numerica ca exemplu
print(histograms[[1]])
print(boxplots[[1]])

# Puteti sa rulati print pentru fiecare element din histograms si boxplots
# pentru a vedea graficele pentru toate variabilele numerice

# Daca este necesar, putem verifica si corelatiile pentru a asigura ca nu exista coliniaritate
correlation_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
corrplot(correlation_matrix, method = "number")
