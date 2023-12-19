# Încărcarea pachetelor necesare
library(tidyverse) # Pentru manipularea și vizualizarea datelor
library(ggplot2)   # Pentru crearea de grafice

# Încărcarea Datelor
data <- read.csv("ORATS-OPT.csv") # Înlocuiți cu calea corectă a fișierului dvs.

# Statistici Descriptive
summary(data)

# Histogramă și Boxplot pentru variabila 'stockpx'
ggplot(data, aes(x = stockpx)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuția Stock Price (stockpx)", x = "Stock Price", y = "Frecvență")

ggplot(data, aes(y = stockpx)) + 
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot Stock Price (stockpx)", y = "Stock Price")

# Repetați acest pas pentru alte variabile de interes
# Exemplu pentru 'iv30':
ggplot(data, aes(x = iv30)) + 
  geom_histogram(bins = 30, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Distribuția IV 30 Days (iv30)", x = "IV 30 Days", y = "Frecvență")

ggplot(data, aes(y = iv30)) + 
  geom_boxplot(fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot IV 30 Days (iv30)", y = "IV 30 Days")

