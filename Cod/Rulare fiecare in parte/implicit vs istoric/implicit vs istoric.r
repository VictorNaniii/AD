# Instalarea și încărcarea pachetelor necesare

library(ggplot2)

# Citirea datelor din fișierul CSV
data <- read.csv("ORATS-OPT.csv")

# Curățarea datelor: eliminarea rândurilor cu valori lipsă
data <- na.omit(data[, c("Date", "iv30", "m2atmiv")])

# Convertirea coloanei 'Date' la formatul datetime
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Sortarea datelor după dată
data <- data[order(data$Date), ]

# Calculul corelației
correlation <- cor(data$iv30, data$m2atmiv)
# Afișarea corelației pe grafic în partea stângă-jos
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = iv30, colour = "Volatilitate Implicită 30 Zile (iv30)")) +
  geom_line(aes(y = m2atmiv, colour = "Volatilitate Istorică 30 Zile (m2atmiv)")) +
  geom_text(x = min(data$Date), y = min(data$iv30),  # Ajustați coordonatele pentru partea stângă-jos
            label = paste("Corelație:", round(correlation, 2)), 
            color = "black", hjust = 0, vjust = 0, size = 4) +  # Ajustați coordonatele și aspectul textului
  labs(title = "Dinamica Corelației între Volatilitatea Implicită și Istorică pe 30 de Zile",
       x = "Data",
       y = "Volatilitate") +
  scale_colour_manual("", 
                      breaks = c("Volatilitate Implicită 30 Zile (iv30)", "Volatilitate Istorică 30 Zile (m2atmiv)"),
                      values = c("blue", "green")) +
  theme_minimal()



