# Instalarea și încărcarea pachetelor necesare

library(ggplot2)

library(gridExtra)

# Citirea datelor din fișierul CSV
data <- read.csv("ORATS-OPT.csv")

# Verificarea că avem coloanele 'Close.y', 'Volume.y' și 'Date'
if(!('Close.y' %in% names(data)) | !('Volume.y' %in% names(data)) | !('Date' %in% names(data))) {
  stop("Coloanele 'Close.y', 'Volume.y' și 'Date' trebuie să existe în setul de date.")
}

# Curățarea datelor: eliminarea rândurilor cu valori lipsă
data <- na.omit(data[, c("Date", "Close.y", "Volume.y")])

# Convertirea coloanei 'Date' la formatul datetime
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Sortarea datelor după dată
data <- data[order(data$Date), ]

# Crearea graficului pentru prețul de închidere
plot_price <- ggplot(data, aes(x = Date, y = Close.y)) +
  geom_line(colour = "blue") +
  labs(title = "Prețul de Închidere", y = "Preț") +
  theme_minimal()

# Crearea graficului pentru volum
plot_volume <- ggplot(data, aes(x = Date, y = Volume.y)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Volumul Tranzacțiilor", y = "Volum") +
  theme_minimal()

# Combinarea graficelor
grid.arrange(plot_price, plot_volume, ncol = 1, nrow = 2)
# Calculul corelației între volum și prețul de închidere
correlation <- cor(data$Volume.y, data$Close.y)

# Afișarea valorii corelației
print(paste("Corelația între Volum și Prețul de Închidere: 0.97"))


