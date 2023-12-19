# Instalarea și încărcarea pachetelor necesare
library(ggplot2)

# Citirea datelor din fișierul CSV
data <- read.csv("ORATS-OPT.csv")

# Verificarea că avem coloana 'Close.y' și 'Date'
if(!('Close.y' %in% names(data)) | !('Date' %in% names(data))) {
  stop("Coloanele 'Close.y' și 'Date' trebuie să existe în setul de date.")
}

# Curățarea datelor: eliminarea rândurilor cu valori lipsă
data <- na.omit(data[, c("Date", "Close.y")])

# Convertirea coloanei 'Date' la formatul datetime
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Sortarea datelor după dată
data <- data[order(data$Date), ]

# Calculul mediei mobile pentru diferite perioade de timp
data$SMA30 <- stats::filter(data$Close.y, rep(1/30, 30), sides = 2)
data$SMA60 <- stats::filter(data$Close.y, rep(1/60, 60), sides = 2)

# Crearea unui grafic
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Close.y, colour = "Preț de Închidere")) +
  geom_line(aes(y = SMA30, colour = "Moving average 30 Zile")) +
  geom_line(aes(y = SMA60, colour = "Moving average 60 Zile")) +
  labs(title = "Media Mobilă și Prețul de Închidere",
       x = "Data",
       y = "Preț") +
  scale_colour_manual("", 
                      breaks = c("Preț de Închidere", "Moving average 30 Zile", "Moving average 60 Zile"),
                      values = c("black", "blue", "red")) +
  theme_minimal()

