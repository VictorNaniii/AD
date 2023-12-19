library(ggplot2)

# Încărcarea datelor
data <- read.csv("ORATS-OPT.csv")

# Calculul corelației
correlation <- cor(data$stockpx, data$iv30, use = "complete.obs")

# Crearea graficului scatter cu axa x din 5 în 5
plot(data$stockpx, data$iv30, main = "Relația dintre Prețul Acțiunilor și Volatilitatea Implicită (30 zile)",
     xlab = "Prețul Acțiunilor (stockpx)", ylab = "Volatilitatea Implicită 30 zile (iv30)",
     xaxt = "n") # Dezactivează axa x implicită

# Adăugarea unei axe x personalizate
axis(1, at = seq(from = min(data$stockpx, na.rm = TRUE), to = max(data$stockpx, na.rm = TRUE), by = 5))

# Afișarea corelației pe grafic pentru referință
mtext(paste("Correlația este: ", round(correlation, 2)), side = 3)

# Aplicarea PCA
pca_result <- prcomp(subset_data_scaled)

# Sumarul rezultatelor PCA
summary(pca_result)

# Plot pentru primele două componente principale
plot(pca_result$x[,1:2], main = "PCA - Primele două componente principale")


#====

# Presupunem că 'Date' este coloana cu datele în format corespunzător
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# Sortarea datelor după data
data <- data[order(data$Date), ]

# Deschiderea unei ferestre grafice noi
plot.new()

# Crearea primului grafic (pentru prețul acțiunilor)
plot(data$Date, data$stockpx, type = "l", col = "blue",
     xlab = "Timp", ylab = "Prețul Acțiunilor",
     main = "Dinamica Prețului Acțiunilor și a Volatilității în Timp")

# Crearea unei axe Y secundare pentru volatilitate
par(new = TRUE)
plot(data$Date, data$iv30, type = "l", col = "red",
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", axes = FALSE)

# Adăugarea unei axe Y secundare pe partea dreaptă
axis(4, at = pretty(range(data$iv30)))
mtext("Volatilitatea Implicită (30 zile)", side = 4, line = 3)

# Adăugarea unei legende pentru a distinge liniile
legend("topright", legend = c("Prețul Acțiunilor", "Volatilitatea Implicită (30 zile)"),
       col = c("blue", "red"), lty = 1, cex = 0.8)
