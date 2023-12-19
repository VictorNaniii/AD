# Încărcarea pachetelor necesare
library(ggplot2)
library(dplyr)

# Citirea datelor
data <- read.csv("D:/UTM AN3/SEM 1/AD/RAPID PENTRU ATESTARE FACEM DUPA SCHIMB/Modelele/ORATS-OPT.csv")

# Calculul statisticilor descriptive
mean_stockpx <- mean(data$stockpx)
median_stockpx <- median(data$stockpx)
mode_stockpx <- as.numeric(names(sort(table(data$stockpx), decreasing = TRUE)[1]))
std_dev_stockpx <- sd(data$stockpx)
variance_stockpx <- var(data$stockpx)
range_stockpx <- max(data$stockpx) - min(data$stockpx)
iqr_stockpx <- IQR(data$stockpx)

# Afișarea rezultatelor
print(paste("Mean:", mean_stockpx))
print(paste("Median:", median_stockpx))
print(paste("Mode:", mode_stockpx))
print(paste("Standard Deviation:", std_dev_stockpx))
print(paste("Variance:", variance_stockpx))
print(paste("Range:", range_stockpx))
print(paste("IQR:", iqr_stockpx))

# Crearea unui histogram
ggplot(data, aes(x = stockpx)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Stock Prices", x = "Stock Price", y = "Frequency")

# Crearea unui boxplot
ggplot(data, aes(y = stockpx)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Stock Prices", y = "Stock Price")
