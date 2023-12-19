library(ggplot2)
library(readr)
library(tidyr)

# Citirea și pregătirea datelor
datele <- read_csv("ORATS-OPT.csv")
datele$Date <- as.Date(datele$Date, format="%m/%d/%Y")
datele_lung <- gather(datele, key = "Volatilitate", value = "Valoare", iv30, iv60, iv90)

# Crearea graficului
ggplot(datele_lung, aes(x=Date, y=Valoare, color=Volatilitate)) +
  geom_line() +
  labs(title="Compararea volatilității implicite (IV) pe 30, 60 și 90 de zile",
       x="Data", y="Volatilitate implicită (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(readr)
library(tidyr)

# Citirea și pregătirea datelor
datele <- read_csv("ORATS-OPT.csv")
datele_lung <- gather(datele, key = "Volatilitate", value = "Valoare", iv30, iv60, iv90)

# Crearea graficului boxplot
ggplot(datele_lung, aes(x=Volatilitate, y=Valoare)) +
  geom_boxplot() +
  labs(title="Distribuția volatilității implicite (IV) pe 30, 60 și 90 de zile",
       x="Perioada de timp", y="Volatilitate implicită (%)") +
  theme_minimal()
