# Încărcarea pachetelor necesare
library(ggplot2)
library(readr)

# Citirea datelor
datele <- read_csv("ORATS-OPT.csv")

# Conversia coloanei 'Date' în formatul de dată
datele$Date <- as.Date(datele$Date, format="%m/%d/%Y")

# Crearea unui grafic liniar fără puncte
ggplot(datele, aes(x=Date, y=stockpx)) +
  geom_line() + # adaugă doar o linie
  labs(title="Evoluția prețului acțiunilor Microsoft (MSFT) în timp",
       x="Data",
       y="Prețul acțiunilor") +
  theme_minimal() + # folosește o temă minimalistă
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # rotește textul axei x
