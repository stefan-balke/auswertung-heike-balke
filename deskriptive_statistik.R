source('load_data.R')
library(ggplot2)

# ---------------------------------
# Alter
# ---------------------------------
data$alter <- (as.numeric(data$AUFNDATUM.YEAR) - data$GEBJAHR)
data$alter_gruppe <- rep(0, nrow(data))
data[data$alter<20, ]$alter_gruppe <- 1
data[data$alter>=20 & data$alter<35, ]$alter_gruppe = 2
data[data$alter>=35, ]$alter_gruppe = 3
# data[10, ]$alter_gruppe <- 1

p <- ggplot(data, aes(AUFNDATUM.YEAR, fill=factor(alter_gruppe))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("<20", "20-35", ">35")) +
    labs(x = "Jahr", y = "Anzahl")

# ---------------------------------
# BMI
# ---------------------------------
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), bmi)) + geom_boxplot()
p + labs(x = "Jahr", y = "BMI")

# ---------------------------------
# Einling/Mehrling
# ---------------------------------
data$mehrlinge <- rep(FALSE, nrow(data))
data[data$ANZMEHRLINGE>1, ]$mehrlinge <- TRUE
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), fill=factor(mehrlinge))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Einling", "Mehrlinge")) +
  labs(x = "Jahr", y = "Anzahl")

# ---------------------------------
# Erstgebärende/Mehrgebärende
# ---------------------------------
data$geburten <- (data$ANZSSVORHLG + data$ANZSSVORHTG)
data$geburten[is.na(data$geburten)] <- 0
data$mehrgebearende <- rep(FALSE, nrow(data))
data[data$geburten > 0, ]$mehrgebearende <- TRUE
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), fill=factor(mehrgebearende))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Erstgeb.", "Mehrgeb.")) +
  labs(x = "Jahr", y = "Anzahl")