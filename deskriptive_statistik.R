source('load_data.R')
library(ggplot2)

# ---------------------------------------------------------------------------------------------------
# Alter
# ---------------------------------------------------------------------------------------------------
data$alter_gruppe <- rep(0, nrow(data))
data[data$alter<20, ]$alter_gruppe <- 1
data[data$alter>=20 & data$alter<35, ]$alter_gruppe = 2
data[data$alter>=35, ]$alter_gruppe = 3

pdf("plots/01_alter.pdf")
p <- ggplot(data, aes(AUFNDATUM.YEAR, fill=factor(alter_gruppe))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("<20", "20-35", ">35")) +
    labs(x = "Jahr", y = "Anzahl")
dev.off()

# ---------------------------------------------------------------------------------------------------
# BMI
# ---------------------------------------------------------------------------------------------------
pdf("plots/02_bmi.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), bmi)) + geom_boxplot()
p + labs(x = "Jahr", y = "BMI")
dev.off()

# ---------------------------------------------------------------------------------------------------
# Einling/Mehrling
# ---------------------------------------------------------------------------------------------------
data$mehrlinge <- rep(FALSE, nrow(data))
data[data$ANZMEHRLINGE>1, ]$mehrlinge <- TRUE
pdf("plots/03_einling_mehrling.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), fill=factor(mehrlinge))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Einling", "Mehrlinge")) +
  labs(x = "Jahr", y = "Anzahl")
dev.off()

# ---------------------------------------------------------------------------------------------------
# Erstgebärende/Mehrgebärende
# ---------------------------------------------------------------------------------------------------
data$geburten <- (data$ANZSSVORHLG + data$ANZSSVORHTG)
data$geburten[is.na(data$geburten)] <- 0
data$mehrgebearende <- rep(FALSE, nrow(data))
data[data$geburten > 0, ]$mehrgebearende <- TRUE
pdf("plots/04_erst_mehrgeb.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), fill=factor(mehrgebearende))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Erstgeb.", "Mehrgeb.")) +
  labs(x = "Jahr", y = "Anzahl")
dev.off()

# ---------------------------------------------------------------------------------------------------
# Zeitentwicklung der Subgruppen
# ---------------------------------------------------------------------------------------------------
get_count <- function(group){
  # Gruppiert die Fallzahlen nach Jahren
  group.yearly <- setNames(aggregate(group$AUFNDATUM.YEAR, list(group$AUFNDATUM.YEAR), FUN=length), c("Year", "Count"))
  # Macht aus den Jahren ein Datumstempel
  group.yearly$Year <- as.Date(as.character(group.yearly$Year),format="%Y")

  return(group.yearly)
}

data.yearly <- get_count(data)
pdf("plots/trend_data.pdf")
p <- ggplot(data.yearly, aes(x=Year, y=100*Count/nrow(data)))
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group01.yearly <- get_count(group01)
pdf("plots/trend_group01.pdf")
p <- ggplot(group01.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group02.yearly <- get_count(group02)
pdf("plots/trend_group02.pdf")
p <- ggplot(group02.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group03.yearly <- get_count(group03)
pdf("plots/trend_group03.pdf")
p <- ggplot(group03.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group04.yearly <- get_count(group04)
pdf("plots/trend_group04.pdf")
p <- ggplot(group04.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group05.yearly <- get_count(group05)
pdf("plots/trend_group05.pdf")
p <- ggplot(group05.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group06.yearly <- get_count(group06)
pdf("plots/trend_group06.pdf")
p <- ggplot(group06.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group07.yearly <- get_count(group07)
pdf("plots/trend_group07.pdf")
p <- ggplot(group07.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()

group08.yearly <- get_count(group08)
pdf("plots/trend_group08.pdf")
p <- ggplot(group08.yearly, aes(x=Year, y=100*Count/nrow(data))) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl (%)")
dev.off()
