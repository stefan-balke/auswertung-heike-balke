source('load_data.R')
library(ggplot2)
library(ggthemes)

print_stats_metrical <- function(my_var){
  print(sprintf("Stichprobenumfang: %f", length(my_var)))
  print(sprintf("Mittelwert: %.02f", mean(my_var)))
  print(sprintf("Median: %.02f", median(my_var)))
  print(sprintf("Modalwert: %.02f", which.max(table(my_var))))
  print(sprintf("Min: %.02f, Max: %.02f", min(my_var), max(my_var)))
  print(sprintf("Quantile:"))
  print(quantile(my_var))
  print(sprintf("Varianz: %.02f", var(my_var)))
  print(sprintf("Standardabw.: %.02f", sd(my_var)))
}

print("Alter ---------------------------------")
print_stats_metrical(data$alter)
print("BMI ---------------------------------")
print_stats_metrical(data$bmi)
print("Koerpergroesse ---------------------------------")
print_stats_metrical(data$LAENGE)
print("Tragzeit ---------------------------------")
print_stats_metrical(data$TRAGZEITKLIN[data$TRAGZEITKLIN>=10 & data$TRAGZEITKLIN<=50 &!is.na(data$TRAGZEITKLIN)])


# ---------------------------------------------------------------------------------------------------
# Alter
# ---------------------------------------------------------------------------------------------------
data$alter_gruppe <- rep(0, nrow(data))
data[data$alter<20, ]$alter_gruppe <- 0
data[data$alter>=20 & data$alter<35, ]$alter_gruppe = 1
data[data$alter>=35, ]$alter_gruppe = 2

pdf("plots/01_alter_komplett.pdf")
p <- ggplot(data, aes(x=0, y=alter), xlab=NA) + labs(x = "", y = "Alter")
p + geom_boxplot(width=0.5) + theme_hc() +
  scale_x_continuous(limits=c(-0.5, 0.5)) +
  scale_y_continuous(limits=c(0, 60), breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size=20))
dev.off()

pdf("plots/01_alter_gruppen.pdf")
p <- ggplot(data, aes(AUFNDATUM.YEAR, fill=factor(alter_gruppe))) + geom_bar(position="dodge")
p + scale_fill_discrete(name="Gruppen",
                        labels=c("<20", "20-35", ">35")) +
    labs(x = "Jahr", y = "Anzahl")
dev.off()

# ---------------------------------------------------------------------------------------------------
# BMI
# ---------------------------------------------------------------------------------------------------
data$bmi_gruppe <- rep(0, nrow(data))
data[data$bmi<17.5 & !is.na(data$bmi), ]$bmi_gruppe <- 0
data[data$bmi>=17.5 & data$bmi<24 & !is.na(data$bmi), ]$bmi_gruppe = 1
data[data$bmi>=24 & data$bmi<33.9 & !is.na(data$bmi), ]$bmi_gruppe = 2
data[data$bmi>=34 & !is.na(data$bmi), ]$bmi_gruppe = 3

pdf("plots/01_bmi_komplett.pdf")
p <- ggplot(data, aes(x=0, y=bmi), xlab=NA) + labs(x = "", y = "BMI")
p + geom_boxplot(width=0.5) + theme_hc() +
  scale_x_continuous(limits=c(-0.5, 0.5)) +
  scale_y_continuous(limits=c(0, 80), breaks=c(10, 20, 30, 40, 50, 60, 70, 80)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size=20))
dev.off()

pdf("plots/02_bmi_boxplot.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), bmi)) + geom_boxplot()
p + labs(x = "Jahr", y = "BMI")
dev.off()

pdf("plots/02_bmi_gruppen.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), fill=factor(bmi_gruppe))) +
  geom_bar(position=position_dodge(width = .8), width = 0.7)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("<17.5", "17.5-23.9", "24.0-33.9", ">34")) +
  labs(x = "Jahr", y = "Anzahl")
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
