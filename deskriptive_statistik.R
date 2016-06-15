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

group_data <- table(data$alter_gruppe, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(data, aes(x=0, y=alter), xlab=NA) + labs(x = "", y = "Alter")
p + geom_boxplot(width=0.5) + theme_hc() +
  scale_x_continuous(limits=c(-0.5, 0.5)) +
  scale_y_continuous(limits=c(0, 60), breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size=20))
ggsave("plots/01_alter_komplett.pdf")

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 60000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Altersgruppen",
                        labels=c("<20", "20-35", ">35")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/01_alter_gruppen.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# BMI
# ---------------------------------------------------------------------------------------------------
data$bmi_gruppe <- rep(0, nrow(data))
data[data$bmi<17.5 & !is.na(data$bmi), ]$bmi_gruppe <- 0
data[data$bmi>=17.5 & data$bmi<24 & !is.na(data$bmi), ]$bmi_gruppe = 1
data[data$bmi>=24 & data$bmi<33.9 & !is.na(data$bmi), ]$bmi_gruppe = 2
data[data$bmi>=34 & !is.na(data$bmi), ]$bmi_gruppe = 3

p <- ggplot(data, aes(x=0, y=bmi), xlab=NA) + labs(x = "", y = "BMI")
p + geom_boxplot(width=0.5) + theme_hc() +
  scale_x_continuous(limits=c(-0.5, 0.5)) +
  scale_y_continuous(limits=c(0, 80), breaks=c(10, 20, 30, 40, 50, 60, 70, 80)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size=20))
ggsave("plots/02_bmi_komplett.pdf")

pdf("plots/02_bmi_boxplot.pdf")
p <- ggplot(data, aes(factor(AUFNDATUM.YEAR), bmi)) + geom_boxplot()
p + labs(x = "Jahr", y = "BMI")
ggsave("plots/02_bmi_boxplot.pdf")

group_data <- table(data$bmi_gruppe, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 37000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="BMI-Gruppen",
                        labels=c("<17.5", "17.5-23.9", "24.0-33.9", ">34")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/02_bmi_gruppen.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Einling/Mehrling
# ---------------------------------------------------------------------------------------------------
data$mehrlinge <- rep(FALSE, nrow(data))
data[data$ANZMEHRLINGE>1, ]$mehrlinge <- TRUE
group_data <- table(data$mehrlinge, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 70000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Einling", "Mehrlinge")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/03_einling_mehrling.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Erstgebärende/Mehrgebärende
# ---------------------------------------------------------------------------------------------------
data$geburten <- (data$ANZSSVORHLG + data$ANZSSVORHTG)
data$geburten[is.na(data$geburten)] <- 0
data$mehrgebearende <- rep(FALSE, nrow(data))
data[data$geburten > 0, ]$mehrgebearende <- TRUE

group_data <- table(data$mehrgebearende, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 37000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Erstgeb.", "Mehrgeb.")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/04_erst_mehrgeb.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Schwangeschaftswochen
# ---------------------------------------------------------------------------------------------------
data$ssw_gruppe <- rep(NA, nrow(data))
data[data$TRAGZEITKLIN>=10 & data$TRAGZEITKLIN<24 & !is.na(data$TRAGZEITKLIN), ]$ssw_gruppe = 0
data[data$TRAGZEITKLIN>=24 & data$TRAGZEITKLIN<=34 & !is.na(data$TRAGZEITKLIN), ]$ssw_gruppe = 1
data[data$TRAGZEITKLIN>34 & data$TRAGZEITKLIN<=50 & !is.na(data$TRAGZEITKLIN), ]$ssw_gruppe = 2

group_data <- table(data$ssw_gruppe, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 30000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="SSW-Gruppen",
                        labels=c("<24", "24-34", ">34.0", ">34")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/05_ssw_gruppen.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Herkunftsland
# ---------------------------------------------------------------------------------------------------
data$land <- rep(NA, nrow(data))
data[data$HERKUNFTDTLD==1 & !is.na(data$HERKUNFTDTLD), ]$land = 1
data[data$HERKUNFTAND==1 & !is.na(data$HERKUNFTAND), ]$land = 2
data[data$HERKUNFTAND==2 & !is.na(data$HERKUNFTAND), ]$land = 3
data[data$HERKUNFTAND==3 & !is.na(data$HERKUNFTAND), ]$land = 4
data[data$HERKUNFTAND==4 & !is.na(data$HERKUNFTAND), ]$land = 5
data[data$HERKUNFTAND==5 & !is.na(data$HERKUNFTAND), ]$land = 6
data[data$HERKUNFTAND==6 & !is.na(data$HERKUNFTAND), ]$land = 7

group_data <- table(data$land, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 60000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Deutschland", "Mittel- u. Nord-EU", "Mittelmeerl.", "Ost-EU", "Mittlerer O.", "Asien", "Sonst.")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/06_herkunftsland.pdf", width = 20, height = 20, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Entbindungsmodus
# ---------------------------------------------------------------------------------------------------
data$entmodus <- rep(NA, nrow(data))
data[grepl("9-260", data$ENTBINDMODUS) | grepl("9-261", data$ENTBINDMODUS) & !is.na(data$HERKUNFTDTLD), ]$entmodus = 1
data[grepl("5-740", data$ENTBINDMODUS) |
     grepl("5-740.0", data$ENTBINDMODUS) |
     grepl("5-740.1", data$ENTBINDMODUS) |
     grepl("5-740.y", data$ENTBINDMODUS) &
     !is.na(data$HERKUNFTDTLD), ]$entmodus = 2

group_data <- table(data$entmodus, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 50000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Vaginal", "Sectio")) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/07_entbindungsmodus.pdf", width = 20, height = 20, units = "cm")

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

# Gesamtgeburten pro Jahr
group_data <- table(data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") +  ylim(0, 70000) +
  geom_text(aes(x=Var1, label=Freq), size=3.5,
            position=position_dodge(width=1), vjust=-0.3) +
  labs(x = "Jahr", y = "Anzahl")
ggsave("plots/trend_data_complete.pdf", width = 14, height = 8, units = "cm")

# Tragzeit
group_data <- table(data[data$TRAGZEITKLIN>=10 & data$TRAGZEITKLIN<=50 &!is.na(data$TRAGZEITKLIN), ], data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
data[data$TRAGZEITKLIN>=10 & data$TRAGZEITKLIN<=50 &!is.na(data$TRAGZEITKLIN)]


group01.yearly <- get_count(group01)
pdf("plots/trend_group01.pdf")
p <- ggplot(group01.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group02.yearly <- get_count(group02)
pdf("plots/trend_group02.pdf")
p <- ggplot(group02.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group03.yearly <- get_count(group03)
pdf("plots/trend_group03.pdf")
p <- ggplot(group03.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group04.yearly <- get_count(group04)
pdf("plots/trend_group04.pdf")
p <- ggplot(group04.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group05.yearly <- get_count(group05)
pdf("plots/trend_group05.pdf")
p <- ggplot(group05.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group06.yearly <- get_count(group06)
pdf("plots/trend_group06.pdf")
p <- ggplot(group06.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group07.yearly <- get_count(group07)
pdf("plots/trend_group07.pdf")
p <- ggplot(group07.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()

group08.yearly <- get_count(group08)
pdf("plots/trend_group08.pdf")
p <- ggplot(group08.yearly, aes(x=Year, y=Count)) 
p + geom_line() + geom_smooth(method="lm", se=TRUE) + labs(x = "Jahr", y = "Anzahl")
dev.off()