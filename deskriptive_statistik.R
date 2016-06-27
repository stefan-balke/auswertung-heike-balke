# UTF-8 für Umlaute etc.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

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
print("SSW ---------------------------------")
print_stats_metrical(data$ssw_berechnet)


# ---------------------------------------------------------------------------------------------------
# Alter
# ---------------------------------------------------------------------------------------------------
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
        text = element_text(size=20)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("plots/01_alter_komplett.pdf")

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 60000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Altersgruppen",
                        labels=c("<20", "20-35", ">35")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/01_alter_gruppen.pdf", width = 20, height = 12, units = "cm")

# Alter Erstgebärende
group_data <- data[data$mehrgebearende==FALSE, ]
group_data <- table(group_data$alter_gruppe, group_data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 30000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Altersgruppen",
                        labels=c("<20", "20-35", ">35")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/01_alter_gruppen_erstgeb.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# BMI
# ---------------------------------------------------------------------------------------------------
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
  geom_bar(position="dodge", stat="identity") + ylim(0, 43000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="BMI-Gruppen",
                        labels=c("<18.5 = untergewichtig",
                                 "18.5-24.9 = normalgewichtig",
                                 "25.0-29.9 = übergewichtig",
                                 ">30 = adipös")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/02_bmi_gruppen.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Einling/Mehrling
# ---------------------------------------------------------------------------------------------------
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
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/03_einling_mehrling.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Erstgebärende/Mehrgebärende
# ---------------------------------------------------------------------------------------------------
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
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/04_erst_mehrgeb.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Schwangerschaftswochen
# ---------------------------------------------------------------------------------------------------
group_data <- table(data$ssw_gruppe, data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 70000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="SSW-Gruppen",
                        labels=c("<24", "24-34", ">34.0", ">34")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/05_ssw_gruppen.pdf", width = 20, height = 12, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Herkunftsland
# ---------------------------------------------------------------------------------------------------
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
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/06_herkunftsland.pdf", width = 20, height = 20, units = "cm")

# ---------------------------------------------------------------------------------------------------
# Entbindungsmodus
# ---------------------------------------------------------------------------------------------------
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
  guides(fill = guide_legend(reverse = TRUE)) +
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

# Gruppe 1
group_data <- table(group01$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 100) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group01.pdf", width = 14, height = 8, units = "cm")

# Gruppe 2
group_data <- table(group02$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 3000) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group02.pdf", width = 14, height = 8, units = "cm")

# Gruppe 3
group_data <- table(group03$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 650) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group03.pdf", width = 14, height = 8, units = "cm")

# Gruppe 4
print("Alter ---------------------------------")
print_stats_metrical(group04$alter)
print("BMI ---------------------------------")
print_stats_metrical(group04$bmi)
print("SSW ---------------------------------")
print_stats_metrical(group04$ssw_berechnet)

print(sprintf("PE-Ekrankte 2001-2009: %.02f", 100*(nrow(group04) / nrow(data))))
group_data <- table(group04$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 1300) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04.pdf", width = 14, height = 8, units = "cm")

# Erst- oder Mehrgebärende
group_data <- table(group04$mehrgebearende, group04$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 800) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Erstgeb.", "Mehrgeb.")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04_erst_mehrgeb.pdf", width = 20, height = 12, units = "cm")

# Alter Erstgebärende
group_data <- group04[group04$mehrgebearende==FALSE, ]
group_data <- table(group_data$alter_gruppe, group_data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 700) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Altersgruppen",
                        labels=c("<20", "20-35", ">35")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04_erstgeb.pdf", width = 20, height = 12, units = "cm")

# Einling Mehrling
group_data <- table(group04$mehrlinge, group04$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 1100) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Einling", "Mehrlinge")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04_einling_mehrling.pdf", width = 20, height = 12, units = "cm")

# SSW
group_data <- table(group04$ssw_gruppe, group04$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 1100) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="SSW-Gruppen",
                        labels=c("<24", "24-34", ">34.0", ">34")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04_yearly.pdf", width = 20, height = 12, units = "cm")

# BMI
group_data <- table(group04$bmi_gruppe, group04$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 500) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="BMI-Gruppen",
                        labels=c("<18.5 = untergewichtig",
                                 "18.5-24.9 = normalgewichtig",
                                 "25.0-29.9 = übergewichtig",
                                 ">30 = adipös")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group04_yearly_bmi.pdf", width = 20, height = 12, units = "cm")


# Gruppe 5
group_data <- table(group05$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 70) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group05.pdf", width = 14, height = 8, units = "cm")

# Erst- oder Mehrgebärende
group_data <- table(group05$mehrgebearende, group05$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 40) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Gruppen",
                        labels=c("Erstgeb.", "Mehrgeb.")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group05_erst_mehrgeb.pdf", width = 20, height = 12, units = "cm")

# Alter Erstgebärende
group_data <- group05[group05$mehrgebearende==FALSE, ]
group_data <- table(group_data$alter_gruppe, group_data$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 40) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="Altersgruppen",
                        labels=c("<20", "20-35", ">35")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group05_erstgeb.pdf", width = 20, height = 12, units = "cm")

# Gruppe 6
group_data <- table(group06$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 310) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group06.pdf", width = 14, height = 8, units = "cm")

# Gruppe 7
group_data <- table(group07$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 250) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group07.pdf", width = 14, height = 8, units = "cm")

# Gruppe 8
group_data <- table(group08$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
group_data$scaler <- table(data$AUFNDATUM.YEAR)

p <- ggplot(group_data, aes(x=Var1, y=Freq)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue") + ylim(0, 30) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var1, label=paste0(round(Freq/scaler*100,2),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group08.pdf", width = 14, height = 8, units = "cm")

# Group 4 + Group 5 in SSW Gruppen
melt_group <- rbind(group04, group05) 

# SSW
group_data <- table(melt_group$ssw_gruppe, melt_group$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 1100) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="SSW-Gruppen",
                        labels=c("<24", "24-34", ">34.0", ">34")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group0405_yearly.pdf", width = 20, height = 12, units = "cm")

# BMI
group_data <- table(melt_group$bmi_gruppe, melt_group$AUFNDATUM.YEAR)
group_data <- data.frame(group_data)
aggregation <- aggregate(Freq ~ Var2, group_data, sum)
group_data$scaler <- rep(aggregation$Freq, each=nlevels(group_data$Var1))

p <- ggplot(group_data, aes(x=Var2, y=Freq, fill=factor(Var1))) +
  geom_bar(position="dodge", stat="identity") + ylim(0, 500) +
  geom_text(aes(y=0, label=Freq), size=2,
            position=position_dodge(0.9), hjust=1) + 
  geom_text(aes(x=Var2, label=paste0(round(Freq/scaler*100,1),"%")), size=3,
            position=position_dodge(0.9), hjust=-0.1)
p + scale_fill_discrete(name="BMI-Gruppen",
                        labels=c("<18.5 = untergewichtig",
                                 "18.5-24.9 = normalgewichtig",
                                 "25.0-29.9 = übergewichtig",
                                 ">30 = adipös")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Jahr", y = "Anzahl") + coord_flip()
ggsave("plots/trend_group0405_yearly_bmi.pdf", width = 20, height = 12, units = "cm")

