## Laden der Daten und Pakete, eventuell Formatierung der Spalten
library(Hmisc)
data <- spss.get("PE_2001-2009.sav", use.value.labels=TRUE)

# Aufnahmedatum splitten
data$AUFNDATUM <- as.Date(data$AUFNDATUM, "%d.%m.%y")
data$AUFNDATUM.YEAR <- as.numeric(format(data$AUFNDATUM,"%Y"))
data$AUFNDATUM.MONTH <- format(data$AUFNDATUM,"%m")

# Geburtsdatum Kind und errechneter Termin splitten
data$GEBDATUMK <- as.Date(data$GEBDATUMK, "%d.%m.%y")
data$GEBDATUMK.YEAR <- as.numeric(format(data$GEBDATUMK,"%Y"))

data$GEBTERMIN <- as.Date(data$GEBTERMIN, "%d.%m.%y")
data$GEBTERMIN.YEAR <- as.numeric(format(data$GEBTERMIN,"%Y"))

# --------------------------------------------------------------------
# Datensatz aufräumen
# --------------------------------------------------------------------
print(nrow(data))

# Schritt 1: AUFNDATUM.YEAR darf nur in [2001,2009] liegen
data <- data[!is.na(data$LAENGE) & data$AUFNDATUM.YEAR >= 2001 & data$AUFNDATUM.YEAR <= 2009, ]
print(sprintf("Nach Cleaning Schritt 1: %d", nrow(data)))

# Schritt 2: LAENGE darf nur in [100, 250] liegen und muss eingetragen sein
data <- data[!is.na(data$LAENGE) & data$LAENGE >= 130 & data$LAENGE <= 250, ]
print(sprintf("Nach Cleaning Schritt 2: %d", nrow(data)))

# Schritt 3: GEBJAHR der Mutter muss eingetragen sein
data <- data[!is.na(data$GEBJAHR), ]
print(sprintf("Nach Cleaning Schritt 3: %d", nrow(data)))

# Alter berechnen
data$alter <- (as.numeric(data$AUFNDATUM.YEAR) - data$GEBJAHR)

# Schritt 4: Alter muss größer als 14 sein
data <- data[data$alter>=10, ]
print(sprintf("Nach Cleaning Schritt 4: %d", nrow(data)))

# Schritt 5: Gewicht bei Erstuntersuchung muss in [40, 250] liegen und gesetzt sein
data <- data[data$KGERSTUNT>40 & data$KGERSTUNT<250 & !is.na(data$KGERSTUNT), ]
print(sprintf("Nach Cleaning Schritt 5: %d", nrow(data)))

# BMI berechnen
data$bmi <- as.numeric(data$KGERSTUNT) / ((as.numeric(data$LAENGE)/100)^2)

# Schritt 6: GEBTERMIN und GEBDATUMK müssen in [2000, 2010] liegen
#            SSW_berechnet muss in [12:46] liegen
#            (manchmal ist das Jahr falsch gesetzt und dann geht die Berechnung kaputt)
data <- data[!is.na(data$GEBTERMIN) & !is.na(data$GEBDATUMK), ]
data <- data[data$GEBDATUMK.YEAR > 2000 & data$GEBDATUMK.YEAR <= 2010, ]
data <- data[data$GEBTERMIN.YEAR > 2000 & data$GEBTERMIN.YEAR <= 2010, ]
data$ssw_berechnet <- floor((280 - (data$GEBTERMIN - data$GEBDATUMK)) / 7) + 1
data <- data[data$ssw_berechnet >= 12 & data$ssw_berechnet <= 46, ]
print(sprintf("Nach Cleaning Schritt 6: %d", nrow(data)))

# --------------------------------------------------------------------
# Zusätzliche Variablen
# --------------------------------------------------------------------
# Berechnete SSW + SSW Gruppen
data$ssw_gruppe <- rep(NA, nrow(data))
data[data$ssw_berechnet>=10 & data$ssw_berechnet<24 & !is.na(data$ssw_berechnet), ]$ssw_gruppe = 0
data[data$ssw_berechnet>=24 & data$ssw_berechnet<=34 & !is.na(data$ssw_berechnet), ]$ssw_gruppe = 1
data[data$ssw_berechnet>34 & data$ssw_berechnet<=50 & !is.na(data$ssw_berechnet), ]$ssw_gruppe = 2

# Herkunftsland
data$land <- rep(NA, nrow(data))
data[data$HERKUNFTDTLD==1 & !is.na(data$HERKUNFTDTLD), ]$land = 1
data[data$HERKUNFTAND==1 & !is.na(data$HERKUNFTAND), ]$land = 2
data[data$HERKUNFTAND==2 & !is.na(data$HERKUNFTAND), ]$land = 3
data[data$HERKUNFTAND==3 & !is.na(data$HERKUNFTAND), ]$land = 4
data[data$HERKUNFTAND==4 & !is.na(data$HERKUNFTAND), ]$land = 5
data[data$HERKUNFTAND==5 & !is.na(data$HERKUNFTAND), ]$land = 6
data[data$HERKUNFTAND==6 & !is.na(data$HERKUNFTAND), ]$land = 7

# Entbindungsmodus
# 1=spontan, 2=sectio
data$entmodus <- rep(NA, nrow(data))
data[grepl("9-260", data$ENTBINDMODUS) | grepl("9-261", data$ENTBINDMODUS) & !is.na(data$ENTBINDMODUS), ]$entmodus = 1
data[grepl("5-740", data$ENTBINDMODUS) |
       grepl("5-740.0", data$ENTBINDMODUS) |
       grepl("5-740.1", data$ENTBINDMODUS) |
       grepl("5-740.y", data$ENTBINDMODUS) &
       !is.na(data$ENTBINDMODUS), ]$entmodus = 2

# Erst- oder Mehrgebärende
data$geburten <- rep(NA, nrow(data))
data$geburten <- (data$ANZSSVORHLG + data$ANZSSVORHTG)
data$geburten[is.na(data$geburten)] <- 0
data$mehrgebearende <- rep(FALSE, nrow(data))
data[data$geburten > 0, ]$mehrgebearende <- TRUE

# Mehrlingsgeburt
data$mehrlinge <- rep(FALSE, nrow(data))
data[data$ANZMEHRLINGE>1, ]$mehrlinge <- TRUE

# BMI Gruppen
data$bmi_gruppe <- rep(0, nrow(data))
data[data$bmi<18.5 & !is.na(data$bmi), ]$bmi_gruppe <- 0
data[data$bmi>=18.5 & data$bmi<25 & !is.na(data$bmi), ]$bmi_gruppe = 1
data[data$bmi>=25 & data$bmi<30 & !is.na(data$bmi), ]$bmi_gruppe = 2
data[data$bmi>=30 & !is.na(data$bmi), ]$bmi_gruppe = 3

# Altersgruppen
data$alter_gruppe <- rep(0, nrow(data))
data[data$alter<20, ]$alter_gruppe <- 0
data[data$alter>=20 & data$alter<35, ]$alter_gruppe = 1
data[data$alter>=35, ]$alter_gruppe = 2

# --------------------------------------------------------------------
# Untergruppen
# --------------------------------------------------------------------
# Gruppe 1: Vorher bestehende Hypertonie
group01 <- subset(data, grepl("O10.-", data$AUFNDIAG.1) |
                    grepl("O10.-", data$AUFNDIAG2) | 
                    grepl("O10.0", data$AUFNDIAG.1) |
                    grepl("O10.0", data$AUFNDIAG2) |
                    grepl("O10.9", data$AUFNDIAG.1) |
                    grepl("O10.9", data$AUFNDIAG2))

# Gruppe 2: Hypertonie nicht näher definiert
group02 <- subset(data, grepl("O16", data$AUFNDIAG.1) | 
                    grepl("O16", data$AUFNDIAG2) |
                    grepl(46, data$STATAUFIND.1) |
                    grepl(46, data$STATAUFIND.2) |
                    grepl(46, data$SSRISIKO.1) |
                    grepl(46, data$SSRISIKO.2) |
                    grepl(46, data$SSRISIKO.3) |
                    grepl(46, data$SSRISIKO.4) |
                    grepl(46, data$SSRISIKO.5) |
                    grepl(46, data$SSRISIKO.6) |
                    grepl(46, data$SSRISIKO.7) |
                    grepl(46, data$SSRISIKO.8) |
                    grepl(46, data$SSRISIKO.9) |
                    grepl(2, data$DPPLSOVSIND.1) |
                    grepl(2, data$DPPLSOVSIND.2) |
                    grepl(2, data$DPPLSONOIND.1) |
                    grepl(2, data$DPPLSONOIND.2) |
                    grepl(2, data$DPPLSONOIND.3) |
                    grepl(2, data$DPPLSONOIND.4) |
                    grepl(66, data$GEBRISIKO.1) |
                    grepl(66, data$GEBRISIKO.2) |
                    grepl(66, data$GEBRISIKO.3) |
                    grepl(66, data$GEBRISIKO.4) |
                    grepl(66, data$GEBRISIKO.5) |
                    grepl(66, data$GEBRISIKO.6) |
                    grepl(66, data$GEBRISIKO.7) |
                    grepl(66, data$GEBRISIKO.8) |
                    grepl(66, data$GEBRISIKO.9) |
                    grepl(66, data$GEBRISIKO.10))

# Gruppe 3: Gastationshypertonie
group03 <- subset(data, grepl("O13", data$AUFNDIAG.1) |
                    grepl("O13", data$AUFNDIAG2))

# Gruppe 4: Präeklampsie
group04 <- subset(data, grepl("O14.-", data$AUFNDIAG.1) |
                    grepl("O14.-", data$AUFNDIAG2) | 
                    grepl("O14.0", data$AUFNDIAG.1) |
                    grepl("O14.0", data$AUFNDIAG2) |
                    grepl("O14.1", data$AUFNDIAG.1) |
                    grepl("O14.1", data$AUFNDIAG2) |
                    grepl("O14.9", data$AUFNDIAG.1) |
                    grepl("O14.9", data$AUFNDIAG2))

# Gruppe 5: Pfropfpräeklampsie
group05 <- subset(data, grepl("O11", data$AUFNDIAG.1) |
                    grepl("O11", data$AUFNDIAG2))

# Gruppe 6: HELLP-Syndropm
group06 <- subset(data, grepl("O14.2", data$AUFNDIAG.1) |
                    grepl("O14.2", data$AUFNDIAG2) |
                    grepl(95, data$GEBRISIKO.1) |
                    grepl(95, data$GEBRISIKO.2) |
                    grepl(95, data$GEBRISIKO.3) |
                    grepl(95, data$GEBRISIKO.4) |
                    grepl(95, data$GEBRISIKO.5) |
                    grepl(95, data$GEBRISIKO.6) |
                    grepl(95, data$GEBRISIKO.7) |
                    grepl(95, data$GEBRISIKO.8) |
                    grepl(95, data$GEBRISIKO.9) |
                    grepl(95, data$GEBRISIKO.10))

# Gruppe 7: z. N. hypertensiven Erkrankung
group07 <- subset(data, grepl(54, data$STATAUFIND.1) |
                    grepl(54, data$STATAUFIND.2) |
                    grepl(55, data$STATAUFIND.1) |
                    grepl(55, data$STATAUFIND.2) |
                    grepl(56, data$STATAUFIND.1) |
                    grepl(56, data$STATAUFIND.2) |
                    grepl(54, data$SSRISIKO.1) |
                    grepl(54, data$SSRISIKO.2) |
                    grepl(54, data$SSRISIKO.3) |
                    grepl(54, data$SSRISIKO.4) |
                    grepl(54, data$SSRISIKO.5) |
                    grepl(54, data$SSRISIKO.6) |
                    grepl(54, data$SSRISIKO.7) |
                    grepl(54, data$SSRISIKO.8) |
                    grepl(54, data$SSRISIKO.9) |
                    grepl(55, data$SSRISIKO.1) |
                    grepl(55, data$SSRISIKO.2) |
                    grepl(55, data$SSRISIKO.3) |
                    grepl(55, data$SSRISIKO.4) |
                    grepl(55, data$SSRISIKO.5) |
                    grepl(55, data$SSRISIKO.6) |
                    grepl(55, data$SSRISIKO.7) |
                    grepl(55, data$SSRISIKO.8) |
                    grepl(55, data$SSRISIKO.9) |
                    grepl(56, data$SSRISIKO.1) |
                    grepl(56, data$SSRISIKO.2) |
                    grepl(56, data$SSRISIKO.3) |
                    grepl(56, data$SSRISIKO.4) |
                    grepl(56, data$SSRISIKO.5) |
                    grepl(56, data$SSRISIKO.6) |
                    grepl(56, data$SSRISIKO.7) |
                    grepl(56, data$SSRISIKO.8) |
                    grepl(56, data$SSRISIKO.9) |
                    grepl(4, data$DPPLSOVSIND.1) |
                    grepl(4, data$DPPLSOVSIND.2) |
                    grepl(4, data$DPPLSONOIND.1) |
                    grepl(4, data$DPPLSONOIND.2) |
                    grepl(4, data$DPPLSONOIND.3) |
                    grepl(4, data$DPPLSONOIND.4))

# Gruppe 8: Eklampsie
group08 <- subset(data, grepl("O15", data$AUFNDIAG.1) |
                    grepl("O15", data$AUFNDIAG2) | 
                    grepl("O15.0", data$AUFNDIAG.1) |
                    grepl("O15.0", data$AUFNDIAG2))

# --------------------------------------------------------------------
# Hilfsvariablen
# Präeklampsie
# --------------------------------------------------------------------
data$PE <- ifelse(grepl("O14.-", data$AUFNDIAG.1) |
                  grepl("O14.-", data$AUFNDIAG2) | 
                  grepl("O14.0", data$AUFNDIAG.1) |
                  grepl("O14.0", data$AUFNDIAG2) |
                  grepl("O14.1", data$AUFNDIAG.1) |
                  grepl("O14.1", data$AUFNDIAG2) |
                  grepl("O14.9", data$AUFNDIAG.1) |
                  grepl("O14.9", data$AUFNDIAG2), TRUE, FALSE)

data$hypertonie <- ifelse(grepl("O16", data$AUFNDIAG.1) | 
                            grepl("O16", data$AUFNDIAG2) |
                            grepl(23, data$SSRISIKO.1) |
                            grepl(23, data$SSRISIKO.2) |
                            grepl(23, data$SSRISIKO.3) |
                            grepl(23, data$SSRISIKO.4) |
                            grepl(23, data$SSRISIKO.5) |
                            grepl(23, data$SSRISIKO.6) |
                            grepl(23, data$SSRISIKO.7) |
                            grepl(23, data$SSRISIKO.8) |
                            grepl(23, data$SSRISIKO.9) |
                            grepl(2, data$DPPLSOVSIND.1) |
                            grepl(2, data$DPPLSOVSIND.2) |
                            grepl(66, data$GEBRISIKO.1) |
                            grepl(66, data$GEBRISIKO.2) |
                            grepl(66, data$GEBRISIKO.3) |
                            grepl(66, data$GEBRISIKO.4) |
                            grepl(66, data$GEBRISIKO.5) |
                            grepl(66, data$GEBRISIKO.6) |
                            grepl(66, data$GEBRISIKO.7) |
                            grepl(66, data$GEBRISIKO.8) |
                            grepl(66, data$GEBRISIKO.9) |
                            grepl(66, data$GEBRISIKO.10), TRUE, FALSE)

# Zustand nach hypertonischer Erkrankung binär kodiert
data$znhypertonie <- ifelse(grepl(54, data$STATAUFIND.1) |
                              grepl(54, data$STATAUFIND.2) |
                              grepl(55, data$STATAUFIND.1) |
                              grepl(55, data$STATAUFIND.2) |
                              grepl(56, data$STATAUFIND.1) |
                              grepl(56, data$STATAUFIND.2) |
                              grepl(54, data$SSRISIKO.1) |
                              grepl(54, data$SSRISIKO.2) |
                              grepl(54, data$SSRISIKO.3) |
                              grepl(54, data$SSRISIKO.4) |
                              grepl(54, data$SSRISIKO.5) |
                              grepl(54, data$SSRISIKO.6) |
                              grepl(54, data$SSRISIKO.7) |
                              grepl(54, data$SSRISIKO.8) |
                              grepl(54, data$SSRISIKO.9) |
                              grepl(55, data$SSRISIKO.1) |
                              grepl(55, data$SSRISIKO.2) |
                              grepl(55, data$SSRISIKO.3) |
                              grepl(55, data$SSRISIKO.4) |
                              grepl(55, data$SSRISIKO.5) |
                              grepl(55, data$SSRISIKO.6) |
                              grepl(55, data$SSRISIKO.7) |
                              grepl(55, data$SSRISIKO.8) |
                              grepl(55, data$SSRISIKO.9) |
                              grepl(56, data$SSRISIKO.1) |
                              grepl(56, data$SSRISIKO.2) |
                              grepl(56, data$SSRISIKO.3) |
                              grepl(56, data$SSRISIKO.4) |
                              grepl(56, data$SSRISIKO.5) |
                              grepl(56, data$SSRISIKO.6) |
                              grepl(56, data$SSRISIKO.7) |
                              grepl(56, data$SSRISIKO.8) |
                              grepl(56, data$SSRISIKO.9) |
                              grepl(4, data$DPPLSOVSIND.1) |
                              grepl(4, data$DPPLSOVSIND.2) |
                              grepl(4, data$DPPLSONOIND.1) |
                              grepl(4, data$DPPLSONOIND.2) |
                              grepl(4, data$DPPLSONOIND.3) |
                              grepl(4, data$DPPLSONOIND.4), TRUE, FALSE)

group01 <- subset(data, grepl("O10.-", data$AUFNDIAG.1) |
                    grepl("O10.-", data$AUFNDIAG2) | 
                    grepl("O10.0", data$AUFNDIAG.1) |
                    grepl("O10.0", data$AUFNDIAG2) |
                    grepl("O10.9", data$AUFNDIAG.1) |
                    grepl("O10.9", data$AUFNDIAG2))

# Vorher bestehende Hypertonie + Hypertonie nicht näher definiert
data$hyper_bestehend <- ifelse(grepl("O10.-", data$AUFNDIAG.1) |
                                 grepl("O10.-", data$AUFNDIAG2) | 
                                 grepl("O10.0", data$AUFNDIAG.1) |
                                 grepl("O10.0", data$AUFNDIAG2) |
                                 grepl("O10.9", data$AUFNDIAG.1) |
                                 grepl("O10.9", data$AUFNDIAG2) |
                               grepl("O16", data$AUFNDIAG.1) | 
                    grepl("O16", data$AUFNDIAG2) |
                    grepl(46, data$STATAUFIND.1) |
                    grepl(46, data$STATAUFIND.2) |
                    grepl(46, data$SSRISIKO.1) |
                    grepl(46, data$SSRISIKO.2) |
                    grepl(46, data$SSRISIKO.3) |
                    grepl(46, data$SSRISIKO.4) |
                    grepl(46, data$SSRISIKO.5) |
                    grepl(46, data$SSRISIKO.6) |
                    grepl(46, data$SSRISIKO.7) |
                    grepl(46, data$SSRISIKO.8) |
                    grepl(46, data$SSRISIKO.9) |
                    grepl(2, data$DPPLSOVSIND.1) |
                    grepl(2, data$DPPLSOVSIND.2) |
                    grepl(2, data$DPPLSONOIND.1) |
                    grepl(2, data$DPPLSONOIND.2) |
                    grepl(2, data$DPPLSONOIND.3) |
                    grepl(2, data$DPPLSONOIND.4) |
                    grepl(66, data$GEBRISIKO.1) |
                    grepl(66, data$GEBRISIKO.2) |
                    grepl(66, data$GEBRISIKO.3) |
                    grepl(66, data$GEBRISIKO.4) |
                    grepl(66, data$GEBRISIKO.5) |
                    grepl(66, data$GEBRISIKO.6) |
                    grepl(66, data$GEBRISIKO.7) |
                    grepl(66, data$GEBRISIKO.8) |
                    grepl(66, data$GEBRISIKO.9) |
                    grepl(66, data$GEBRISIKO.10), TRUE, FALSE)