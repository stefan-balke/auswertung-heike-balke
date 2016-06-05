## Laden der Daten und Pakete, eventuell Formatierung der Spalten
library(Hmisc)
data <- spss.get("Testdatei_NPE.sav", use.value.labels=TRUE)

# Aufnahmedatum splitten
data$AUFNDATUM <- as.Date(data$AUFNDATUM, "%d.%m.%y")
data$AUFNDATUM.YEAR <- as.numeric(format(data$AUFNDATUM,"%Y"))
data$AUFNDATUM.MONTH <- format(data$AUFNDATUM,"%m")

# Alter berechnen
data$alter <- (as.numeric(data$AUFNDATUM.YEAR) - data$GEBJAHR)

# BMI berechnen
data$bmi <- data$KGERSTUNT / (data$LAENGE/100)^2

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
# hypertonisch erkrankte Frauen binär kodiert
# --------------------------------------------------------------------
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