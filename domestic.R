##HOW MANY DOMESTIC FLIGHTS ARE IN BIRD DB???##

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(ggplot2)
library(extrafont)

full <- read.csv("transittots.csv", header = T, stringsAsFactors = F)
bird <- subset(full, full$Database == "Birds")

bird <- subset(bird, select = c(Origin.Country1, Transit.Country1, Transit.Country2, Destination.Country1))
bird[bird == ""] <- NA

a <- data.frame(bird$Origin.Country1 == bird$Transit.Country1)
b <- data.frame(bird$Transit.Country1 == bird$Transit.Country2)
c <- data.frame(bird$Transit.Country2 == bird$Destination.Country1)
