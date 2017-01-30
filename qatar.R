setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

##TIMELINE by DATABASE/SEIZURELOC##

library(dplyr)
library(ggplot2)

full <- read.csv("transittots_ratio.csv")

smug <- subset(full[1:776,])

oc1 <- data.frame(subset(smug, Origin.Country1.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
oc2 <- data.frame(subset(smug, Origin.Country2.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
oc3 <- data.frame(subset(smug, Origin.Country3.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
origin <- rbind(oc1,oc2,oc3)
origin$SC <- "origin"

tc1 <- data.frame(subset(smug, Transit.Country1.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
tc2 <- data.frame(subset(smug, Transit.Country2.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
tc3 <- data.frame(subset(smug, Transit.Country4.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
tc4 <- data.frame(subset(smug, Transit.Country4.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
trans <- rbind(tc1,tc2,tc3,tc4)
trans$SC <- "transit"

dest1 <- data.frame(subset(smug, Destination.Country1.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
dest2 <- data.frame(subset(smug, Destination.Country2.1 == "Qatar", select = c(Seizure.Country.1, Date, Database)))
dest <- rbind(dest1,dest2)
dest$SC <- "destination"

qatar <- rbind(origin, trans, dest)
qatar$seiz <- qatar$Seizure.Country.1 == "Qatar"
qatar$Date <- as.Date(qatar$Date)
qatar <- transform(qatar, SC = reorder(SC))
qatar$yref <- ifelse(qatar$Database == "Reptiles", 1, ifelse(qatar$Database == "Birds", 2, ifelse(qatar$Database == "Rhino Horn", 3, 4)))
qatar$shaperef <- ifelse(qatar$SC == "origin", 21, ifelse(qatar$SC == "transit", 22, 23))

png(file = "qatar_timeline.png", units="in", width=11, height=8.5, res=300)

ggplot(data = qatar, aes(x = Date, y = yref, shape = SC, fill = seiz)) + 
  geom_point(size = 10, alpha = .7) + 
  scale_shape_manual(name = "Supply Chain",
                     breaks = c("origin", "transit", "destination"),
                     labels = c("Origin", "Transit", "Destination"),
                     values = c(21, 22, 24),
                     guide = guide_legend(override.aes = list(color = 1))) +
  scale_fill_manual(name = "Seizure Location",
                    labels = c("Seized Elsewhere", "Seized in Dubai"),
                    values = c("darkred", "darkgreen"),
                    guide = guide_legend(override.aes = list(shape = 23))) +
  scale_y_continuous(breaks = c(1:4), labels = c("Reptiles", "Birds", "Rhino Horn", "Ivory")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank())

dev.off()