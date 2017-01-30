setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(dplyr)
library(ggplot2)
library(extrafont)

full <- read.csv("transittots_ratio.csv", header = T)
subset <- subset(full[1:773,])

oc1 <- dplyr :: count(subset, Origin.Country1.1)
names(oc1) <- c("country", "n1")
oc2 <- dplyr :: count(subset, Origin.Country2.1)
names(oc2) <- c("country", "n2")
oc3 <- dplyr :: count(subset, Origin.Country3.1)
names(oc3) <- c("country", "n3")

origin <- merge(merge(x = oc1, y = oc2, by = "country", all = T), oc3, by = "country", all = T)
origin <- origin[origin$country!="SEIZURE" & origin$country!="" & origin$country!=0 & origin$country!="#N/A",]
origin$origin <- rowSums(origin[,2:4], na.rm = T)
origin <- data.frame(subset(origin, select = c("country", "origin")))

tc1 <- dplyr :: count(subset, Transit.Country1.1)
names(tc1) <- c("country", "n1")
tc2 <- dplyr :: count(subset, Transit.Country2.1)
names(tc2) <- c("country", "n2")
tc3 <- dplyr :: count(subset, Transit.Country3.1)
names(tc3) <- c("country", "n3")
tc4 <- dplyr :: count(subset, Transit.Country4.1)
names(tc4) <- c("country", "n4")

transit <- merge(merge(merge(tc1, tc2, by = "country", all = T), tc3, by = "country", all = T), tc4, by = "country", all = T)
transit <- transit[transit$country!="SEIZURE" & transit$country!="" & transit$country!=0 & transit$country!="#N/A",]
transit$transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "transit")))

dc1 <- dplyr :: count(subset, Destination.Country1.1)
names(dc1) <- c("country", "n1")
dc2 <- dplyr :: count(subset, Destination.Country2.1)
names(dc2) <- c("country", "n2")
dc3 <- dplyr :: count(subset, Destination.Country3.1)
names(dc3) <- c("country", "n3")

dest <- merge(merge(x = dc1, y = dc2, by = "country", all = T), dc3, by = "country", all = T)
dest <- dest[dest$country!="SEIZURE" & dest$country!="" & dest$country!=0 & dest$country!="#N/A",]
dest$destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "destination")))

seizures <- data.frame(table(subset$Seizure.Country.1))
names(seizures) <- c("country", "seizures")
seizures <- seizures[seizures$country!="" & seizures$country!=0 & seizures$country!="#N/A",]
seizures[is.na(seizures),] <- 0

ratio <- merge(merge(merge(origin, transit, by = "country", all = T), dest, by = "country", all = T), seizures, by= "country", all = T)
ratio[is.na(ratio)] <- 0
ratio$tsums <- rowSums(ratio[,2:4], na.rm = T)
ratio$ratio <- ratio$seizures/ratio$tsums

ratiograph <- data.frame(subset(ratio, tsums > 5))

png(file = "tot_risk.png", units="in", width=11, height=8.5, res=300)

ggplot(ratiograph, aes(x = reorder(country, ratio), y = ratio)) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "maroon") +
  geom_text(aes(label=paste(round(ratio*100, 1), "%")),hjust=-.1,family = "Gill Sans MT", size = 5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0,.25,.50,.75,1), labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.5))

dev.off()

ratiograph2 <- data.frame(subset(ratio, tsums > 20))

png(file = "tot_risk20.png", units="in", width=11, height=8.5, res=300)

ggplot(ratiograph2, aes(x = reorder(country, ratio), y = ratio)) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "maroon") +
  geom_text(aes(label=paste(round(ratio*100, 1), "%")),hjust=-.1,family = "Gill Sans MT", size = 5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0,.25,.50,.75,1), labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.5))

dev.off()
