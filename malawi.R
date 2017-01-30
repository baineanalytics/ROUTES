##TIMELINE by DATABASE/SEIZURELOC##

library(dplyr)
library(ggplot2)
library(extrafont)

full <- read.csv("transittots_ratio.csv")

origin <- data.frame(subset(full, Origin.Country1.1 == "Malawi", select = c(Seizure.Country.1, Year, Database)))
origin$SC <- "origin"

tc1 <- data.frame(subset(full, Transit.Country1.1 == "Malawi", select = c(Seizure.Country.1, Year, Database)))
tc2 <- data.frame(subset(full, Transit.Country2.1 == "Malawi", select = c(Seizure.Country.1, Year, Database)))
tc3 <- data.frame(subset(full, Transit.Country3.1 == "Malawi", select = c(Seizure.Country.1, Year, Database)))
trans <- rbind(tc1,tc2,tc3)
trans$SC <- "transit"

dest <- data.frame(subset(full, Destination.Country1.1 == "Malawi", select = c(Seizure.Country.1, Year, Database)))
dest$SC <- "destination"

malawi <- rbind(origin, trans, dest)

malawi$seiz <- malawi$Seizure.Country.1 == "Malawi"

stack <- data.frame(table(malawi$Year, malawi$seiz))

png(file = "malawi_timeline.png", units="in", width=5, height=4, res=300)

ggplot(data = stack, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkred", "darkgreen"), labels = c("Seized Elsewhere", "Seized in Malawi")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .75),
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()