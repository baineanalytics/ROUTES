## Total rhino horn seizures and horns over time##

library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

tot$Weight..kg. <- as.numeric(as.character(tot$Weight..kg.))

rhino <- data.frame(subset(tot, Database == "Rhino Horn"))

yrs <- data.frame(table(rhino$Year))
names(yrs) <- c("year", "seizures")

yrs5 <- subset(rhino, Number > 5)
yrs5 <- data.frame(table(yrs5$Year))
names(yrs5) <- c("year", "horns5")

yrs10 <- subset(rhino, Number > 10)
yrs10 <- data.frame(table(yrs10$Year))
names(yrs10) <- c("year", "horns10")

horns <- ddply(rhino, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(horns) <- c("year", "horns")

linegraph <- merge(merge(yrs, yrs5, by = "year", all=T), yrs10, by="year", all=T)
linegraph[is.na(linegraph)] <- 0
linegraph <- melt(linegraph, id = "year")

png(file = "rh_num_time.png", units="in", width=11, height=8.5, res=300)

ggplot(horns, aes(x = year)) + 
  geom_bar(aes(y=horns), stat = "identity", color = "black", fill = "red") +
  theme_bw() +
  scale_x_continuous(breaks = c(2008:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

cols <- c("seizures" = "tomato", "horns5" = "red", "horns10" = "darkred")

png(file = "rh_seiz_time.png", units="in", width=11, height=8.5, res=300)

ggplot(linegraph, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 2) + 
  geom_point(aes(y=value), color = "Black", size = 3) + 
  theme_bw() +
  #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 5 Horns", "Seizures > 10 Horns")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5))

dev.off()

png(file = "rh_time_bof.png", units="in", width=11, height=8.5, res=300)

ggplot(timeline, aes(x = year)) + 
  geom_bar(aes(y=horns), stat = "identity", color = "black", fill = "red") +
  geom_line(aes(y=seizures), group = 1, color = "black", size = 1) + 
  geom_point(aes(y=seizures), color = "Black", size = 2) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

#avg num per seizure#

nonum <- subset(rhino, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sumhorn <- ddply(nonum, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
hornavg <- merge(sumhorn, years, by = "Year")
hornavg$avg <- hornavg$sumhorn/hornavg$count

png(file = "rh_time_avg.png", units="in", width=11, height=8.5, res=300)

ggplot(hornavg, aes(x = Year)) + geom_line(aes(y=avg), group = 1, color = "red", size = 2) + 
  geom_point(aes(y=avg), color = "Black", size = 3) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5))

dev.off()

##top airports## 

air <- data.frame(subset(rhino, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
names(air) <- c("airport", "sum")
air <- air[!is.na(air$sum),]
airports <- subset(air, sum>2)

png(file = "rh_airports.png", units="in", width=11, height=8.5, res=300)

ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "red") +
  scale_y_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .5),
        panel.grid.major.y = element_blank())

dev.off()

##airports over time##

timeline <- data.frame(c(2009:2016))
names(timeline) <- c("Year")

airports <- arrange(airports, -sum)
tops <- subset(airports, sum > 5)

for (j in 1:nrow(tops)){
  name <- tops$airport[j]
  data <- subset(rhino, Location == name, select = c(Year, Location))
  data <- data.frame(table(data$Year))
  names(data) <- c("Year", as.character(name))
  timeline <- merge(timeline, data, by = "Year", all = T)
}

timeline[is.na(timeline)] <- 0
timeline$`Maputo Airport` <- timeline$`Maputo Airport` + .02
timeline$`Jomo Kenyatta Airport` <- timeline$`Jomo Kenyatta Airport` - .02
timeline$`Beijing Capital Airport` <- timeline$`Beijing Capital Airport` + .04
timeline$`Tan Son Nhat Airport` <- timeline$`Tan Son Nhat Airport` - .04
timeline$`Noi Bai Airport` <- timeline$`Noi Bai Airport` + .06
timeline$`Suvarnabhumi Airport` <- timeline$`Suvarnabhumi Airport` - .06

timelinemelt <- melt(data = timeline, id = "Year")

png(file = "rh_air_time.png", units="in", width=11, height=8.5, res=300)

ggplot(data = timelinemelt, aes(x=Year, y=value, color=variable, group = variable)) + geom_line(size = 2) +
  theme_bw() + 
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .5))

dev.off()
