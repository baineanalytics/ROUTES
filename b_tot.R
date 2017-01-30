## Total seizures and birds over time##

library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

bird <- data.frame(subset(tot, Database == "Birds"))

yrs <- data.frame(table(bird$Year))
names(yrs) <- c("year", "seizures")

b50 <- subset(bird, Number > 50)
b50 <- data.frame(table(b50$Year))
names(b50) <- c("year", "b50")

b500 <- subset(bird, Number > 500)
b500 <- data.frame(table(b500$Year))
names(b500) <- c("year", "b500")

timeline <- merge(merge(yrs, b50, by="year", all=T),b500,by="year",all=T)
timeline[is.na(timeline)] <- 0

timemelt <- melt(timeline, id="year")

bird <- transform(bird, Number = as.numeric(Number))
nb <- ddply(bird, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(nb) <- c("year", "birds")

png(file = "b_aml_time.png", units="in", width=11, height=8.5, res=300)

ggplot(nb, aes(x = year)) + 
  geom_bar(aes(y=birds), stat = "identity", color = "black", fill = "steelblue3") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

cols <- c("seizures" = "skyblue1", "b50" = "steelblue3", "b500" = "midnightblue")

png(file = "b_seiz_time.png", units="in", width=11, height=8.5, res=300)

ggplot(timemelt, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 2) + 
  geom_point(aes(y=value), color = "Black", size = 3) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 50 Birds", "Seizures > 500 Birds")) +
    #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

#box#

years <- c(2009:2016)
ylim1 <- c(0,250)

png(file = "bird_time_box2.png", units="in", width=11, height=8.5, res=300)

ggplot(bird, aes(Year, Number, group = Year)) + geom_boxplot(fill = 'steelblue3', na.rm = T) +
  theme_bw() + scale_x_continuous(breaks = years) +
  coord_cartesian(ylim = ylim1) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 1))

dev.off()

##top airports## 

air <- data.frame(subset(bird, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
air <- air[complete.cases(air),]
names(air) <- c("airport", "sum")
air <- subset(air, sum>2)

png(file = "b_airports.png", units="in", width=11, height=8.5, res=300)

ggplot(air, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "steelblue3") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), breaks = c(0,5,10,15)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = .5))

dev.off()

##airports over time##

timeline <- data.frame(c(2009:2016))
names(timeline) <- c("Year")

airports <- arrange(air, -sum)
tops <- subset(airports, sum > 3)

for (j in 1:nrow(tops)){
  name <- tops$airport[j]
  data <- subset(bird, Location == name, select = c(Year, Location))
  data <- data.frame(table(data$Year))
  names(data) <- c("Year", as.character(name))
  timeline <- merge(timeline, data, by = "Year", all = T)
}

timeline[is.na(timeline)] <- 0

timeline$`Dubai Airport` <- timeline$`Dubai Airport` + .02
timeline$`Cheddi Jagan Airport` <- timeline$`Cheddi Jagan Airport` - .02
timeline$`Soekarno-Hatta Airport` <- timeline$`Soekarno-Hatta Airport` + .04
timeline$`Miami Airport` <- timeline$`Miami Airport` - .04
timeline$`Sao Paulo-Guarulhos Airport` <- timeline$`Sao Paulo-Guarulhos Airport` + .06
timeline$`Abu Dhabi Airport` <- timeline$`Abu Dhabi Airport` - .06
timeline$`Jinnah Airport` <- timeline$`Jinnah Airport` + .08
timeline$`Los Angeles Airport` <- timeline$`Los Angeles Airport` -.08

timelinemelt <- melt(data = timeline, id = "Year")

png(file = "b_air_time.png", units="in", width=11, height=8.5, res=300)

ggplot(data = timelinemelt, aes(x=Year, y=value, color=variable)) + geom_line(size = 2) +
  theme_bw() + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .5))

dev.off()