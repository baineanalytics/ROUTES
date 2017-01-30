## Total reptile seizures over time##

library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

rep <- data.frame(subset(tot, Database == "Reptiles"))

yrs <- data.frame(table(rep$Year))
names(yrs) <- c("year", "seizures")

rep100 <- subset(rep, Number > 100)
rep100 <- data.frame(table(rep100$Year))
names(rep100) <- c("year", "rep100")

rep1000 <- subset(rep, Number > 1000)
rep1000 <- data.frame(table(rep1000$Year))
names(rep1000) <- c("year", "rep1000")

timeline <- merge(merge(yrs, rep100, by="year", all=T),rep1000,by="year",all=T)
timeline[is.na(timeline)] <- 0

timemelt <- melt(timeline, id="year")
  
nr <- ddply(rep, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(nr) <- c("year", "reptiles")

timeline <- merge(yrs, nr, by = "year")

png(file = "rep_aml_time.png", units="in", width=11, height=8.5, res=300)

ggplot(nr, aes(x = year)) + 
  geom_bar(aes(y=reptiles), stat = "identity", color = "black", fill = "green4") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

cols <- c("seizures" = "#a2ff98", "rep100" = "green3", "rep1000" = "#13460e")

png(file = "rep_seiz_time2.png", units="in", width=5, height=4, res=300)

ggplot(timemelt, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 100 Reptiles", "Seizures > 1000 Reptiles")) +
    #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5),
        legend.text = element_text(family = "Gill Sans MT", size = 8),
        legend.title = element_text(family = "Gill Sans MT", size = 10))

dev.off()

##AVERAGE##

nonum <- subset(rep, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sums <- ddply(nonum, .(Year), summarize, sums = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
avg <- merge(sums, years, by = "Year")
avg$avg <- avg$sums/avg$count

png(file = "rep_time_avg.png", units="in", width=11, height=8.5, res=300)

ggplot(avg, aes(x = Year)) + geom_line(aes(y=avg), group = 1, color = "green4", size = 2) + 
  geom_point(aes(y=avg), color = "Black", size = 3) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5))

dev.off()

#box#

years <- c(2009:2016)
ylim1 <- c(0,1000)

png(file = "rep_time_box.png", units="in", width=11, height=8.5, res=300)

ggplot(rep, aes(Year, Number, group = Year)) + geom_boxplot(fill = 'green4', na.rm = T) +
  theme_bw() + scale_x_continuous(breaks = years) +
  coord_cartesian(ylim = ylim1) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

##top airports## 

air <- data.frame(subset(rep, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
names(air) <- c("airport", "sum")
airports <- subset(air, sum>2)
airports <- airports[!airports$airport == "",]

png(file = "rep_airports.png", units="in", width=11, height=8.5, res=300)

ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "green4") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
       axis.text = element_text(family = "Gill Sans MT", size = 15),
       panel.grid.minor = element_blank(),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(size = .5))

dev.off()

##airports over time##

timeline <- data.frame(c(2009:2016))
names(timeline) <- c("Year")

airports <- arrange(airports, -sum)
tops <- subset(airports, sum > 8)

for (j in 1:nrow(tops)){
  name <- tops$airport[j]
  data <- subset(rep, Location == name, select = c(Year, Location))
  data <- data.frame(table(data$Year))
  names(data) <- c("Year", as.character(name))
  timeline <- merge(timeline, data, by = "Year", all = T)
}

timeline[is.na(timeline)] <- 0

timeline$`Dubai Airport` <- timeline$`Dubai Airport` + .02
timeline$`Chennai Airport` <- timeline$`Chennai Airport` - .02
timeline$`Ivato Airport` <- timeline$`Ivato Airport` + .04
timeline$`Soekarno-Hatta Airport` <- timeline$`Soekarno-Hatta Airport` - .04

timelinemelt <- melt(data = timeline, id = "Year")

png(file = "rep_air_time.png", units="in", width=11, height=8.5, res=300)

ggplot(data = timelinemelt, aes(x=Year, y=value, color=variable)) + geom_line(size = 2) +
  theme_bw() + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .5))

dev.off()