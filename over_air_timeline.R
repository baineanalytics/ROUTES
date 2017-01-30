##AIRPORT TOP SIX TIMELINE##

library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

air <- data.frame(table(tot$Location))
air[air == 0] <- NA
air[air == ""] <- NA
air <- air[complete.cases(air),]
names(air) <- c("airport", "sum")

timeline <- data.frame(c(2009:2016))
names(timeline) <- c("Year")

tops <- subset(air, sum > 21)

for (j in 1:nrow(tops)){
  name <- tops$airport[j]
  data <- subset(tot, Location == name, select = c(Year, Location))
  data <- data.frame(table(data$Year))
  names(data) <- c("Year", as.character(name))
  timeline <- merge(timeline, data, by = "Year", all = T)
}

timeline[is.na(timeline)] <- 0

timeline$`Dubai Airport` <- timeline$`Dubai Airport` + .02
timeline$`Entebbe Airport` <- timeline$`Entebbe Airport` - .02
timeline$`Hong Kong Airport` <- timeline$`Hong Kong Airport` + .04
timeline$`Suvarnabhumi Airport` <- timeline$`Suvarnabhumi Airport` - .04
timeline$`Jomo Kenyatta Airport` <- timeline$`Jomo Kenyatta Airport` + .06

timelinemelt <- melt(data = timeline, id = "Year")

png(file = "overall_air_time.png", units="in", width=11, height=8.5, res=300)

ggplot(data = timelinemelt, aes(x=Year, y=value, color=variable)) + geom_line(size = 2) +
  theme_bw() + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .5))

dev.off()