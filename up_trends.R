#SEIZURE TIMELINE#

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(ggplot2)
library(reshape2)
library(extrafont)

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))

ivyrs <- data.frame(table(ivory$Year))
names(ivyrs) <- c("year", "Ivory")

rhino <- data.frame(subset(tot, Database == "Rhino Horn"))

ryrs <- data.frame(table(rhino$Year))
names(ryrs) <- c("year", "Rhino Horn")

rep <- data.frame(subset(tot, Database == "Reptiles"))

repyrs <- data.frame(table(rep$Year))
names(repyrs) <- c("year", "Reptiles")

bird <- data.frame(subset(tot, Database == "Birds"))

byrs <- data.frame(table(bird$Year))
names(byrs) <- c("year", "Birds")

timeline <- merge(merge(merge(byrs, repyrs, by = "year"), ryrs, by = "year"), ivyrs, by = "year")
timelinemelt <- melt(timeline)
timelinemelt$variable <- factor(timelinemelt$variable, levels = c("Ivory", "Rhino Horn", "Reptiles", "Birds"))

png(file = "uptot_seiz_time.png", units="in", width=5, height=4, res=300)

ggplot(timelinemelt, aes(x = year, y = value, group = variable, color = variable)) + geom_line(lwd = 1.5) + 
  scale_color_manual(values = c('darkorange', 'red', 'green4', 'steelblue3')) +
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

#Total seizures timeline#

timeline$sum <- rowSums(timeline[,2:5])

png(file = "overall_seiz_time.png", units="in", width=5, height=4, res=300)

ggplot(timeline, aes(x = year, y = sum)) + 
  geom_line(group = 1, lwd = 1.5, color = "maroon") + 
  geom_point(aes(y=sum), color = "Black", size = 2) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

#COUNTRY SEIZURES#

library(dplyr)

trans <- read.csv("transittots.csv")
trans[trans == ""] <- NA

count <- trans %>% select(ID, Date, Database, Seizure.Country, Origin.Country1, Origin.Country2, Origin.Country3, Transit.Country1, Transit.Country2, Transit.Country3, Transit.Country4, Destination.Country1, Destination.Country2, Destination.Country3)
count <- subset(count[,4:14])
uni <- NULL

for (j in 1:length(count)){
  nat <- count[j]
  natu <- unique(nat)
  names(natu) <- c("country")
  uni <- rbind(natu, uni)
}

countries <- unique(uni)
countries <- data.frame(countries[!is.na(countries$country),])
names(countries) <- c("country")

for (i in 1:length(count)){
  data <- count[i]
  name <- names(data)
  data <- data.frame(table(data, exclude = c(NA, "")))
  names(data) <- c("country", name)
  countries <- merge(countries, data, by = "country", all=T)
}

countries$Origin <- rowSums(countries[,3:5], na.rm = T)
countries$Transit <- rowSums(countries[,6:9], na.rm = T)
countries$Destination <- rowSums(countries[10:12], na.rm = T)
countries$sums <- rowSums(countries[13:15], na.rm = T)

countries[is.na(countries)] <- 0

seizures <- countries %>% dplyr::select(country, Seizure.Country)
names(seizures) <- c("Country", "Seizures")
lseiz <- subset(seizures, Seizures>10)

png(file = "uptot_seizures.png", units="in", width=5, height=4, res=300)

ggplot(data = lseiz, aes(x = reorder(Country, Seizures), y = Seizures)) + 
  geom_bar(stat = "identity", color = "black", fill = "maroon", width = .85, size = .4) + 
  theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .75), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

#Data Availability#

tot <- read.csv("tot.csv")

tot[is.na(tot)] <- ""

info <- NULL

for (j in 1:length(tot)){
  data <- tot[j]
  name <- names(data)
  wifdata <- sum(data != "")
  dataperc <- data.frame((wifdata/773)*100)
  names(dataperc) <- name
  info <- dplyr::bind_cols(info, dataperc)
}

infoq <- info[,3:16]
infoq <- infoq[,-2]
names(infoq) <- c("Seizure Date", "Seizure Airport", "Seizure Location", "Origin Location", "Transit Location(s)", "Destination Location", "Weight", "Number", "Airline", "Mode of Transport", "Method of Detection", "Obfuscation Method", "Species")

library(reshape2)
meltedinfo <- melt(infoq)
meltedinfo$labs <- paste(round(meltedinfo$value, 1),"%",sep="")

png(file = "uptot_info_quality.png", units="in", width=11, height=8.5, res=300)

ggplot(data = meltedinfo, aes(x = reorder(variable,value), y = value)) + 
  geom_bar(stat = 'identity', color = "black", fill = "maroon") +
  geom_text(aes(label = labs), hjust = 1.1, family = "Gill Sans MT", size = 5) +
  theme_bw() + coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100), labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .5), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

dev.off()

##IVORY##


library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))
ivory <- transform(ivory, Weight..kg. = as.numeric(as.character(Weight..kg.)))

yrs <- data.frame(table(ivory$Year))
names(yrs) <- c("year", "seizures")

iv100 <- subset(ivory, Weight..kg. > 100)
yrs100 <- data.frame(table(iv100$Year))
names(yrs100) <- c("year", "seizures100")

iv500 <- subset(ivory, Weight..kg. > 500)
yrs500 <- data.frame(table(iv500$Year))
names(yrs500) <- c("year", "seizures500")

ni <- ddply(ivory, .(Year), summarize, sumweight = sum(Weight..kg., na.rm = TRUE))
names(ni) <- c("year", "ivory")

timeline <- merge(merge(yrs, yrs100, by = "year"),yrs500, by= "year")
linegraph <- melt(timeline, id="year")

png(file = "upiv_weight_time.png", units="in", width=5, height=4, res=300)

ggplot(ni, aes(x = year)) + 
  geom_bar(aes(y=ivory), stat = "identity", color = "black", fill = "darkorange", width = .85, size = .4) +
  theme_bw() +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000)) +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

dev.off()

cols <- c("seizures" = "#FFD6A7", "seizures100" = "#F98C0E", "seizures500" = "#4B2800")

png(file = "upiv_seiz_time.png", units="in", width=5, height=4, res=300)

ggplot(linegraph, aes(x = year)) + geom_line(aes(y=value, group=variable, color = variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures",values=cols,labels=c("All Seizures", "Seizures > 100kgs", "Seizures > 500kgs")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.title = element_text(family = "Gill Sans MT"))

dev.off()



nonum <- subset(ivory, select = c("Weight..kg.", "Year"), na.strings=c("","NA"))

require(dplyr)

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

nonum <- nonum %>% mutate_each(funs(empty_as_na)) 
nonum <- nonum[complete.cases(nonum),]

sums <- ddply(nonum, .(Year), summarize, sums = sum(Weight..kg., na.rm = TRUE))

years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")

ivavg <- merge(sums, years, by = "Year")
ivavg$avg <- ivavg$sums/ivavg$count

png(file = "upiv_time_avg.png", units="in", width=5, height=4, res=300)

ggplot(ivavg, aes(x = Year)) + 
  geom_line(aes(y=avg), group = 1, color = "darkorange", size = 1.5) + 
  geom_point(aes(y=avg), color = "black", size = 2) + 
  theme_bw() +  
  scale_y_continuous(breaks = c(100,200,300,400,500,600)) +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

dev.off()


##RHINO HORN##


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

png(file = "uprh_num_time.png", units="in", width=5, height=4, res=300)

ggplot(horns, aes(x = year)) + 
  geom_bar(aes(y=horns), stat = "identity", color = "black", fill = "red", width = .85, size = .4) +
  theme_bw() +
  scale_x_continuous(breaks = c(2008:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

dev.off()

cols <- c("seizures" = "#FFA7A7", "horns5" = "#F90E0E", "horns10" = "#4B0000")

png(file = "uprh_seiz_time.png", units="in", width=5, height=4, res=300)

ggplot(linegraph, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 5 Horns", "Seizures > 10 Horns")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.title = element_text(family = "Gill Sans MT"))

dev.off()


nonum <- subset(rhino, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sumhorn <- ddply(nonum, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
hornavg <- merge(sumhorn, years, by = "Year")
hornavg$avg <- hornavg$sumhorn/hornavg$count

png(file = "uprh_time_avg.png", units="in", width=5, height=4, res=300)

ggplot(hornavg, aes(x = Year)) + geom_line(aes(y=avg), group = 1, color = "red", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

dev.off()


##REPTILES##


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

png(file = "uprep_aml_time.png", units="in", width=5, height=4, res=300)

ggplot(nr, aes(x = year)) + 
  geom_bar(aes(y=reptiles), stat = "identity", color = "black", fill = "green4", width = .85, size = .4) +
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

dev.off()

cols <- c("seizures" = "#9DEF9D", "rep100" = "#0BC80B", "rep1000" = "#003C00")

png(file = "uprep_seiz_time.png", units="in", width=5.25, height=4, res=300)

ggplot(timemelt, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 100 Reptiles", "Seizures > 1000 Reptiles")) +
  #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.title = element_text(family = "Gill Sans MT"))

dev.off()

nonum <- subset(rep, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sums <- ddply(nonum, .(Year), summarize, sums = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
avg <- merge(sums, years, by = "Year")
avg$avg <- avg$sums/avg$count

png(file = "uprep_time_avg.png", units="in", width=5, height=4, res=300)

ggplot(avg, aes(x = Year)) + geom_line(aes(y=avg), group = 1, color = "green4", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

dev.off()


##BIRDS##


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

png(file = "upb_aml_time.png", units="in", width=5, height=4, res=300)

ggplot(nb, aes(x = year)) + 
  geom_bar(aes(y=birds), stat = "identity", color = "black", fill = "steelblue3", width = .85, size = .4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

dev.off()

cols <- c("seizures" = "#97C9E1", "b50" = "#10709E", "b500" = "#012030")

png(file = "upb_seiz_time.png", units="in", width=5, height=4, res=300)

ggplot(timemelt, aes(x = year)) + geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 50 Birds", "Seizures > 500 Birds")) +
  #scale_y_continuous(limits = c(0, 35), breaks = c(0,5,10,15,20,25,30,35)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.title = element_text(family = "Gill Sans MT"))

dev.off()


nonum <- subset(bird, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sums <- ddply(nonum, .(Year), summarize, sums = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
avg <- merge(sums, years, by = "Year")
avg$avg <- avg$sums/avg$count

png(file = "upb_time_avg.png", units="in", width=5, height=4, res=300)

ggplot(avg, aes(x = Year)) + geom_line(aes(y=avg), group = 1, color = "steelblue3", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

dev.off()