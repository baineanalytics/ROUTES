#total seizures per database

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(ggplot2)
library(reshape2)
library(extrafont)

tot <- read.csv("tot.csv")
dbcount <- data.frame(table(tot$Database))
dbcount$Var1 <- factor(dbcount$Var1, levels = c("Ivory", "Rhino Horn", "Reptiles", "Birds"))

png(file = "tot_seiz_counts.png", units="in", width=11, height=8.5, res=300)

ggplot(data = dbcount, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = 'identity', color = "black", fill = "maroon") +
  geom_text(aes(label = Freq), vjust = -.25, family = "Gill Sans MT", size = 6) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .5), 
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

dev.off()

#total seizures timeline

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(ggplot2)
library(reshape2)

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

png(file = "tot_seiz_time.png", units="in", width=11, height=8.5, res=300)

ggplot(timelinemelt, aes(x = year, y = value, group = variable, color = variable)) + geom_line(lwd = 2) + 
  scale_color_manual(values = c('darkorange', 'red', 'green4', 'steelblue3')) +
  geom_point(aes(y=value), color = "Black", size = 3) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

dev.off()

#Airports

library(ggplot2)
library(dplyr)

tot <- read.csv("tot.csv")
dbref <- c("Ivory", "Rhino Horn", "Reptiles", "Birds")
tot$Database <- factor(tot$Database, levels = dbref)

airports <- data.frame(table(tot$Database, tot$Location, exclude = ""))
names(airports) <- c("db","airport","Freq")

sums <- airports %>% group_by(airport) %>% summarize(sum = sum(Freq))
sums <- sums %>% filter(sum>4)

airports <- subset(airports, airport %in% sums$airport)
airports <- dplyr::full_join(sums,airports)
airports <- transform(airports, airport = reorder(airport, sum))

colors <- c("darkorange", "red", "green4", "steelblue3")

png(file = "tot_airports.png", units="in", width=11, height=8.5, res=300)

ggplot(data = airports, aes(x = airport, y = Freq, fill = db)) + 
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() + coord_flip() +
  scale_fill_manual(values = colors) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .75),
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

#Transit Info

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

transitgraph <- data.frame(subset(countries, sums > 15))
transitgraph <- transform(transitgraph, country = reorder(country, sums))
transitgraph <- dplyr::select(transitgraph, country, Origin, Transit, Destination)

require(reshape2)

stack <- melt(data = transitgraph, id = "country")

png(file = "tots_transit.png", units="in", width=11, height=8.5, res=300)

require(ggplot2)

ggplot(data = stack, aes(x = country, y = value, fill =variable)) + 
  geom_bar(stat = "identity") + theme_bw() + coord_flip() +
  scale_fill_brewer(palette = 'Set1') +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .5), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

dev.off()

#Seizures Counts

seizures <- countries %>% dplyr::select(country, Seizure.Country)
names(seizures) <- c("Country", "Seizures")
lseiz <- subset(seizures, Seizures>10)

png(file = "tot_seizures.png", units="in", width=11, height=8.5, res=300)

ggplot(data = lseiz, aes(x = reorder(Country,Seizures), y = Seizures)) + 
  geom_bar(stat = "identity", color = "black", fill = "maroon") + theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .5), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

dev.off()

#Information Quality

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
names(infoq) <- c("Date", "Seizure Airport", "Seizure Location", "Origin", "Transit", "Destination", "Weight", "Number", "Airline", "Mode of Transport", "Method of Detection", "Obfuscation Method", "Species")

library(reshape)
meltedinfo <- melt(infoq)
meltedinfo$labs <- paste(round(meltedinfo$value, 1),"%",sep="")

png(file = "tot_info_quality.png", units="in", width=11, height=8.5, res=300)

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

#totals heatmap

#heatmap#

trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])


transitgraph <- data.frame(subset(transitgraph, sum > 0))

## Pick up here ##

library(ggplot2)
library(plyr)
library(maps)
library(RColorBrewer)
library(ggalt)

world_map <- map_data("world")
world_map <- subset(world_map, region!="Antarctica")

heat <- merge(transitgraph, world_map, by.x = "country", by.y = "region", all = T)
heat <- arrange(heat, group, order)

##PLOT## 

theme_clean <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0,0,0,0),"lines"),
      complete = TRUE)
}

png(file = "tot_heat600.png", units="in", width=11, height=6, res=600)

gg <- ggplot(data = heat, aes(x = long, y = lat, group = group, fill = sum)) + 
  geom_map(dat = world_map, map = world_map, aes(map_id = region), color = "gray20", fill = "white", lwd = .05) + 
  theme_clean() +
  coord_proj("+proj=robin")

gg <- gg + geom_map(map = world_map, aes(map_id = country, fill = sum), color = "gray20", lwd = .05) + 
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray87", direction = 1) + 
  theme(legend.position = c(0.5, 0.12),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.key.height = unit(.08,"in"),
        legend.key.width = unit(.25,"in"),
        panel.background = element_rect(size = 2, color = "black"))
gg

dev.off()






##FOR A COUNT Of COUNTRIES##

trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

count <- merge(merge(merge(seiz, orig, by = "country", all = T), transit, by = "country", all = T), dest, by = "country", all = T)
count[is.na(count)] <- 0
count$sum <- rowSums(count[,2:5])

count <- data.frame(subset(count, sum > 0))

dplyr :: count(count, seizure > 0)
