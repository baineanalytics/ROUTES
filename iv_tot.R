## Total ivory seizures over time##

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

png(file = "iv_weight_time.png", units="in", width=11, height=8.5, res=300)

ggplot(ni, aes(x = year)) + 
  geom_bar(aes(y=ivory), stat = "identity", color = "black", fill = "darkorange") +
  theme_bw() +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000)) +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

cols <- c("seizures" = "orange", "seizures100" = "darkorange", "seizures500" = "orangered")

png(file = "iv_seiz_time.png", units="in", width=11, height=8.5, res=300)

ggplot(linegraph, aes(x = year)) + geom_line(aes(y=value, group=variable, color = variable), size = 2) + 
  geom_point(aes(y=value), color = "Black", size = 3) + 
  theme_bw() +
  scale_colour_manual(name="Seizure Weight",values=cols,labels=c("All Seizures", "Seizures > 100kgs", "Seizures > 500kgs")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

png(file = "iv_time_bof.png", units="in", width=11, height=8.5, res=300)

ggplot(timeline, aes(x = year)) + 
  geom_bar(aes(y=ivory), stat = "identity", color = "black", fill = "darkorange") +
  geom_line(aes(y=seizures), group = 1, color = "black", size = 1) + 
  geom_point(aes(y=seizures), color = "Black", size = 2) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

##Average Weight per Seizure##

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

#line avg

png(file = "iv_time_avg.png", units="in", width=11, height=8.5, res=300)

ggplot(ivavg, aes(x = Year)) + 
  geom_line(aes(y=avg), group = 1, color = "darkorange", size = 2) + 
  geom_point(aes(y=avg), color = "black", size = 3) + 
  theme_bw() +  
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

#box

years <- c(2009:2016)

png(file = "iv_time_box2.png", units="in", width=11, height=8.5, res=300)

ggplot(ivory, aes(Year, Weight..kg., group = Year)) + geom_boxplot(fill = 'darkorange') +
  theme_bw() + scale_x_continuous(breaks = years) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

##top airports## 

air <- data.frame(subset(ivory, select = c("Year", "Location")))
air <- data.frame(table(air$Location))
air[air == 0] <- NA
air <- air[complete.cases(air),]
names(air) <- c("airport", "sum")
airports <- subset(air, sum>3)

png(file = "iv_airports.png", units="in", width=11, height=8.5, res=300)

ggplot(airports, aes(x = reorder(airport, sum), y = (sum))) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "darkorange") +
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
tops <- subset(airports, sum > 7)

for (j in 1:nrow(tops)){
  name <- tops$airport[j]
  data <- subset(ivory, Location == name, select = c(Year, Location))
  data <- data.frame(table(data$Year))
  names(data) <- c("Year", as.character(name))
  timeline <- merge(timeline, data, by = "Year", all = T)
}

timeline[is.na(timeline)] <- 0

timeline$`Lilongwe Airport` <- timeline$`Lilongwe Airport` + .02
timeline$`Entebbe Airport` <- timeline$`Entebbe Airport` - .02
timeline$`Hong Kong Airport` <- timeline$`Hong Kong Airport` + .04
timeline$`Suvarnabhumi Airport` <- timeline$`Suvarnabhumi Airport` - .04
timeline$`Murtala Mohammed Airport` <- timeline$`Murtala Mohammed Airport` + .06

timelinemelt <- melt(data = timeline, id = "Year")

png(file = "iv_air_time.png", units="in", width=11, height=8.5, res=300)

ggplot(data = timelinemelt, aes(x=Year, y=value, color=variable)) + geom_line(size = 2) +
  theme_bw() + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .5))

dev.off()

## Ivory seizures >500 kgs timeline

library(ggplot2)
library(reshape2)

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))
ivory$Weight..kg. <- as.numeric(as.character(ivory$Weight..kg.))
ivory <- subset(ivory, Weight..kg. > 500)

yrs <- data.frame(table(ivory$Year))
names(yrs) <- c("year", "seizures")

png(file = "iv_seiz_time500.png", units="in", width=11, height=8.5, res=300)

ggplot(yrs, aes(x = year)) + geom_line(aes(y=seizures), group = 1, color = "darkorange", size = 2) + 
  geom_point(aes(y=seizures), color = "Black", size = 3) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5))

dev.off()

## Routes for ivory >500

ids <- ivory$ID

library(ggplot2)
library(maps)
library(geosphere)
library(cobs)
library(RColorBrewer)
library(sp)
library(ggalt)
library(ggrepel)

routes <- read.csv("ivory500_map.csv")
routes <- routes[order(routes$count),]

fps <- gcIntermediate(routes[,c('c1long', 'c1lat')], 
                      routes[,c('c2long', 'c2lat')], 
                      n=100, 
                      breakAtDateLine = F, 
                      addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps) # convert into something ggplot can plot

routes$id <-as.character(c(1:nrow(routes))) # that rts.ff$id is a char
gcir <- merge(fps.ff, routes, all.x=T, by="id") # join attributes, we keep them all, just in case

## make more lines for count > 1 

new_data <- gcir[gcir$count > 1,]

new_lines_data <- NULL
groups <- unique(new_data$group)
for(i in 1:length(groups)){
  group <- groups[i]
  data <- new_data[new_data$group == group,]
  count <- max(data$count)
  second_data <- NULL
  for(j in 1:count){
    if(j==1){
      next
    }
    
    data_2 <- data
    
    df <- data.frame(long=data_2$long,lat=data_2$lat)
    
    ## first get new latitute line
    df$lat[50] <- df$lat[50] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[50], df$lat[50])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[50],tail(df$long,1))
    
    ## make like with contrained b-spline
    cob_lat <- cobs(df$long,df$lat,knots=knots,pointwise=cons)
    ##plot(cob_lat)
    
    ## this will be the new curved latitute line
    lat_fit <- cob_lat$fitted
    
    data_2$lat <- lat_fit
    
    data_2$group <- paste(group,".",j,sep="") 
    
    second_data <- rbind(second_data,data_2)
  }
  inter_data <- rbind(data,second_data)
  new_lines_data <- rbind(new_lines_data, inter_data)
}

gcir2 <- gcir[gcir$count == 1,]
gcir2 <- rbind(gcir2, new_lines_data)

## back to the code

map <- map_data("world")
map <- subset(map, region!="Antarctica")

lat <- c(min(routes$c1lat) - 10, max(routes$c2lat + 15))
long <- c(min(routes$c1long) - 15, max(routes$c2long + 20))

#bubs and labs

bubs <- subset(routes, select = c(unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])

##PLOT##

png(file = "ivory_map500.png", units="in", width=11, height=8.5, res=300)

wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) + 
  #xlim(long) + 
  #ylim(lat) +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .2) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        #panel.grid = element_blank(),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "darkorange", alpha = .9) + 
  scale_size(breaks = c(3,6,9), label = c("3","6","9")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .7) + 
  scale_color_distiller(palette = 'Oranges', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
  theme(legend.key.height = unit(0.025, "in"),
        legend.position = c(0.5, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"))

wm

dev.off()