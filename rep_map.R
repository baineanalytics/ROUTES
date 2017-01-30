setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(ggplot2)
library(maps)
library(geosphere)
library(cobs)
library(RColorBrewer)
library(sp)
library(ggalt)
library(extrafont)

map <- map_data("world")
map <- subset(map, region!="Antarctica")

rep <- read.csv("rep_map.csv")
rep <- rep[order(rep$count),]

fps <- gcIntermediate(rep[,c('c1long', 'c1lat')], rep[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

rep$id <-as.character(c(1:nrow(rep))) # that rts.ff$id is a char
gcir <- merge(fps.ff, rep, all.x=T, by="id") # join attributes, we keep them all, just in case

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
    df$lat[nrow(df)/2] <- df$lat[nrow(df)/2] + j
    
    ## create the contraints
    start <- c(0, head(df$long,1) ,head(df$lat,1))
    mid <- c(0, df$long[nrow(df)/2], df$lat[nrow(df)/2])
    end <- c(0, tail(df$long,1), tail(df$lat,1))
    cons <- rbind(start,mid,end)
    knots <- c(head(df$long,1),df$long[nrow(df)/2],tail(df$long,1))
    
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

#Back to code

bubs <- subset(rep, select = c(Unique, ulat, ulong, sum))
bubs <- data.frame(bubs[complete.cases(bubs),])

text <- subset(bubs, sum > 3)

#PLOT# 

png(file = "rep_map.png", units="in", width=11, height=8.5, res=300)

wm <- ggplot(data = map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_map(map = map, aes(map_id = region), 
           color = "black", 
           fill = "gray75", 
           lwd = .2) + 
  coord_proj("+proj=robin") + ylim(c(-86,86)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey92"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0),"lines"))

wm <- wm + geom_point(data = bubs, aes(x = ulong, y = ulat, size = sum), color = "green4", alpha = .9) + 
  scale_size(breaks = c(1,25,54), label = c("1","25","54")) +
  geom_line(data = gcir2, aes(color = order, group = group), lwd = .7, alpha = .4) + 
  scale_color_distiller(palette = 'Greens', direction = 1, guide = "colorbar", name = "Flight\nPath", 
                        breaks = c(10,80), label = c("Origin" , "Destination")) +
  theme(legend.key.height = unit(0.025, "in"),
        legend.position = c(0.5, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"))

wm

dev.off()
