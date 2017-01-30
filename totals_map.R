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

tots <- read.csv("tots_map.csv")

fps <- gcIntermediate(tots[,c('c1long', 'c1lat')], tots[,c('c2long', 'c2lat')], n=100, breakAtDateLine = T, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps)

tots$id <-as.character(c(1:nrow(tots)))
gcir2 <- merge(fps.ff, tots, all.x=T, by="id")

bubs <- subset(tots, select= c("Unique","lat","long","total.sum"))
bubs <- data.frame(bubs[complete.cases(bubs),])

png(file = "tots_map2.png", units="in", width=6.5, height=5, res=300)

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

wm <- wm + geom_point(data = bubs, aes(size = total.sum, group=1), color = "purple4", alpha = .9) + 
  scale_size(breaks = c(30,60,90,114), label = c("30","60","90","114")) +
  geom_line(data = gcir2, aes(x = long.x, y = lat.x, group = group, alpha = sum), color = "purple3", lwd = .7) + 
  scale_alpha_continuous(range = c(0.15, 0.6)) +
  theme(legend.key.height = unit(0.025, "in"),
        legend.position = c(0.5, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"))

wm

dev.off()