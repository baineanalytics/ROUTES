library(ggplot2)
library(maps)
library(geosphere)
library(plyr)
library(sp)

Rhino <- read.csv("RH.csv")
Rhino <- Rhino[order(Rhino$Count),]

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
      complete = TRUE
    )
}

fps <- gcIntermediate(Rhino[,c('Long', 'Lat')], Rhino[,c('Long.1', 'Lat.1')], n=100, breakAtDateLine = F, addStartEnd=T, sp = T)
fps <- as(fps, "SpatialLinesDataFrame")
fps.ff <- fortify(fps) # convert into something ggplot can plot


Rhino$id <-as.character(c(1:nrow(Rhino))) # that rts.ff$id is a char
gcir <- merge(fps.ff, Rhino, all.x=T, by="id") # join attributes, we keep them all, just in case


### Recenter ####

center <- 115 # positive values only - US centered view is 260

# shift coordinates to recenter great circles
gcir$long.recenter <-  ifelse(gcir$long  < center - 180 , gcir$long + 360, gcir$long) 

# shift coordinates to recenter worldmap
map <- map_data("world")
map$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)

### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
gcir.rg <- ddply(gcir, .(id), RegroupElements, "long.recenter", "id")
map.rg <- ddply(map, .(group), RegroupElements, "long.recenter", "group")

# close polys
map.cp <- ddply(map.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var


##PLOT##

png(file = "map.png", units="in", width=11, height=8.5, res=300)

wm <- ggplot() + xlim(c(min(Rhino$Long - 15), max(Rhino$Long.1 + 25))) + ylim(c(min(Rhino$Lat - 5), max(Rhino$Lat.1 + 25))) +
  geom_polygon(data = map.cp, aes(long.recenter, lat, group = group.regroup), fill = "gray87", color="white", lwd = .25) + 
  theme_clean()

wm + geom_path(data = gcir.rg, aes(long.recenter, lat, group = group.regroup, color = "maroon"), lwd = (gcir.rg$Count/3), alpha = .9) + 
  theme(legend.position = "none")

dev.off()
