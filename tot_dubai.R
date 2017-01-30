setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")
dbref <- c("Ivory", "Rhino Horn", "Reptiles", "Birds")
colors <- c("darkorange", "red", "green4", "steelblue3")

##TIMELINE by DATABASE/SEIZURELOC##

library(dplyr)
library(ggplot2)
library(extrafont)

full <- read.csv("transittots_ratio.csv")

smug <- subset(full[1:773,])

origin <- data.frame(subset(smug, Origin.City1.1 == "Dubai", select = c(Seizure.City.1, Date, Database)))
origin$SC <- "origin"

tc1 <- data.frame(subset(smug, Transit.City1.1 == "Dubai", select = c(Seizure.City.1, Date, Database)))
tc2 <- data.frame(subset(smug, Transit.City2.1 == "Dubai", select = c(Seizure.City.1, Date, Database)))
trans <- rbind(tc1,tc2)
trans$SC <- "transit"

dest <- data.frame(subset(smug, Destination.City1.1 == "Dubai", select = c(Seizure.City.1, Date, Database)))
dest$SC <- "destination"

dubai <- rbind(origin, trans, dest)
dubai$seiz <- dubai$Seizure.City.1 == "Dubai"

dubai$Date <- as.Date(dubai$Date)
dubai <- transform(dubai, SC = reorder(SC))

dubai$yref <- ifelse(dubai$Database == "Birds", 1, ifelse(dubai$Database == "Reptiles", 2, ifelse(dubai$Database == "Rhino Horn", 3, 4)))
dubai$shaperef <- ifelse(dubai$SC == "origin", 21, ifelse(dubai$SC == "transit", 22, 23))

png(file = "dubai_timeline.png", units="in", width=5, height=4, res=300)

ggplot(data = dubai, aes(x = Date, y = yref, shape = SC, fill = seiz)) + 
  geom_point(size = 7, alpha = .7) + 
  scale_shape_manual(name = "Supply Chain",
                     breaks = c("origin", "transit", "destination"),
                     labels = c("Origin", "Transit", "Destination"),
                     values = c(21, 22, 24),
                     guide = guide_legend(override.aes = list(color = 1))) +
  scale_fill_manual(name = "Seizure Location",
                    labels = c("Seized Elsewhere", "Seized in Dubai"),
                    values = c("darkred", "darkgreen"),
                    guide = guide_legend(override.aes = list(shape = 23))) +
  scale_y_continuous(breaks = c(1:4), labels = c("Birds", "Reptiles", "Rhino Horn", "Ivory")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 10),
        legend.text = element_text(family = "Gill Sans MT", size = 10),
        legend.title = element_text(family = "Gill Sans MT", size = 10))

dev.off()

##SEIZURE LOC by SUPPLY CHAIN## Not so useful

do <- data.frame(subset(dubai, SC == "origin"))
do <- data.frame(table(do$seiz))
do$SC <- "Origin"

dt <- data.frame(subset(dubai, SC == "transit"))
dt <- data.frame(table(dt$seiz))
dt$SC <- "Transit"

dd <- data.frame(subset(dubai, SC == "destination"))
dd <- data.frame(table(dd$seiz))
dd$SC <- "Destination"

dubstack <- rbind(do, dt, dd)
dubstack$SC <- factor(dubstack$SC, levels = c("Origin", "Transit", "Destination"))

png(file = "dubai_seiz.png", units="in", width=5, height=4, res=300)

ggplot(data = dubstack, aes(x = SC, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(name = "Seizure Location",
                    labels = c("Seized Elsewhere", "Seized in Dubai"),
                    values = c("darkred", "darkgreen")) + 
  scale_x_discrete(breaks = c("Origin", "Transit", "Destination")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

##DATABASE by SUPPLY CHAIN##

library(dplyr)

count <- unique(dubai$Database)
dubaidbstack <- NULL
levels <- c("origin", "transit", "destination")

for (j in 1:length(count)){
  db <- count[j]
  data <- subset(dubai, Database == db)
  table <- data.frame(table(data$SC))
  table <- table %>% slice(match(levels, Var1))
  table$db <- count[j]
  dubaidbstack <- rbind(table, dubaidbstack)
  }

dubaidbstack$Var1 <- factor(dubaidbstack$Var1, levels = c("origin", "transit", "destination"))
dubaidbstack$db <- factor(dubaidbstack$db, dbref)

png(file = "dubai_database.png", units="in", width=5, height=4, res=300)

ggplot(data = dubaidbstack, aes(x = db, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = 'Set1', labels = c("Origin", "Transit", "Destination")) +
  scale_x_discrete(breaks = c("Birds", "Reptiles", "Rhino Horn", "Ivory")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),  
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

##TIMELINE of DATABASE##

origin <- data.frame(subset(smug, Origin.City1.1 == "Dubai", select = c(Seizure.City.1, Year, Database)))
origin$SC <- "origin"

tc1 <- data.frame(subset(smug, Transit.City1.1 == "Dubai", select = c(Seizure.City.1, Year, Database)))
tc2 <- data.frame(subset(smug, Transit.City2.1 == "Dubai", select = c(Seizure.City.1, Year, Database)))
trans <- rbind(tc1,tc2)
trans$SC <- "transit"

dest <- data.frame(subset(smug, Destination.City1.1 == "Dubai", select = c(Seizure.City.1, Year, Database)))
dest$SC <- "destination"

dubai <- rbind(origin, trans, dest)
dubai$Database <- factor(dubai$Database, levels = dbref)

dubai <- dubai[with(dubai, order(-Year)),]
count <- unique(dubai$Year)
yrstack <- NULL

for (j in 1:length(count)){
  db <- count[j]
  data <- subset(dubai, Year == db)
  data <- data %>% dplyr::count(Database, Year)
  yrstack <- rbind(data, yrstack)
}
yrstack$Database <- factor(yrstack$Database, dbref)

png(file = "dubai_timeline_db.png", units="in", width=5, height=4, res=300)

ggplot(data = yrstack, aes(x = Year, y = n, fill = Database)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  scale_x_discrete(breaks = c(2009:2016), labels = c(2009:2016)) +
  scale_fill_manual(values = colors) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),  
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

##Network

full <- read.csv("transittots.csv")
smug <- subset(full[1:773,])

origin <- data.frame(subset(smug, Origin.City1 == "Dubai", select = c(Origin.City1, Transit.City1,Transit.City2,Transit.City3,Destination.City1)))

tc1 <- data.frame(subset(smug, Transit.City1 == "Dubai", select = c(Origin.City1, Transit.City1,Transit.City2,Transit.City3,Destination.City1)))
tc2 <- data.frame(subset(smug, Transit.City2 == "Dubai", select = c(Origin.City1, Transit.City1,Transit.City2,Transit.City3,Destination.City1)))
trans <- rbind(tc1,tc2)

dest <- data.frame(subset(smug, Destination.City1 == "Dubai", select = c(Origin.City1, Transit.City1,Transit.City2,Transit.City3,Destination.City1)))

dubai <- rbind(origin, trans, dest)
write.csv(dubai, file = "dubs2.csv")

##EDITED IN EXCEL##

dubnet <- read.csv("dubs2.csv")

library(igraph)

g <- graph.data.frame(dubnet, directed = T)
par(mar=(c(0,0,0,0)))
E(g)$col <- dubnet$sc
V(g)$du <- c("D","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O")

png(file = "dubai_network.png", units="in", width=11, height=8.5, res=300)

plot(g, layout = layout.star, 
     edge.arrow.size = .5, 
     vertex.label.dist = .5,
     vertex.label.color = "black",
     vertex.size = 10,
     vertex.label.family = "Gill Sans MT",
     vertex.color=c("orange2","Darkgreen")[1+(V(g)$du == "D")],
     edge.color =c("green3","red2")[1+(E(g)$col == "1")])

dev.off()