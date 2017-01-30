setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

##TIMELINE by DATABASE/SEIZURELOC##

library(dplyr)
library(ggplot2)

full <- read.csv("transittots_ratio.csv")

smug <- subset(full[1:773,])

Nigeria <- data.frame(subset(smug, Origin.Country1.1 == "Nigeria", select = c(Seizure.Country.1, Year)))

Nigeria$seiz <- Nigeria$Seizure.Country.1 == "Nigeria"

stack <- data.frame(table(Nigeria$Year, Nigeria$seiz))

png(file = "nigeria_timeline.png", units="in", width=5, height=4, res=300)

ggplot(data = stack, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkred", "darkgreen"), labels = c("Seized Elsewhere", "Seized in Nigeria")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .75),
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

dev.off()

###NETWORK

full <- read.csv("transittots.csv")

ninet <- data.frame(subset(full, Origin.Country1 == "Nigeria"))

write.csv(ninet, file = "nigerianet.csv")

##EDITED IN EXCEL##

ninet <- read.csv("nigerianet1.csv")

library(igraph)

g <- graph.data.frame(ninet, directed = T)
par(mar=(c(0,0,0,0)))

E(g)$col <- ninet$sc
V(g)$ni <- c("N","N","O","O","N","O","O","O","O","O","O","O","O","O")

png(file = "nigeria_network.png", units="in", width=11, height=8.5, res=300)

plot(g, layout = layout.kamada.kawai, 
     edge.arrow.size = .5, 
     vertex.label.dist = .5,
     vertex.label.color = "black",
     vertex.size = 10,
     vertex.color=c("orange2","Darkgreen")[1+(V(g)$ni == "N")],
     vertex.label.family = "Gill Sans MT",
     edge.color =c("green3","red2")[1+(E(g)$col == "1")])

dev.off() 