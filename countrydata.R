## To extract data for specific countries

setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

library(dplyr)
library(ggplot2)

full <- read.csv("transittots.csv")
transit <- colnames(full[,6:25])

x <- "South Korea"   ##INPUT THE COUNTRY HERE## 

newdata <- NULL

for(i in 1:length(transit)){
  a <- which(full[,i] == x)
  newdata <- c(a,newdata)
}

newdata <- unique(newdata)

tots <- read.csv("tot.csv")
export <- subset(tots, ID %in% newdata)
export <- export[,-1]

write.csv(export, paste(x,".csv", sep = ""))
