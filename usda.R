setwd("E:/ROUTES/Year One Airport Seizure Analysis/R")

usda <- c("Afghanistan", "Albania", "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Burkina Faso", "Cambodia", "Cameroon", "Djibouti", "Egypt", "Ghana", "Hong Kong", "India", "Indonesia", "Iran", "Iraq", "Israel", "Ivory Coast", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Laos", "Lebanon", "Libya", "Macau", "Malaysia", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan", "Palestine", "China", "Romania", "Russia", "Saudi Arabia", "South Africa", "South Korea", "South Sudan", "Sudan", "Taiwan", "Thailand", "Togo", "Turkey", "Ukraine", "Vietnam")

trans <- read.csv("transittots.csv")
trans <- data.frame(subset(trans, Database == "Birds"))

origin <- as.character(trans$Origin.Country1)
origintable <- table(origin)

test <- NULL

for(j in 1:length(usda)){
  country <- usda[j]
  count <- data.frame(origintable[names(origintable) == country])
  test <- rbind(test, count)
}

names(test) <- c("count")
sum(test$count)



library(dplyr)

instance <- NULL

for(j in 1:length(usda)){
  country <- usda[j]
  a <- dplyr::filter(trans, Origin.Country1 == country)
  instance <- rbind(instance, a)
}

which(instance$Destination.Country1 == "USA")
