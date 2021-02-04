library(rgdal)
library(tidyverse)

data <- read.csv("Side Hustles/Mapping/Salt Lake/Inc_and_Comm.csv")
shape <- readOGR("Side Hustles/Mapping/Salt Lake/tl_2019_49_tract.shp","tl_2019_49_tract")

shape$TRACTCE
head(data)
names(data)[1] <- "TRACT"

shape <- subset(shape,shape$COUNTYFP == "035")
data$AVG_TT <- as.numeric(as.character(data$AVG_TT))
data$MED_INC <- as.numeric(as.character(data$MED_INC))
data$AVG_INC <- as.numeric(as.character(data$AVG_INC))
data$PCT60 <- as.numeric(as.character(data$PCT60))
data$PCT45 <- as.numeric(as.character(data$PCT45))

str(data)

merged <- merge(shape,data,by.x = "TRACTCE",by.y = "TRACT")

spplot(merged,"AVG_INC")
writeOGR(merged,"Side Hustles/Mapping/Salt Lake/Finished File/Salt_Lake_data","Salt_Lake_data",driver = "ESRI Shapefile")
