################################################################################
#
#  REGIONAL RAIL PROXIMITY DUMMIES FOR LONGLAT DATA (MANY LARGE FILES)
#  BY COLIN WICK
#  LAST EDIT 3/2/2021
#
################################################################################

n_file <- length(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/"))

library(rgdal)
library(tidyverse)
library(lubridate)
library(rgeos)

routes <- readOGR("WorkHobby/Freelance/AlphaVu/1.28 metrolink/Shapefiles/2019SCRRALines/2019SCRRALines.shp")

for(f in c(1:n_file)){
  
  ######
  # Read CSV and only select for key and lon/lat columns to reduce time of calcs
  ######
  
  mydf <- read.csv(paste("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/",
                         list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/")[f],
                         sep=''))
  
  print(paste('loaded vf',f,"of 12"))
  
  
  # Unfortunately we have to drop records without a long-lat code 
  mydf <- mydf[!is.na(mydf$lon),]
  mydf <- mydf[,c("av_vtr_id","lon","lat")]
  mydf_coords <- SpatialPointsDataFrame(data=mydf,
                                        coords = SpatialPoints(mydf[,c("lon","lat")]))
  
  mydf_coords@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #### Route Coding
  
  cl <- length(routes$Route)

  for(i in c(1:cl)){
    catch_i <- routes[i,]
    
    # Grab 1 district
    # Plot (you can comment this out for speed)
    dist <- catch_i$Route
    catch_i <- spTransform(x = catch_i,CRS("+proj=utm"))
    catch_i <- buffer(catch_i,width = 1610)
    catch_i <- spTransform(x = catch_i,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    inbounds <- data.frame(over(catch_i,mydf_coords,returnList = TRUE))
    names(inbounds) <- c("av_vtr_id","lon","lat")
    mydf_coords[c(1:nrow(mydf_coords)),length(names(mydf_coords))+1] <- ifelse(mydf_coords$av_vtr_id %in% inbounds[,1],1,0)
    print(paste(i,"of",cl))
 }
summary(mydf_coords)
names(mydf_coords)[c(4:11)] <- levels(factor(routes$Route))
names(mydf_coords)
  
  end_directory <- "WorkHobby/Freelance/AlphaVu/1.28 metrolink/FINISHED FILES/Rail Lines/"
  file_name <- substr(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats")[f],12,str_length(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/")[f])-4)
  
  mydf_coords <- data.frame(mydf_coords)
  mydf_coords <- mydf_coords[,c(1:11)]
  
  write.csv(mydf_coords,
            paste(end_directory,
                  file_name,
                  "raillines.csv",
                  sep="_"),
            row.names = FALSE,
            fileEncoding = "utf-8")
  
  print(summary(mydf_coords))
}

