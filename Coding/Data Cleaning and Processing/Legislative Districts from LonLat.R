################################################################################
#
#  CONGRESSIONAL, STATE LEG, ETC DISTRICT DUMMIES FOR LONGLAT DATA (MANY LARGE FILES)
#  BY COLIN WICK
#  LAST EDIT 2/21/2021
#
################################################################################

n_file <- length(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/"))

library(rgdal)
library(tidyverse)
library(lubridate)

stateleg <- readOGR("WorkHobby/Freelance/AlphaVu/1.28 metrolink/Shapefiles/LegDist/tl_2018_06_sldl.shp")

statesen <- readOGR("WorkHobby/Freelance/AlphaVu/1.28 metrolink/Shapefiles/SenateDist/tl_2018_06_sldu.shp")

congdist <- readOGR("WorkHobby/Freelance/AlphaVu/1.28 metrolink/Shapefiles/CongDist/tl_2018_us_cd116.shp")
congdist <- subset(congdist,subset = as.factor(congdist$STATEFP) == "06")


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
mydf <- select(mydf,"av_vtr_id","lon","lat")
mydf_coords <- SpatialPointsDataFrame(data=mydf,
                          coords = SpatialPoints(mydf[,c("lon","lat")]))

mydf_coords@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

dist <- 1
    ### STATE ASSEMBLY DISTRICTS
    print("state assembly")

    mydf_coords$assem_dist <- 0
    cl <- length(stateleg$SLDLST)

    for(i in c(1:cl)){
        catch_i <- stateleg[i,]
        # Grab 1 district
        # Plot (you can comment this out for speed)
        plot(catch_i)
        dist <- catch_i$SLDLST
        catch_i <- spTransform(x = catch_i,
                               CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        inbounds <- data.frame(over(catch_i,mydf_coords,returnList = TRUE))
        mydf_coords$assem_dist[mydf_coords$av_vtr_id %in% inbounds[,1]] <- dist
        print(paste(i,"of",cl))
    }
    
    ### CONGRESSIONAL DISTRICTS
    print("congressional districts")
    
    mydf_coords$cong_dist <- 0
    cl <- length(congdist$CD116FP)
    
    for(i in c(1:cl)){
      catch_i <- congdist[i,]
      # Grab 1 district
      # Plot (you can comment this out for speed)
      plot(catch_i)
      dist <- catch_i$CD116FP
      catch_i <- spTransform(x = catch_i,
                             CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      inbounds <- data.frame(over(catch_i,mydf_coords,returnList = TRUE))
      mydf_coords$cong_dist[mydf_coords$av_vtr_id %in% inbounds[,1]] <- dist
      print(paste(i,"of",cl))
    }
    
    #### STATE SENATE
    print("state senate")
    
    mydf_coords$senate_dist <- 0
    cl <- length(statesen$SLDUST)
    
    for(i in c(1:cl)){
      catch_i <- statesen[i,]
      # Grab 1 district
      # Plot (you can comment this out for speed)
      plot(catch_i)
      dist <- catch_i$SLDUST
      catch_i <- spTransform(x = catch_i,
                             CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      inbounds <- data.frame(over(catch_i,mydf_coords,returnList = TRUE))
      mydf_coords$senate_dist[mydf_coords$av_vtr_id %in% inbounds[,1]] <- dist
      print(paste(i,"of",cl))
    }

end_directory <- "WorkHobby/Freelance/AlphaVu/1.28 metrolink/FINISHED FILES/Legislative Districts/OnlyIDs/"
file_name <- substr(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/")[f],12,str_length(list.files("WorkHobby/Freelance/AlphaVu/1.28 metrolink/LonLats/")[f])-4)

mydf_coords <- select(data.frame(mydf_coords),"av_vtr_id","lon","lat","assem_dist","senate_dist","cong_dist")

write.csv(mydf_coords,
              paste(end_directory,
                    file_name,
                    "legdist.csv",
                    sep="_"),
              row.names = FALSE,
              fileEncoding = "utf-8")
print(head(select(mydf_coords,"assem_dist","senate_dist","cong_dist")))
}

