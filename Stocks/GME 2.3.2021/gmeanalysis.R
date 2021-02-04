library("lubridate")
library("tidyquant")
library("tidyverse")
library("ggplot2")


gmeshort <- read.csv("../Downloads/regshodaily_2021_02_03_15_16_27.csv")
gmeshort$Date <- as.Date(gmeshort$Date,format="%Y-%m-%d")


gmeprice <- read.csv("../Downloads/GME.csv")
gmeprice$Date <- as.Date(gmeprice$Date,format="%Y-%m-%d")
dates <- gmeprice$Date

gmeshort %>%
  group_by(Date,Symbol) %>%
  summarize(tot_vol = sum(Total.Volume),
            tot_short = sum(Short.Volume),
            short_vol = tot_short/tot_vol) %>%
  ggplot()+
  geom_line(aes(x=Date,y=short_vol),color="red",size=2)

dates <- as_date(c((today()-365):today()))


#dates <- bizdays::bizseq(from = (today()-365),to=today())
#dates <- dates[weekdays(dates) %in% c("Friday","Monday","Thursday","Tuesday","Wednesday")]

#gme <- c()

for(i in c(1:length(dates))){
  print(dates[i])
  gme <- rbind(gme,
                read.csv(paste("http://regsho.finra.org/CNMSshvol",format.Date(dates[i],"%Y%m%d"),".txt",sep = ""),
         sep = "|") %>%
          filter(Symbol == "GME"))
print(gme[i,])
}

gme <- read.csv("WorkHobby/Stocks/GME 2.3.2021/gmeshorts.csv")

gme <- rbind(gme,
             read.csv(paste("http://regsho.finra.org/CNMSshvol",format.Date(today(),"%Y%m%d"),".txt",sep = ""),
                      sep = "|") %>%
               filter(Symbol == "GME"))

gme %>%
  ggplot()+
  geom_line(aes(Date,ShortVolume,color="short_volume"),linetype="solid",n = 7,size=1)+
  geom_line(aes(Date,TotalVolume,color="volume"),linetype="solid",n = 7,size=1)+
  scale_x_date(limits=as.Date(c("2020-10-01","2021-02-04")),date_breaks = "3 weeks")+
  theme_minimal()


gmeprice %>%
  ggplot()+
  geom_line(aes(Date,Open,color="open"),size=1)+
  geom_line(aes(Date,Close,color="close"),size=1)+
  scale_x_date(limits=as.Date(c("2020-10-01","2021-02-04")),date_breaks = "3 weeks")+
  theme_minimal()
  

#write.csv(gme,"WorkHobby/Stocks/GME 2.3.2021/gmeshorts.csv",row.names = FALSE)
