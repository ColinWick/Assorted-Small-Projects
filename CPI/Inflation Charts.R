library(tidyverse)
library(lubridate)
library(zoo)
library("ggthemes")

infl <- read.csv("../Downloads/CPIAUCNS.csv")

str(infl)

infl %>% 
  ggplot() +
  geom_point(aes(DATE,CPIAUCNS))

infl <- infl %>% 
  mutate(CPI_1y = rollmean(x = CPIAUCNS,k = 12,fill = NA),
         CPI_2y = rollmean(x = CPIAUCNS,k = 24,fill = NA),
         CPI_6m = rollmean(x = CPIAUCNS,k = 6,fill = NA),
         rCPI_1y = ((CPIAUCNS - lag(CPIAUCNS,n = 12))/CPIAUCNS*100))

infl %>%
  group_by(year(DATE)) %>%
  summarize(avg_CPI = mean(CPIAUCNS)) %>%
  mutate(rCPI = (avg_CPI - lag(avg_CPI))/avg_CPI*100,
         CPIyear = `year(DATE)`) %>%
  ggplot()+
  geom_line(aes(CPIyear,rCPI))+
  ggplot2::scale_y_continuous(breaks = c(-16:16))+
  geom_hline(yintercept = 2,size=2,alpha=.2)

infl %>% 
  #filter(month(DATE) %in% c(3,9)) %>%
  filter(year(DATE) >= 2005) %>%
  ggplot()+
  geom_smooth(aes(as.Date(DATE),rCPI_1y),method = "loess",se = FALSE)+
  geom_line(aes(as.Date(DATE),rCPI_1y),size=1)+
  geom_hline(yintercept = 2,size=2,alpha=.2)+
  xlab("month")+
  ylab("YoY CPI")+
  theme_minimal(base_size = 15)
