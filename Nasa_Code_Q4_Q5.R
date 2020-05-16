library(forecast)
library(dplyr)
library(lubridate)
library(ggplot2)

nasa_Data<-read.csv("C:/Users/neela/Desktop/Nasa_Clean.csv", header=TRUE, sep=",")

nasa_Data$date<-as.Date(ymd(nasa_Data$date))

nasa_Data$Year<-year(nasa_Data$date)

nasa_Data<-nasa_Data%>%
  filter(Year<2020)

Nasa_YearlyAvg<-nasa_Data%>%
  group_by(Year)%>%
  summarise(Average_Anamoly=mean(Avg_Anomaly_deg_C),Max_Anamoly=max(Avg_Anomaly_deg_C),Min_Anamoly=min(Avg_Anomaly_deg_C),Variance=var(Avg_Anomaly_deg_C),SD=sd(Avg_Anomaly_deg_C))%>%
  mutate(Range=Max_Anamoly-Min_Anamoly)%>%
  mutate(CT=Average_Anamoly/SD)

p1 <- ggplot() + geom_line(aes(x = Year, y = Average_Anamoly),
                           data = Nasa_YearlyAvg)

p1

p2 <- ggplot() + geom_line(aes(x = Year, y = SD),
                           data = Nasa_YearlyAvg)

p2

p3 <- ggplot() + geom_line(aes(x = Year, y = Range),
                           data = Nasa_YearlyAvg)

p3


p4 <- ggplot(Nasa_YearlyAvg, aes(x=Average_Anamoly, y=Year)) + 
  geom_boxplot() +
  facet_wrap(~Year, scale="free")

p4

write.csv(Nasa_YearlyAvg,"Nasa_YearlyData.csv",row.names = F)
