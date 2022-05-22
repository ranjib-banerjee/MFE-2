CODE FOR PLOTTING BAR GRAPH USING R
state1<-c("Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh","Goa","Gujarat","Haryana", "Himachal Pradesh","Jammu & Kashmir","Jharkhand","Karnataka","Kerala","Madhya Pradesh","Maharashtra")
rain2014<-c(695,10,189,609,718,222,815,300,86,90,488,1827,2903,1086,359)
rain2016<-c(1456,30,528,939,1279,529,759,1656,136,77,859,3475,  6902,3931,1958)
x<-rain2014
y<-rain2016
barplot(x, names.arg = state1, col = "red",main="Bar Graph of Road accidents in 2014")
barplot(y, names.arg = state1, col = "yellow",main="Bar Graph of Road accidents in 2016")

state1<-c("Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh","Goa","Gujarat","Haryana", "Himachal Pradesh","Jammu & Kashmir","Jharkhand","Karnataka","Kerala","Madhya Pradesh","Maharashtra")
rain2014<-c(695,10,189,609,718,222,815,300,86,90,488,1827,2903,1086,359)
rain2016<-c(1456,30,528,939,1279,529,759,1656,136,77,859,3475,  6902,3931,1958)
x<-rain2014
y<-rain2016
pie(x,state1,main = "Representation of accidents in 2014")
pie(y,state1,main = "Representation of accidents in 2016)


CODE FOR LINE PLOT IN COMPARISON OF TWO DATA

state1<-c("Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh","Goa","Gujarat","Haryana", "Himachal Pradesh","Jammu & Kashmir","Jharkhand","Karnataka","Kerala","Madhya Pradesh","Maharashtra")
rain2014<-c(695,10,189,609,718,222,815,300,86,90,488,1827,2903,1086,359)
rain2016<-c(1456,30,528,939,1279,529,759,1656,136,77,859,3475,  6902,3931,1958)
x<-rain2014
y<-rain2016
plot(x, type = "l", col = "blue",ylab = "Accidents",main = "comparison between 2014 and 2016")
lines(y, type="l", col = "red")

years<-c(2013,2014,2015,2016,2017,2018,2019,2020)
Total_Number_of_RoadAccidents<-c(520300,501423,480652,464910,467044,449002,320496,410298)
Total_Number_of_PersonsKilled<-c(130894,146133,150785,147913,151417,151113,79342,100934)
Total_Number_of_PersonsInjured<-c(576890,500279,494624,470975,469418,451361,259256,305182)
plot(years,Total_Number_of_RoadAccidents,col="red",main = "No.of persons affected due to Road accidents")
lines(years,Total_Number_of_RoadAccidents,col="blue")
plot(years,Total_Number_of_PersonsKilled,col="blue",main = "No.of persons died due to Road accidents")
lines(years,Total_Number_of_PersonsKilled,col="red")
plot(years,Total_Number_of_PersonsInjured,col="pink",main = "No.of persons injured due to Road accidents")
lines(years,Total_Number_of_PersonsInjured,col="purple")

CODE FOR HYPOTHESIS TESTING:


rainacc2014<-c(695,10,189,609,718,222,815,300,86,90,488,1827,2903,1086,359)
rainacc2016<-c(1456,30,528,939,1279,529,759,1656,136,77,859,3475,  6902,3931,1958)
# null hypothesis 
#       Ho :  mean of rainacc2014 = mean of rainacc2016
# alternating hyphotesis  
#       Ha : mean of rainacc2014 < mean of rainacc2016

# t testing
t.test(rainacc2016,alternative = 'greater', mu=mean(rainacc2014))

Anova test:
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
acc.data<-read.csv("C:/Users/dveer/Downloads/Acc_Classified_according_to_Type_of_Weather_Condition_2014_and_20161.csv",header = TRUE,colClasses = c("numeric","numeric"))
summary(acc.data)
one.way<-aov(TotalrainAcc2014~TotalrainAcc2016,data=acc.data)
summary(one.way)
