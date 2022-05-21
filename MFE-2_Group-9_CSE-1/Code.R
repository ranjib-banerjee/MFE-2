A= c(62,55,54,36,35,27,24,21,17,13,12,9,9,7,5,4,1)
B=c("KA","MH","KL","AP","MP","TR","TN","WB","GJ","RJ","HR","AS","OR"
,"HP","PB","UP","BR")
barplot(A, names.arg = B, xlab ="States",
ylab ="Rural population(in million)", col =rainbow(17),
main ="Suicides per million rural population",ylim=c(0,70))
legend("topright",legend=c("KA:Karnatak","MH:Maharashtra","KL:Kerela","AP:
AndhraPradesh","MP:MadhyaPradesh","TR:Tripura","TN:TamilNadu","WB:West 
Bengal","GJ:Gujarat","RJ:Rajasthan","HR:Haryana","AS:Assam","OR:Orissa","H
P:HimachalPradesh","PB:Punjab","UP:UttarPradesh","BR:Bihar"),fill=rainbow(17
))

v<-
c(10720,13729,13622,16015,16082,16603,16415,17971,17164,18241,17131,1706
0,16632,16796,17368,15964,14027,13754,11772,12360,12602,11379)
x<-
c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,20
09,2010,2011,2012,2013,2014,2015,2016)
 
data <- data.frame(x,v)
# Plot
ggplot(data, aes(x=x, y=v)) + geom_line()

library(readxl) data <- read_excel("data.xlsx") View(data) mean(data$`Total Farmer 
Suicides`)
[1] 15154.86 summary(data)
 Year Total Farmer Suicides
Min. :1995 Min. :10720 
1st Qu.:2000 1st Qu.:13649 
Median :2006 Median :16048 
Mean :2006 Mean :15155 
3rd Qu.:2011 3rd Qu.:16994 
Max. :2016 Max. :18241

library(plotrix)
percentages <- c(38.7, 19.5, 11.7, 10.5, 4.1, 2, 1.1 , 1.1)
labels <- c("Debt", "Farming related issues", "Family Problems", "Illness","Drug 
Abuse","Marriage related issues","Poverty","Property dispute")
piepercent<- round(100 * percentages / sum(percentages), 1)
pie3D(percentages, labels = piepercent, main = "Major Causes of suicide among 
Farmers", col = rainbow(length(percentages)))
legend("topright", c("Debt", "Farming related issues", "Family Problems", 
"Illness","Drug Abuse","Marriage related issues","Poverty","Property dispute"),
 cex = 0.5, fill = rainbow(length(percentages)))
 
 df <- data.frame(value = c(37,27.7,0.6,10,24.7),group = paste0("G", 1:5))
library(ggplot2)
library(dplyr)
# Hole size
hsize <- 3
df <- df %>% 
 mutate(x = hsize)
ggplot(df, aes(x = hsize, y = value, fill = group)) +geom_col(color = "black") +
 geom_text(aes(label = value),position = position_stack(vjust = 0.5)) +
coord_polar(theta = "y") + xlim(c(0.2, hsize + 0.5)) +scale_fill_discrete(labels = 
c("30-44", "14-29", "upto 14", "60 and above","45-59"))

df <- data.frame(value = c(34.4,31.8,3.1,5.4,17.4),group = paste0("G", 1:5))
library(ggplot2)
library(dplyr)
# Hole size
hsize <- 3
df <- df %>% 
 mutate(x = hsize)
ggplot(df, aes(x = hsize, y = value, fill = group)) +geom_col(color = "black") 
+geom_text(aes(label = value), position = position_stack(vjust = 0.5)) 
+coord_polar(theta = "y") +
xlim(c(0.2, hsize + 0.5)) +scale_fill_discrete(labels = c("30-44", "14-29", "upto 
14", "60 and above","45-59"))

library(readxl)
Book_2 <- read_excel("Book 2.xlsx")
View(Book_2)

a 
=c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2
015,2016);
b = 
c(0.120,0.150,0.111,0.146,0.106,0.032,0.036,0.043,0.093,0.068,0.052,0.031,0.002,
0.001,0.002,0.007)
barplot(b,names.arg=a,xlab="Year",ylab="P(x) of 
suicides",col="blue",main="Total Number of Suicides of age group 0-29 for 
Males")

a = 
c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,20
15,2016);
b = 
c(0.079,0.118,0.120,0.156,0.122,0.026,0.028,0.045,0.106,0.074,0.071,0.035,0.001,
0.003,0.004,0.005)
barplot(b,names.arg=a,xlab="Year",ylab="P(x) of 
suicides",col="blue",main="Total Number of Suicides of age group 0-29 for 
females")

a= 
c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,20
15,2016)
b = 
c(0.077,0.136,0.092,0.145,0.116,0.045,0.051,0.053,0.100,0.0410,0.060,0.049,0.01
1,0.008,0.008,0.008)
barplot(b,names.arg=a,xlab="Year",ylab="P(x) of 
suicides",col="blue",main="Total Number of Suicides of age group 30-59")

a= 
c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,20
15,2016);
b = 
c(0.071,0.082,0.089,0.13,0.10,0.057,0.059,0.067,0.089,0.030,0.064,0.052,0.028,0.
018,0.022,0.019)
barplot(b,names.arg=a,xlab="Year",ylab="P(x) of 
suicides",col="blue",main="Total Number of Suicides of age group 30-59 for 
females")

a= 
c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,20
15,2016);
b = 
c(0.088,0.084,0.083,0.168,0.141,0.030,0.035,0.035,0.101,0.050,0.060,0.070,0.023,
0.013,0.015,0.006)
barplot(b,names.arg=a,xlab="Year",ylab="P(x)of 
suicides",col="blue",main="Total Number of Suicides of age group 60 and above")

a= 
c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,20
15,2016);
b = 
c(0.092,0.055,0.98,0.13,0.20,0.012,0.012,0.012,0.117,0.018,0.074,0.030,0.049,0.0
30,0.055,0)
barplot(b,names.arg=a,xlab="Year",ylab="P(x) of 
suicides",col="blue",main="Total Number of Suicides of age group 60 and above 
for females")
link for dataset - https://ncrb.gov.in/en/accidental-deaths-suicides-in-india
