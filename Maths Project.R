library(webr)
a <- read.csv("C:/Users/laksh/OneDrive/Desktop/data1.csv")

s1 <-  data1$N
mu1 <- mean(s1, na.rm = TRUE)
s2 <-  data1$P
mu2 <- mean(s2, na.rm = TRUE)


t.test(s1, s2 , alernative="less", conf.level = 0.95)
library(webr)
library(BSDA)
a <- read.csv("C:/Users/laksh/OneDrive/Desktop/data1.csv")


s1 <- data1$temperature
mu1 <- mean(s1, na.rm = TRUE)
std1 <- sd(s1)

s2 <- data1$rainfall
mu2 <- mean(s2, na.rm = TRUE)
std2 <- sd(s2)
z.test(s1,s2,sigma.x = std1 , sigma.y = std2 , conf.level = 0.95 , alternative = "greater")
input_data<-data1[,c('P','label')]
print(input_data)

boxplot(P~label,data=data1,xlab='label',ylab='P',main="Boxplot", col=c("red","blue"))
input_data<-data1[,c('ph','label')]
print(input_data)

boxplot(ph~label,data=data1,xlab='label',ylab='ph',main="Boxplot", col=c("green","purple"))
ss<-data1[1:800,c("temperature","rainfall")]
print(ss)

plot(x=data1$temperature,y=data1$rainfall,xlab="temperature",ylab="rainfall",main="Temp Vs Rainfall",col="purple")