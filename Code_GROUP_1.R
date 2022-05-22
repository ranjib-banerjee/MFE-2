library(plotly)

data1 <- read.csv("C:/Users/Arham/Documents/stormset.csv")
data2 <- read.csv("C:/Users/Arham/Documents/floodsetc.csv")
data3 <- read.csv("C:/Users/Arham/Documents/earthquakesetc.csv")
data4 <- read.csv("C:/Users/Arham/Documents/Droughtdatasetc.csv")

#ALL FOUR 
fig <- plot_ly(x= c(1960,1970,1980,1990,2000,2010), y = data1$Number_of_deaths_from_storms,name = "Strom",type = "bar",xlab = data1$Year)
fig <- fig %>% layout(title = "NUMBER OF DEATHS", yaxis = list(title = "Number of Deaths"),xaxis = list(title = "YEAR"))
fig <- fig %>% add_trace(y = data2$Number_of_deaths_from_floods, name = 'Flood')
fig <- fig %>% add_trace(y = data3$Number.of.deaths.from.earthquakes, name = 'Earthquake')
fig <- fig %>% add_trace(y = data4$Number_of_deaths_from_drought, name = 'Drought')
fig

# THREE NUMBER OF DEATHS
#ALL FOUR 
fig2 <- plot_ly(x= c(1960,1970,1980,1990,2000,2010), y = data1$Number_of_deaths_from_storms,name = "Strom",type = "bar",xlab = data1$Year)
fig2 <- fig2 %>% layout(title = "NUMBER OF DEATHS", yaxis = list(title = "Number of Deaths"),xaxis = list(title = "YEAR"))
fig2 <- fig2 %>% add_trace(y = data2$Number_of_deaths_from_floods, name = 'Flood')
fig2 <- fig2 %>% add_trace(y = data3$Number.of.deaths.from.earthquakes, name = 'Earthquake')
fig2

#ALL FOUR ECONOMICAL DAMAGE 
fig3 <- plot_ly(x= c(1960,1970,1980,1990,2000,2010), y = data1$Total.economic.damages.from.storms,name = "Strom",type = "bar",xlab = data1$Year)
fig3 <- fig3 %>% layout(title = "Total economic damages", yaxis = list(title = "AMOUNT"),xaxis = list(title = "YEAR"))
fig3 <- fig3 %>% add_trace(y = data2$Total.economic.damages.from.floods, name = 'Flood')
fig3 <- fig3 %>% add_trace(y = data3$Total.economic.damages.from.earthquakes, name = 'Earthquake')
fig3 <- fig3 %>% add_trace(y = data4$Total.economic.damages.from.drought, name = 'Drought')
fig3

#ALL FOUR ECONOMICAL DAMAGE 
fig4 <- plot_ly(x= c(1970,1975,1980,1985), y = c(0.75,4.31,8.58,24.50),name = "Andhra Pradesh",type = "bar",xlab = c(1970,1975,1980,1985))
fig4 <- fig4 %>% layout(title = "Annual Amount of Margin Money For Differnt State", yaxis = list(title = "Amount"),xaxis = list(title = "YEAR"))
fig4 <- fig4 %>% add_trace(y = c(0.48,1.25,3.46,7.25), name = 'Assam')
fig4 <- fig4 %>% add_trace(y = c(1.50,4.61,13.08,33.75), name = 'Bihar')
fig4 <- fig4 %>% add_trace(y = c(0.80,4.55,9.56,28.75), name = 'Gujarat')
fig4 <- fig4 %>% add_trace(y = c(1.55,1.24,1.47,4.50), name = 'Haryana')
fig4 <- fig4 %>% add_trace(y = c(0.40,0.35,1.30,1.50), name = 'Jammu&Kashmir')
fig4 <- fig4 %>% add_trace(y = c(0.10,0.30,1.59,5.00), name = 'Kerala')
fig4 <- fig4 %>% add_trace(y = c(0.80,3.41,1.83,4.75), name = 'Madhya Pradesh')
fig4 <- fig4 %>% add_trace(y = c(0.86,4.17,4.75,7.25), name = 'Maharashtra')
fig4 <- fig4 %>% add_trace(y = c(0.44,0,0,0), name = 'Mysore')
fig4 <- fig4 %>% add_trace(y = c(0,0.02,0.14,0.25), name = 'Nagaland')
fig4 <- fig4 %>% add_trace(y = c(1.25,3.58,8.71,26.25), name = 'Orissa')
fig4 <- fig4 %>% add_trace(y = c(0.41,0.33,2.68,6.00), name = 'Punjab')
fig4 <- fig4 %>% add_trace(y = c(1.08,10.19,0.01,0.25), name = 'Rajasthan')
fig4 <- fig4 %>% add_trace(y = c(0.50,1.52,8.59,8.75), name = 'Tamil Nadu')
fig4 <- fig4 %>% add_trace(y = c(0.94,2.18,10.80,32.50), name = 'Uttar Pradesh')
fig4 <- fig4 %>% add_trace(y = c(2.61,6.61,13.60,23.75), name = 'West Bengal')
fig4

data5 <- read.csv("C:/Users/Arham/Downloads/kerala.csv")
#check for empty column
colSums(is.na(data5) | data5 == "")
#replacing yes with 1 and no with 0
data5$FLOODS<-ifelse(data5$FLOODS=="YES",1,0)
#data5$FLOODS

#KNN
library(e1071)
library(caTools)
library(class)

split <- sample.split(data5, SplitRatio = 0.5)
train_cl <- subset(data5, split == "TRUE")
test_cl <- subset(data5, split == "FALSE")

train_scale <- scale(train_cl[, 2:5])
test_scale <- scale(test_cl[, 2:5])

classifier_knn <- knn(train = train_scale,test = test_scale,cl = train_cl$FLOODS,k = 2)
classifier_knn

cm <- table(test_cl$FLOODS, classifier_knn)
cm

misClassError <- mean(classifier_knn != test_cl$FLOODS)
print(paste('Accuracy =', 1-misClassError))

#LOGESTIC REGRESSION
library(dplyr)
library(ROCR)

split <- sample.split(data5, SplitRatio = 0.8)
split

train_reg <- subset(data5, split == "TRUE")
test_reg <- subset(data5, split == "FALSE")

input <- data5[,c("JUN","JUL","AUG","SEP","FLOODS")]
FLOODS.data = glm(formula = FLOODS ~ + JUN+JUL+AUG+SEP, data = train_reg, family = binomial)
print(summary(FLOODS.data))

predict_reg <- predict(FLOODS.data,test_reg,type = "response")
predict_reg
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
predict_reg

missing_classerr <- mean(predict_reg != test_reg$FLOODS)
print(paste('Accuracy =', 1 - missing_classerr))

ROCPred <- prediction(predict_reg,test_reg$FLOODS) 
ROCPer <- performance(ROCPred, measure = "tpr",x.measure = "fpr")
plot.new()

plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)


predict_reg2 <- predict(FLOODS.data,data5,type = "response")
predict_reg2 <- ifelse(predict_reg2 >0.5, 1, 0)
predict_reg2

# Defining sample vector
x <- rnorm(data5$FLOODS)
y <- rnorm(predict_reg2)
# Two Sample T-Test
t.test(x, y)
