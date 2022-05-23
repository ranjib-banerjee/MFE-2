library(readr)
library(dplyr)
library(car)
library(lattice)
library(ggplot2)
heart <- read_csv("heart.csv")
str(heart)
summary(heart)
heart$sex <- heart$sex %>% factor(levels=c(0,1),
                                  labels=c("Female","Male"))
heart$target <- heart$target %>% factor(levels=c(0,1),
                                        labels=c(0,1))

heart <- heart %>% rename(gender = sex)

str(heart)
heart_summary1 <- heart %>% group_by(`gender`) %>% summarise(Min = min(age,na.rm = TRUE),
                                                             Q1 = quantile(age,probs = .25,na.rm = TRUE),
                                                             Median = median(age, na.rm = TRUE),
                                                             Q3 = quantile(age,probs = .75,na.rm = TRUE),
                                                             Max = max(age,na.rm = TRUE),
                                                             Mean = mean(age, na.rm = TRUE),
                                                             SD = sd(age, na.rm = TRUE),
                                                             n = n(),
                                                             Missing = sum(is.na(target)))
heart %>% histogram(~target | gender, data= ., main = "Risk of heart attack")
heart %>% histogram(~age | gender, data= ., main = "Age of observations", breaks=10)
heart %>% ggplot(aes(x=age)) + geom_histogram(aes(y=..density..), colour="black")+
  geom_density(alpha=.2, fill="dodgerblue3")
heart_filtered <- heart %>% filter(target == 1)
table_age <- table(heart_filtered$gender, heart_filtered$age)

barplot(table_age, main="Bar plot of Age",
        ylab="Number of observations", xlab="Age",
        ylim=c(0,8),legend=row.names(table_age), beside=TRUE,
        args.legend=c(x="topright",horiz=FALSE,title="Gender"),
        col=c( "#FF66FF","#0099FF"))
boxplot(age ~ gender, data=heart_filtered)
heart_summary2 <- heart_filtered %>% group_by(gender) %>% summarise(Mean = round(mean(age, na.rm = TRUE),2),
                                                                    Min = min(age,na.rm = TRUE),
                                                                    Q1 = quantile(age,probs = .25,na.rm = TRUE),
                                                                    Median = median(age, na.rm = TRUE),
                                                                    Q3 = quantile(age,probs = .75,na.rm = TRUE),
                                                                    Max= max(age,na.rm=TRUE),
                                                                    n = n())
heart_summary2
heart_summary3 <- heart_filtered %>% group_by(gender) %>% summarise(Mean = round(mean(age, na.rm = TRUE),2),
                                                                    SD = round(sd(age, na.rm = TRUE),3),
                                                                    n = n(),
                                                                    tcrit = round(qt(p = 0.975, df = n - 1),3),
                                                                    SE = round(SD/sqrt(n),3),
                                                                    `95% CI Lower Bound` = round(Mean - tcrit * SE,2),
                                                                    `95% CI Upper Bound` = round(Mean + tcrit * SE,2))
heart_summary3
table_heart <- table(heart$target, heart$gender)
table_heart
table_heart %>% addmargins()
table_heart2 <- table_heart %>% prop.table(margin=2)
table_heart2
barplot(table_heart2, main="Bar plot For Male and Female",
        ylab="Proportion within Gender", xlab="Likelihood of heart attack",
        ylim=c(0,1),legend=row.names(table_heart2), beside=TRUE,
        args.legend=c(x="topleft",horiz=FALSE,title="Likelihood of heart attack"))
chi_heart <- chisq.test(table_heart, p=c(0.5,0.5))
chi_heart
chi_heart$expected
chi_heart$observed
heart_filtered <- heart %>% filter(target == 1)
heart_male <- heart_filtered %>% filter(gender=="Male") 
heart_male$age %>% qqPlot(dist="norm", xlab = "Age of Male")
heart_female <- heart_filtered %>% filter(gender=="Female")
heart_female$age %>% qqPlot(dist="norm", ylab = "Age of Female")
leveneTest(age ~ gender, data = heart)
result <- t.test(age ~ gender, data=heart_filtered,
                 var.equal=TRUE, alternative ="two.sided")

result
result$conf.int
result$p.value