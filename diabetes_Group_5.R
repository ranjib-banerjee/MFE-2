library(readr)
library(faraway)
library(ggplot2)
library(TeachingDemos)
library(ggpubr)
library(nhstplot)
library(IPSUR)
library(HH)
library(webr)
library(BSDA)
library(EnvStats)

diabetes<-read.csv("C:\\Users\\ASUS\\Downloads\\archive (14)\\diabetes.csv")
head(diabetes)
summary(diabetes)

#task 1
#distribution of insulin differs between those testing positive and negative.
diabetes$Outcome.f <- factor(diabetes$Outcome)
levels(diabetes$Outcome.f) <- c("negative","positive")

# plot insulin vs diabetes
plot(Insulin ~ Outcome.f, diabetes, xlab="diabetes",col="pink")

ggplot(diabetes, aes(x=Insulin, y=..density.., color=Outcome.f))+
  geom_histogram(position='dodge',binwidth = 30, fill="white")+geom_density(alpha=.2, fill="#FF6666")

#Can we say that two variables (Glucose and BMI), predict presence of diabetes in a person ?
mean(diabetes$Glucose)
sd(diabetes$Glucose)

ggplot(diabetes, aes(x=Insulin, y=..density..,color=Insulin)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=Age, y=..density..,color=Age)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=Glucose, y=..density..,color=Glucose)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=BloodPressure, y=..density..,color=BloodPressure)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=BMI, y=..density..,color=BMI)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=SkinThickness, y=..density..,color=SkinThickness)) +
  geom_histogram(position='dodge',binwidth = 30, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

ggplot(diabetes, aes(x=DiabetesPedigreeFunction, y=..density..,color=DiabetesPedigreeFunction)) +
  geom_histogram(position='dodge',binwidth = 5, fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

diabetes$Outcome <- as.factor(diabetes$Outcome)

preg_dp <- ggplot(diabetes, aes(x = Pregnancies, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

gluc_dp <- ggplot(diabetes, aes(x = Glucose, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

bmi_dp <- ggplot(diabetes, aes(x = BMI, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

dpf_dp <- ggplot(diabetes, aes(x = DiabetesPedigreeFunction, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

bp_dp <- ggplot(diabetes, aes(x = BloodPressure, fill = Outcome)) +
  geom_density(size = 1, alpha = .5)

ggarrange(preg_dp, gluc_dp, bmi_dp, dpf_dp, bp_dp, ncol = 3,nrow=2)


#hypothesis testing

-qnorm(0.95)
pnorm(-1.680919)

df1 <- subset(diabetes, diabetes$Outcome==1, select = c("Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age","Outcome"))
nrow(df1)


df2 <- subset(diabetes, diabetes$Outcome==0, select = c("Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age","Outcome"))
nrow(df2)

b<-mean(diabetes$Glucose)
b
a<-t.test(df1$Glucose,mu=b,alternative = "greater")
plot(a)



f<-mean(diabetes$Insulin)
g<-sd(diabetes$Insulin)
#One Sample Tests for Means and Variances
#mean 
z.test(df1$Insulin, mu = f , sigma.x = g, conf.level = 0.95)
plotztest(2.9172)

chisq.test(diabetes$Glucose)

#variance
h<-var(diabetes$Glucose)
v1<-varTest(diabetes$Glucose,alternative ="less",sigma.squared = h)
v1
