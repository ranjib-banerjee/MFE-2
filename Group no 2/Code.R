# linear regression using scatter plot
df1<-read.csv("C:/Users/Siri Sujala/Downloads/aksh project/State_Region_corrected.csv")
names(df1)
head(df1)
plot(df1$Area..km2., df1$National.Share....)


model<- lm(National.Share.... - Area..km2)
abline(model2)


# one sample t test
df1<-read.csv("C:/Users/Siri Sujala/Downloads/aksh project/State_Region_corrected.csv")
view(df1)
x=df1$Area..km2.;
mean(x);
sd(x);
t.test(x, mu = 8);

df1<-read.csv("C:/Users/Siri Sujala/Downloads/aksh project/State_Region_corrected.csv")
df1
av=as.numeric(df1$Area..km2.)
a=rep()
for (i in av)
{
  a=append(a,i)
}
m=sum(av)
m=m/length(av)
print("mean is : ")
print(m)
print(a)
hist(a,ylim = c(0,30),col = "#4682B4")  #histogram



hist(a,ylim = c(0,30),col = "#4682B4")
d=density(a)
plot(d)                               # density plot



df1<-read.csv("C:/Users/Siri Sujala/Downloads/aksh project/file_02.csv")
av=as.numeric(df1$Thermal.Generation.Actual..in.MU.)
a=rep()
for (i in av)
{
  a=append(a,i)
}
m=sum(av)
m=m/length(av)
print("mean is : ")
print(m)
print(a)
hist(a,ylim = c(0,1000),col = blues9)
