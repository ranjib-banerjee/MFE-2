# MFE-2
Undergraduate Statistics Project codes
Group 15
Cse - 1 R studio Code
R codes:



T-TEST:
R CODE:
# defining sample vector
> x<-rnorm(100)
>
> one sample T-Test
t.test(x, mu = 5)

codes used for analyzing the data:

1)	
 df1<-read.csv("type11.csv")
df1
av=as.numeric(df1$No.of.companies)
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
hist(a,ylim = c(0,30),col = blues9)


2)

df1<-read.csv("location11.csv")
df1
av=as.numeric(df1$No.of.companies)
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
hist(a,ylim = c(0,30),col = blues9)


code used for barplot:

library(corrplot)
## corrplot 0.84 loaded
corrplot(corr=cor(disclosedfunding.df[,c(4,5,6,9)]),method="ellipse")

code used for small graph:
barplot(yeartable, xlab="Year", ylab = "No. of Startups which received funding", col="lightblue" )
