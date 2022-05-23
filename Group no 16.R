install.packages("readxl")
install.packages("BSDA")
library(readxl)
library(BSDA)

Natural_Disasters_INDIA <- read_excel("C:/Users/himan/Downloads/Natural_Disasters_INDIA.xlsx")
View(Natural_Disasters_INDIA)

ggplot(df, aes(df$Year, df$ People dies))+ggtitle("People died in respective years")+ylab ("People died")+ xlab("Years")+theme(plot,title= element_text(color="black", size=16,fcae="bold.italic",hjust=0.5),axis.title.x=element_text(color="black",size=16, face="bold"),axis.title.y=element_text(color="#993333",size=16,face="bold"))

ggplot(df, aes(df$Title, df$ Govt. Budget(in lac) ))+ggtitle("how much govt spent at disater place")+ xlab("Places")+ y lab("Budget (in lac)")+theme(plot,title= element_text(color="black", size=16,fcae="bold.italic",hjust=0.5),axis.title.x=element_text(color="black",size=16, face="bold"),axis.title.y=element_text(color="#993333",size=16,face="bold"))

ggplot(df, aes(df$Years, df$ Title ))+ggtitle("Places wrt years")+ xlab("Title")+ y lab("Years")+theme(plot,title= element_text(color="black", size=16,fcae="bold.italic",hjust=0.5),axis.title.x=element_text(color="black",size=16, face="bold"),axis.title.y=element_text(color="#993333",size=16,face="bold"))

mode(Year)
mean(People died)
mean(Affected)
mean(Govt. Budget in (in lac))

ggplot(df , aes(x = Duration, y = Govt. Budget (in lac))) +    geom_bar(stat = "identity", fill = "darkorchid4") +
facet_wrap( ~ Year, ncol = 1)+
labs(title = "duration when govt supplied items and repaired damages",
subtitle = "Data plotted by year",y = "budget ",x = "Duration") + theme_bw(base_size = 9)

#T_test
  
sd(affected)
mean(affected)
affected <- c(rnorm(2000, mean = 150, sd = 10))
affected
View(affected)

sd(died)
mean(died)
died <- c(rnorm(2000, mean = 144, sd = 9))
died
View(died)

t_test <- t.test(affected, died, paired = TRUE)
t_test
View(t_test) 
t <- (Natural_Disasters_INDIA$affected,Natural_Disasters_INDIA$died, alternative="greater", conf.level = 0.95)
plot(t)

#Z_test
  
z_affected <- affected
z_died <- died
Z_test <- z.test(x=z_affected, y=z_died, mu=0, sigma.x=15, sigma.y=15)
z_test
View(z_test)