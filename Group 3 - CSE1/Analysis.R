library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
d <- read_csv("Documents/data4.csv")
d
head(d)
nrow(d)
meanLevel <- tapply(d$cardio, d$BMI,mean)
 
 
ggplot(data = d, aes(x = as.character(cardio), y = BMI)) +
  geom_jitter(color = "firebrick", size = 0.5, width = 0.15) +
  labs(x = "cardio", y = "BMI") +
  theme_classic()+
  ylim(0,40)
  
  
######################

library(prob)
library(distr)
library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
d <- read_csv("Documents/data4.csv")
d
head(d)
nrow(d)
meanLevel <- tapply(d$cardio, d$BMI,mean)
 
 
ggplot(data = d, aes(x = as.character(cardio), y = smoke)) +
  geom_jitter(color = "blue", size = 0.5, width = 0.15) +
  labs(x = "cardio", y = "smoke") +
  theme_classic()+
  ylim(0,1)
  
######################

library(prob)
library(distr)
library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
d <- read_csv("Documents/data4.csv")
d
head(d)
nrow(d)
meanLevel <- tapply(d$cardio, d$BMI,mean)
 
 
ggplot(data = d, aes(x = as.character(cardio), y = gender)) +
  geom_jitter(color = "green", size = 0.5, width = 0.15) +
  labs(x = "cardio", y = "gender") +
  theme_classic()+
  ylim(0,2)

######################

library(prob)
library(distr)
library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
data <- read_csv("Documents/data5.csv")
View(data)
glimpse(data)
head(data)
library(ggthemes)
library(forcats)
data %>%
  select(one_of('dataset','Data_1')) %>%
  arrange(desc(Data_1)) %>%
  head(17) %>%
  mutate(state_ut = fct_reorder(dataset,Data_1)) %>%
  ggplot() + geom_col(aes(y = dataset,x =Data_1), fill = 'red') +
  geom_label(aes(y = dataset,x =Data_1, label = Data_1), fill = 'yellow')+
  labs(title = 'Condensed Data',
       subtitle = '',
       caption = '') +
  #theme_hc(style = 'darkunica') +
  theme_economist() +
  theme(axis.text.x = element_text(color = 'red'),
        axis.text.y = element_text(color = 'red'))+
  xlim(0,90)

