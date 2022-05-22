alpha<-0.02
n<-6235,
xbar<-9.56
sigma<-3.11
zaby<-qnorm(1-alpha/2)
LCL<-xbar-(zaby2)*sigma/sqrt(n)
UCL<-xbar+(zaby2)*sigma/sqrt(n)
print(LCL)
print(UCL)
MET<-(zaby2)*sigma/sqrt
 
#######################

df <- data.frame(value = c(119007
                           ,41245-105096
                           ,556807-37719
                           ,36087-9036
                           30727
),
                 group = paste0("G", 1:5))
library(ggplot2)
library(dplyr)
 
# Hole size
hsize <- 3
 
df <- df %>%
  mutate(x = hsize)
 
ggplot(df, aes(x = hsize, y = value, fill = group)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  scale_fill_discrete(labels = c("CRUELTY_BY_HUSBAND_OR_HIS_RELATIVES(ANDHRA PRADESH)"
                                 ,"TOTAL_MURDER , RIOTS (BIHAR)"
                                 ,"THEFT , ROBBERY  (MAHARASHTRA)"
                                 ,"RAPE , DOWRY_DEATHS (MADHYA PRADESH)"
                                 ,"KIDNAPPING & ABDUCTION (RAJASTHAN)"
  ))
 
#######################
 
library(prob)
library(distr)
library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
data <- read_csv("Documents/data9.csv")
View(data)
glimpse(data)
head(data)
library(ggthemes)
library(forcats)
data %>%
  select(one_of('state_ut','Installed_Capacity')) %>%
  arrange(desc(Installed_Capacity)) %>%
  head(36) %>%
  mutate(state_ut = fct_reorder(state_ut,Installed_Capacity)) %>%
  ggplot() + geom_col(aes(y = state_ut,x =Installed_Capacity), fill = 'pink') +
  geom_label(aes(y = state_ut,x =Installed_Capacity, label = Installed_Capacity), fill = 'white')+
  labs(title = 'INSTALLED CAPACITY IN DIFFERENT STATES/UT',
       subtitle = 'TOTAL IS 31,841',
       caption = '') +
  theme_hc(style = 'darkunica') +
  theme_economist() +
  theme(axis.text.x = element_text(color = 'blue'),
        axis.text.y = element_text(color = 'blue'))+
  xlim(0,7000)
 
#######################
 
library(prob)
library(distr)
library(distrEx)
library(ggplot2)
library(readr)
library(dplyr)
library(sfsmisc)
library(sf)
library(magrittr)
data <- read_csv("Documents/data9.csv")
View(data)
glimpse(data)
head(data)
library(ggthemes)
library(forcats)
data %>%
  select(one_of('state_ut','Proposed_Capacity')) %>%
  arrange(desc(Proposed_Capacity)) %>%
  head(36) %>%
  mutate(state_ut = fct_reorder(state_ut,Proposed_Capacity)) %>%
  ggplot() + geom_col(aes(y = state_ut,x =Proposed_Capacity), fill = 'orange') +
  geom_label(aes(y = state_ut,x =Proposed_Capacity, label = Proposed_Capacity), fill = 'lightblue')+
  labs(title = 'PROPOSED CAPACITY IN DIFFERENT STATES/UT',
       subtitle = 'TOTAL IS 4,827',
       caption = '') +
  theme_hc(style = 'darkunica') +
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black'))+
  xlim(0,3000)

#######################

df <- data.frame(
      value = c( 0, 853, 0, 0, 631, 293, 73, 24, 104, 3378, 1880, 155, 222, 639, 2712, 120, 0, 1924, 9819, 0, 0, 10, 0, 2896, 378, 59,  1781, 1195, 30, 1492, 901, 8, 3374, 515, 1202),group = paste0("G", 1:35))
library(ggplot2)
library(dplyr)
 
# Hole size
hsize <- 3
 
df <- df %>%
      mutate(x = hsize)
 
ggplot(df, aes(x = hsize, y = value, fill = group)) +
      geom_col(color = "black") +
      geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)
      ) +
      coord_polar(theta = "y") +
      xlim(c(0.2, hsize + 0.5)) +
      scale_fill_discrete(labels = c("Andaman & Nicobar Islands", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",  "Dadra & Nagar Haveli",  "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Karnataka", "Kerala", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "NCT of Delhi", "Orissa", "Pondicherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh", "Uttarakhand", "West Bengal" ))
 
#######################

df <- data.frame(value = c(0 ,443 ,0 ,0 ,0 ,271 ,73 ,24 ,44 ,3358 ,1880 ,99 ,93 ,22 ,1922 ,114 ,0 ,684 ,6366 ,0 ,0 ,0 ,0 ,2715 ,55 ,56 ,1601 ,783 ,18 ,1492 ,842 ,8 ,3224 ,345 ,337 group = paste0("G", 1:35))
library(ggplot2)
library(dplyr)
 
# Hole size
hsize <- 3
 
df <- df %>%
  mutate(x = hsize)
 
ggplot(df, aes(x = hsize, y = value, fill = group)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
      scale_fill_discrete(labels = c("Andaman & Nicobar Islands", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",  "Dadra & Nagar Haveli",  "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Karnataka", "Kerala", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "NCT of Delhi", "Orissa", "Pondicherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh", "Uttarakhand", "West Bengal" ))
 



