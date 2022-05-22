library(ggplot2)
library(csv)
library(lubridate)
library(dplyr)
library(webr)
library(BSDA)

data<-read.csv("C:\\Users\\aryam\\Desktop\\MFE 2\\Maths project.csv")
data
str(data)
colnames(data) <- c('date','pm2.5','pm10','o3','no2','so2','co','Year')

data <- data %>%
  mutate(month = month(date))
str(data)

data_new  = data %>% group_by(month, Year) %>%
  summarise(avg_pm2.5 = mean(pm2.5),
            avg_pm10 = mean(pm10),
            avg_o3 = mean(o3),
            avg_no2 = mean(no2),
            avg_so2 = mean(so2),
            avg_co = mean(co),
            .groups = 'drop')

ggplot(data_new, aes(x = month, y = avg_pm2.5)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "PM 2.5 levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = pm2.5, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "PM 2.5 levels",
       x = "Date") + theme_bw(base_size = 8)

ggplot(data_new, aes(x = month, y = avg_pm10)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "PM 10 levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = pm10, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "PM 10 levels",
       x = "Date") + theme_bw(base_size = 8)

ggplot(data_new, aes(x = month, y = avg_o3)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "O3 levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = o3, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "O3 levels",
       x = "Date") + theme_bw(base_size = 8)

ggplot(data_new, aes(x = month, y = avg_no2)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "NO2 levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = no2, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "NO2 levels",
       x = "Date") + theme_bw(base_size = 8)

ggplot(data_new, aes(x = month, y = avg_so2)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "SO2 levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = so2, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "SO2 levels",
       x = "Date") + theme_bw(base_size = 8)

ggplot(data_new, aes(x = month, y = avg_co)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ Year, ncol = 1)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "CO levels",
       x = "Date") + theme_bw(base_size = 9)

ggplot(data, aes(x = date, y = co, group = 1)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=0.5) +
  facet_wrap( ~ month, ncol = 3)+
  labs(title = "Air Quality Index - Punjabi Bagh, Delhi",
       subtitle = "Data plotted by year",
       y = "CO levels",
       x = "Date") + theme_bw(base_size = 8)


data<-read.csv("C:\\Users\\aryam\\Desktop\\MFE 2\\Maths project.csv")
data

#t test
winter = data[data$month %in% c(10,11,12,1,2),]
rest = data[data$month %in% c(3,4,5,6,7,8,9),]

t = t.test(winter$pm2.5, rest$pm2.5, alternative="greater", conf.level = 0.95)
print(t)
plot(t)

#z test
std1 = sd(winter$pm2.5)
std2 = sd(rest$pm2.5)
z = z.test(winter$pm2.5, rest$pm2.5, sigma.x = std1, sigma.y = std2, conf.level = 0.95, alternative="greater")
print(z)
plot(z)







