# Null Hypothesis: mu1 <= mu2
# Alternative Hypothesis: mu1 > mu2

library(webr)
data <- read.csv("C:/Users/naman/Downloads/covid_data_cleaned1.csv")
data$new_deaths <- as.integer(data$new_deaths != 0)
data$new_deaths[is.na(data$new_deaths)] = 0
m1 = sum(data$new_deaths) / nrow(data)

s1 <- subset(data$new_deaths,data$continent == "North America")
mu1 <- mean(s1, na.rm = TRUE)
s2 <- subset(data$new_deaths,data$continent == "South America")
mu2 <- mean(s2, na.rm = TRUE)

x = t.test(s1, s2, alternative="greater", conf.level = 0.95, var.equal = FALSE)
print(x)
plot(x)


