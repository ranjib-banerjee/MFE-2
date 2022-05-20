# Null Hypothesis: mu1 <= mu2
# Alternative Hypothesis: mu1 > mu2

library(webr)
library(BSDA)
data <- read.csv("C:/Users/naman/Downloads/covid_data_cleaned1.csv")
data$new_cases[is.na(data$new_cases)] = 0

s1 <- data$new_cases
mu1 <- mean(s1, na.rm = TRUE)
std1 <- sd(s1)
s2 <- data$new_vaccinations
mu2 <- mean(s2, na.rm = TRUE)
std2 <- sd(s2)

x = z.test(s1,s2,sigma.x = std1,sigma.y =  std2, conf.level = 0.95, alternative = "greater")
print(x) # Z test results