# Null Hypothesis: mu1 >= mu2
# Alternative Hypothesis: mu1 < mu2

data <- read_csv("data2.csv")

s1 <- data$good
mu1 <- mean(s1, na.rm = TRUE)
s2 <- data$bad
mu2 <- mean(s2, na.rm = TRUE)

t.test(s1, s2, alternative="less", conf.level = 0.95)

plot(x = data$good, y = data$bad, xlab = "GOOD",
     ylab = "BAD", main = "GOOD vs BAD",
     col.lab = "darkgreen", col.main = "darkgreen",
     col.axis = "darkgreen",colours())

