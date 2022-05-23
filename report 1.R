# Null Hypothesis: mu1 >= mu2
# Alternative Hypothesis: mu1 < mu2

data <- read_csv("data2.csv")

s1 <- data$bad
mu1 <- mean(s1, na.rm = TRUE)
s2 <- data$timeonline
mu2 <- mean(s2, na.rm = TRUE)

barplot(s1, names.arg = s2, xlab ="bad", 
        ylab ="timeonline", col ="blue", 
        main ="online time vs E-learning preffered students ")
m<-hist(s2, xlab = "timeonline", ylab ="Frequency",
        col = "darkmagenta", border = "pink",
        breaks = 5)

