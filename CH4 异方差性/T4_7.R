setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("4.7data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~x, data = X)
summary(lm.1)

# (2)
library(lmtest)
gqtest(lm.1, fraction = 0, alternative = "two.sided")

# (3)
lm.2 <- lm(y~x, data = X, weights = 1/(x^2))
summary(lm.2)
