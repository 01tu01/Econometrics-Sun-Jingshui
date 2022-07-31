setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("3.14data.txt", header = TRUE)
# (1)(2)
lm.1 <- lm(y~x1+x2, data = log(X))
summary(lm.1)

# (4)
lm.2 <- lm(y~x1+x2, data = X)
summary(lm.2)