setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("2.9data.txt", header = TRUE)
# (1)(2)
lm.1 <- lm(y~x, data = X)
summary(lm.1)

# (3)
abs(qt(0.025, 8))

# (4)
yt <- predict(lm.1, data.frame(x=45), interval="prediction", level=0.95)
