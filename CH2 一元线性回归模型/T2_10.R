setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("2.10data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~x, data = X)
summary(lm.1)
attach(X)
plot(y~x)
abline(lm(y~x))

# (3)
yt <- predict(lm.1, data.frame(x=15.0)); yt