setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("3.13data.txt", header = TRUE)
lm.1 <- lm(log(y)~x, data = X)
summary(lm.1)

ln.y2002 <- predict(lm.1, data.frame(x=12))
y2002 <- exp(ln.y2002)