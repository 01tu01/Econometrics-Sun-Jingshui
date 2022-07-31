setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("3.16data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~x1+x2, data = X)
summary(lm.1)

# (2)
qf(0.05, 2, 7, lower.tail=FALSE)
qt(0.025, 7)
confint(lm.1)

# (3)
y <- predict(lm.1,data.frame(x1=35,x2=20000),interval="prediction",level=0.95)