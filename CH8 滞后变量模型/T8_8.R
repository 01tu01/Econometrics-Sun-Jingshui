setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("8.8data.txt", header = TRUE)
attach(X)
z0 <- x[4:length(x)] + x[3:(length(x)-1)] + x[2:(length(x)-2)] + x[1:(length(x)-3)]
z1 <- x[3:(length(x)-1)] + 2*x[2:(length(x)-2)] + 3*x[1:(length(x)-3)]
z2 <- x[3:(length(x)-1)] + 4*x[2:(length(x)-2)] + 9*x[1:(length(x)-3)]
lm.1 <- lm(y[4:length(x)]~z0+z1+z2)
summary(lm.1)
a <- -6.4196
b0 <- 0.6303
b1 <- 0.6303 + 0.9874 - 0.4608
b2 <- 0.6303 + 2*0.9874 - 4*0.4608
b2 <- 0.6303 + 3*0.9874 - 9*0.4608
data.frame(a, b0, b1, b2, b3)
