setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("8.11data.txt", header = TRUE)
# (1)
lm.1 <- lm(y[2:length(x)]~x[2:length(x)]+y[1:(length(y)-1)], data = X)
summary(lm.1)
library(lmtest)
bgtest(lm.1, order=1)
delta <- 1-0.27168
a <- -15.10403/delta
b <- 0.62927/delta
data.frame(delta, a, b)

# (2)
lm.2 <- lm(y[2:length(x)]~x[2:length(x)]+y[1:(length(y)-1)], data = log(X))
summary(lm.2)
library(lmtest)
bgtest(lm.2, order=1)
delta <- 1-0.2600
a <- -1.0780/delta
b <- 0.9045/delta
data.frame(delta, a, b)

# (3)
library(ivreg)
lm.3 <- ivreg(y[2:length(x)]~x[2:length(x)]+y[1:(length(y)-1)]
						| x[1:(length(y)-1)]+x[2:length(x)], data = X)
summary(lm.3)
library(lmtest)
bgtest(y[2:length(x)]~x[2:length(x)]+y[1:(length(y)-1)]
						| x[1:(length(y)-1)]+x[2:length(x)], data = X, order=1)
gamma <- 1-0.2685
a <- -15.1833/gamma
b <- 0.6319/gamma
data.frame(gamma, a, b)

# (4)
attach(X)
z0 <- x[5:length(x)] + x[4:(length(x)-1)] + x[3:(length(x)-2)] + x[2:(length(x)-3)] + x[1:(length(x)-4)]
z1 <- x[4:(length(x)-1)] + 2*x[3:(length(x)-2)] + 3*x[2:(length(x)-3)] + 4*x[1:(length(x)-4)]
z2 <- x[4:(length(x)-1)] + 4*x[3:(length(x)-2)] + 9*x[2:(length(x)-3)] + 16*x[1:(length(x)-4)]
lm.4 <- lm(y[5:length(x)]~z0+z1+z2)
summary(lm.4)
a <- -35.49234
b0 <- 0.89101
b1 <- 0.89101 - 0.66990 + 0.10439
b2 <- 0.89101 - 2*0.66990 + 4*0.10439
b3 <- 0.89101 - 3*0.66990 + 9*0.10439
b4 <- 0.89101 - 4*0.66990 + 16*0.10439
data.frame(a, b0, b1, b2, b3, b4)
library(lmtest)
bgtest(lm.4, order=1)
