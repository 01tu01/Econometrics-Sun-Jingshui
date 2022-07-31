setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("5.7data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~x, data = log(X))
summary(lm.1)

# (2)
library(lmtest)
dwtest(lm.1)
rho <- 1 - 0.45171/2
# Durbin第一步
attach(X)
yt <- log(y[2:length(y)])
yt1 <- log(y[1:length(y)-1])
xt <- log(x[2:length(x)])
xt1 <- log(x[1:length(x)-1])
lm.2 <- lm(yt~yt1+xt+xt1)
summary(lm.2)
dwtest(lm.2)
rhohat <- 0.63187
# Durbin第二步
dby <- yt - rhohat*yt1
dbx <- xt - rhohat*xt1
lm.3 <- lm(dby~dbx)
summary(lm.3)
bgtest(lm.3, order=1)
b0 <- 0.41536/(1-rhohat)
# 广义最小二乘法
gy <- log(y)
gx <- log(x)
gy[1] = gy[1] * sqrt(1-rho^2)
gx[1] = gx[1] * sqrt(1-rho^2)
newy <- gy[2:length(gy)] - rho*gy[1:length(gy)-1]
newx <- gx[2:length(gx)] - rho*gx[1:length(gx)-1]
lm.4 <- lm(newy~newx)
summary(lm.4)
b0 <- -0.20322/(1-rho)
b0 <- 0.27559/(1-rho)
bgtest(lm.4, order=1)

# (3)
diy <- y[2:length(y)]-y[1:length(y)-1]
dix <- x[2:length(x)]-y[1:length(x)-1]
lm.5 <- lm(diy~dix)
summary(lm.5)
dwtest(lm.5)
