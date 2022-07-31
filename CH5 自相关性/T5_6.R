setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("5.6data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~I(1/x), data = X)
summary(lm.1)

# (2)
library(lmtest)
dwtest(lm.1)

# (3)
bgtest(lm.1, order=1)

# (4)
rho <- 1 - 0.6393684/2

# (5)
attach(X)
newy <- y[2:length(y)] - rho*y[1:length(y)-1]
newx <- 1/x[2:length(y)] - rho*(1/x[1:length(y)-1])
lm.2 <- lm(newy~newx)
summary(lm.2)
bgtest(lm.2, order=1)
b0 <- -0.3589884/(1-rho)
