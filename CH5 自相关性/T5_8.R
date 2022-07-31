setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("5.8data.txt", header = TRUE)
# (1)
y <- (X$consumer) / (X$price)
x <- (X$income) / (X$price)
lm.1 <- lm(y~x)
summary(lm.1)

# (2)
library(lmtest)
dwtest(lm.1)
bgtest(lm.1, order=1)

# (3)
rho <- 1 - 0.59857/2

# (4)
diy <- y[2:length(y)] - rho*y[1:length(y)-1]
dix <- x[2:length(x)] - rho*x[1:length(x)-1]
lm.2 <- lm(diy~dix)
summary(lm.2)
dwtest(lm.2)
b0 <- 45.16602/(1-rho)

# (5)
lm.3 <- lm(log(y)~log(x))
summary(lm.3)
dwtest(lm.3)
newy <- log(y[2:length(y)]) - rho*log(y[1:length(y)-1])
newx <- log(x[2:length(x)]) - rho*log(x[1:length(x)-1])
lm.4 <- lm(newy~newx)
summary(lm.4)
dwtest(lm.4)
b0 <- 0.25907/(1-rho)