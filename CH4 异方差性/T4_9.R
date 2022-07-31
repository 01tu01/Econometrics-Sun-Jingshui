setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("4.9data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~x, data = X)
summary(lm.1)

# (2)
# White检验
lm.2 <- lm(e^2 ~ x + I(x^2), data = X)
R2 <- summary(lm.2)$r.sq
n <- nrow(lm.2$model)
m <- ncol(lm.2$model)
W <- n*R2
p = 1 - pchisq(W, m-1)
data.frame(W, p)
# Park检验
lm.3 <- lm(log(residuals(lm.1)^2) ~ log(x), data = X)
summary(lm.3)
# Glejser检验
lm.4 <- lm(abs(residuals(lm.1)^2) ~ x, data = X)
summary(lm.4)

# (3)
lm.5 <- lm(log(y)~log(x), data = X)
summary(lm.5)
lm.6 <- lm(residuals(lm.5)^2 ~ x + I(x^2), data = X)
R2 <- summary(lm.6)$r.sq
n <- nrow(lm.6$model)
m <- ncol(lm.6$model)
W <- n*R2
p = 1 - pchisq(W, m-1)
data.frame(W, p)