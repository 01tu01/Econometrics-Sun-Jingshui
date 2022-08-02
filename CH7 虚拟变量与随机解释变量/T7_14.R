setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("7.14data.txt", header = TRUE)
# (1)
lm.1 <- lm(y~D2+D3+D4+x, data = X)
summary(lm.1)

# (2)
lm.2 <- lm(y~I(D2*x)+I(D3*x)+I(D4*x)+x, data = X)
summary(lm.2)

# (3)
lm.3 <- lm(y~D2+D3+D4+I(D2*x)+I(D3*x)+I(D4*x)+x, data = X)
summary(lm.3)