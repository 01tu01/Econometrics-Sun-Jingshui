setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("7.13data.txt", header = TRUE)
# (1)
lm.1 <- lm(S~D2+D3+D4, data = X)
summary(lm.1)
