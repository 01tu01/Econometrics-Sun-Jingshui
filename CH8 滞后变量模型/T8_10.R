setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("8.9data.txt", header = TRUE)
# (1)
lm.1 <- lm(log(CS)~log(I), data = X)
summary(lm.1)
lm.2 <- lm(log(CS[2:length(CS)])~log(I[2:length(I)])+log(CS[1:(length(CS)-1)]), data = X)
summary(lm.2)

# (2)
E1 <- 1.10254
E2.short <- 1.13171
E2.long <- 1.13171/(1-(-0.01528))
data.frame(E1, E2.short, E2.long)