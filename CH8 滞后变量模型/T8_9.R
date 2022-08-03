setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("8.9data.txt", header = TRUE)
# (1)
lm.1 <- lm(CS~I, data = X)
summary(lm.1)
lm.2 <- lm(CS[2:length(CS)]~I[2:length(I)]+CS[1:(length(CS)-1)], data = X)
summary(lm.2)

# (2)
MPC1 <- 1.00719
MPC2.short <- 0.97590
MPC2.long <- 0.97590/(1-0.04305)
data.frame(MPC1, MPC2.short, MPC2.long)