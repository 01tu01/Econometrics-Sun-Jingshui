setwd("D:\\undergraduate\\研\\432统计学\\计量经济学 孙敬水 PPT\\习题R代码")
X <- read.table("3.9data.txt", header = TRUE)
lm.1 <- lm(yt~x1t+x2t, data = X)
summary(lm.1)
