aa<-'E:/Rdata/Basiceco/homework1'
dir.create(aa)
setwd(aa)
g01<-read.csv('E:/data/homework/7.16/7.16.csv', header = TRUE, stringsAsFactors = FALSE)[1:16, ]
# estimated para of multiple linear regression
g_lm<-lm(Y ~ X2 + X3 + X4 + X5, data = g01)
summary(g_lm)
anova(g_lm)
# estimated para of logarithm model
glm1<-lm(log(Y) ~ log(X2) + log(X3) + log(X4) + log(X5), data = g01)
summary(glm1)
anova(glm1)
