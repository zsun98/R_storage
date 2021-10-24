install.packages('carData')
library(car)

aa<-'E:/Rdata/Basiceco/homework1'
dir.create(aa)
setwd(aa)
# Exercise 7.16
# import data
g01<-read.csv('E:/data/homework/eco/7.16.csv', header = TRUE, stringsAsFactors = FALSE)[1:16, ]
# estimated para of multiple linear regression
g_lm<-lm(Y ~ X2 + X3 + X4 + X5, data = g01)
summary(g_lm)
anova(g_lm)
# residual analysis
plot(residuals(g_lm), main = 'Residuals of Model g_lm')
# estimated para of logarithm model
glm1<-lm(log(Y) ~ log(X2) + log(X3) + log(X4) + log(X5), data = g01)
summary(glm1)
anova(glm1)
# residual analysis
plot(residuals(glm1), main = 'Residuals of Model g_lm')

# Exercise 7.19
# import data
g02<-read.csv('E:/data/homework/eco/7.19.csv', header = TRUE, stringsAsFactors = FALSE)[1:23, ]
str(g02)
# correlation between X4 and X5
cor(g02$X4, g02$X5, method = 'pearson')
cor.test(g02$X4, g02$X5)
# scatter chart
plot(g02$X4, g02$X5, xlab = 'Price of Pork', ylab = 'Price of Beef', main = 'Correlation between Price of Pork and Price of Beef')
abline(lm(X5 ~ X4, data = g02))
# estimate model 4
g_lm2<-lm(log(Y) ~ log(X2) + log(X3) + log(X4) + log(X5), data = g02)
summary(g_lm2)
vif(g_lm2)
# estimate model 5
g_lm3<-lm(log(Y) ~ log(X2) + log(X3) + log(X6), data = g02)
summary(g_lm3)
vif(g_lm3)
anova(g_lm3)
# correlation between variables
pairs(g02[, c(3, 4, 7)])
cor(log(g02$X2), log(g02$X6), method = 'pearson')

# Example 7.21
# import data
g03<-read.csv('E:/data/homework/eco/7.21.csv', header = TRUE, stringsAsFactors = FALSE)
str(g03)
colnames(g03)<-c('year', 'Y', 'M', 'CPI', 'Lr', 'Tr')
# estimate model 1
g_lm41<-lm(log(M) ~ log(Y) + log(Lr), data = g03)
summary(g_lm41)
# estimate model 2
g_lm42<-lm(log(M) ~ log(Y) + log(Tr), data = g03)
summary(g_lm42)
# estimate model 3
g03$F<-g03$M/g03$Y
g_ml51<-lm(log(F) ~ log(Lr), data = g03)
summary(g_ml51)
# estimate model 4
g_ml52<-lm(log(F) ~ log(Tr), data = g03)
summary(g_ml52)

