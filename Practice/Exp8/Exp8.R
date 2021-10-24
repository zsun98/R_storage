install.packages("car")
library(car)
dir.create('E:/Rdata/Exp8')
setwd('E:/Rdata/Exp8')
data<-read.csv('E:/data/homework/module12/12.1.csv', header = TRUE, stringsAsFactors = FALSE)
colnames(data)<-c('code', 'bad debt', 'remaining debt', 'returning debt', 'debt numbers', 'investigation this year')
data1<-data[, -1]
# scatter matrix
pairs(data1)
# multiple linear regression
y<-data1$`bad debt`
x1<-data1$`remaining debt`
x2<-data1$`returning debt`
x3<-data1$`debt numbers`
x4<-data1$`investigation this year`
g_lm<-lm(y ~ x1 + x2 +x3 +x4)
summary(g_lm)
# determine multicollinearity
# calculate correlational relationship
round(cor(data1), 2)
x1<-data1[, 2]
x2<-data1[, 3]
x3<-data1[, 4]
x4<-data1[, 5]
cor.test(x1, x2)
cor.test(x1, x3)
cor.test(x1, x4)
cor.test(x2, x3)
cor.test(x2, x4)
cor.test(x3, x4)
cor.test(x, y)
# variance inflation factor
vif(g_lm)
# handle the muticollinearity
# delete the related variable
g1_lm<-lm(y ~ x1 + x4)
summary(g1_lm)
vif(g1_lm)
# prediction
# point estimate
fitted(g_lm)
# interval estimate
r1<-predict(g_lm, interval = 'c', level = 0.95)#confidence interval estimate
r2<-predict(g_lm, interval = 'p', level = 0.95)#prediction interval estimate
r<-data.frame(r1, r2[, -1])
# prediction according new data
predict(g_lm, newdata = data.frame(x1 = 104.2, x2 = 11, x3 = 12, x4 = 88.2), interval = 'c', level = 0.95)
predict(g_lm, newdata = data.frame(x1 = 104.2, x2 = 11, x3 = 12, x4 = 88.2), interval = 'p', level = 0.95)
# residual analysis
residual<-residuals(g_lm)
plot(residual)
# stepwise regression
slm<-step(g_lm)
summary(slm)
