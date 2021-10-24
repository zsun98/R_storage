dir.create('E:/Rdata/Exp8')
setwd('E:/Rdata/Exp8')
path<-'E:/data/homework/module11/'
data<-read.csv(paste(path, '11.6.csv', sep = ''), header = TRUE)
# correlational relationship
cor(data[,-1], y = NULL, method = 'pearson')
cor.test(data$不良贷款..亿元.,data$各项贷款余额..亿元., method = 'pearson')
# simple linear regression
y<-data$不良贷款..亿元.
x<-data$各项贷款余额..亿元.
g_lm<-lm(y ~ x, data = data)
# the regression result
summary(g_lm)
# analysis of variance
anova(g_lm)
# calculated fitted value and residuals
fittedvalue<-fitted(g_lm)
residual<-residuals(g_lm)
sum<-cbind(y, fittedvalue, residual)
# interval estimation
c_inter<-predict(g_lm, interval = 'c', level = 0.95) # confidence interval estimate
p_inter<-predict(g_lm, interval = 'p', level = 0.95) # prediction interval estimate
ppp<-cbind(c_inter, p_inter)[, -4]
# prediction line
par(pin = c(4, 3))
plot(data$各项贷款余额..亿元., data$不良贷款..亿元., type = 'p', pch = 16,  col = 'red', ylab = '不良贷款（亿元）', xlab = '贷款余额（亿元）', main = '不良贷款与贷款余额')
lines(data$各项贷款余额..亿元., fittedvalue, type = 'l', lty = 1, col = 'red', lwd = 1)
lines(data$各项贷款余额..亿元., c_inter[, 2], type = 'l', lty = 6, col = 'blue', lwd = 1 )
lines(data$各项贷款余额..亿元., c_inter[, 3], type = 'l', lty = 6, col = 'blue', lwd = 1 )
lines(data$各项贷款余额..亿元., p_inter[, 2], type = 'l', lty = 6, col = 'purple', lwd = 1)
lines(data$各项贷款余额..亿元., p_inter[, 3], type = 'l', lty = 6, col = 'purple', lwd = 1)
# prediction
g01<-data.frame(x = 4 )
pp<-predict(g_lm, newdata =  g01, interval = 'p', level = 0.95)
pc<-predict(g_lm, newdata =  g01, interval = 'c', level = 0.95)
# residual analysis
plot(x, residual)
# normal test about residual
shapiro.test(residual)# if p < 0.05, the residual obeys normal distribution

