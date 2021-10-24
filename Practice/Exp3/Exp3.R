setwd('E:/Rdata')
path<-'E:/data/homework/module5'
# 1.normal distribution
x1<-1.96
#calculate probability
p<-pnorm(x1)
alpha<-1-p # significance
#calculate critical value
q<-qnorm(p, mean = 0, sd = 1, lower.tail = TRUE)#left area
q<-qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)#right area
q1<-qnorm(alpha/2, mean = 0, sd = 1, lower.tail = TRUE)#lower value
q2<-qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)# upper value
# create random numbers obeying normal distribution
rnorm(10, mean = 2, sd = 1)
# plot normal distribution
par(mfrow = c(1, 2))
# plot density curve
plot(dnorm, -3, 3)
#plot probability curve
plot(pnorm, -3, 3)
# histogram & density curve
par(mfrow = c(1, 1))
x2<-rnorm(100, 0, 1)
hist(x2, probability = TRUE, col = gray(.9), main = 'normal mu= 0 sigma=1')
lines(density(x2), lty = 1)# add a density curve
#binomial distribution
x3<-0:10000
plot(x3, dbinom(x3, size = 50, prob = .33), type = 'h', xlim = c(0, 50))#density curve of binomial distribution
#binomial distribution & normal distribution
b<-rnorm(10000, mean = 50*0.33, sd = (50*0.33*0.67)^0.5)
lines(density(b), lty = 1)
#calculate probability p(-0.6 <= x =< -0.4) x ~ n(0.5, 3)
p1<-pnorm(-0.4, mean = 0.5, sd = 3, lower.tail = TRUE)
p2<-pnorm(-0.6, mean = 0.5, sd = 3, lower.tail = TRUE)
print(paste('Probability =', round(p1-p2, digits = 2)))

# 2.t distribution
#calculate probability
p1<-pt(1.96, df = 3, lower.tail = TRUE)
#calculate significance
alpha1<-1-p1
alpha1<-pt(1.96, df = 3, lower.tail = FALSE)
#calculate critical value
q1<-qt(0.9275739, df = 3, lower.tail = TRUE)
# generate 30 numbers obey t(3)
x4<-rt(30, df = 3)
#calculate probability x ~ t(3) p(0.4 < x <0.6)
p3<-pt(0.6, df = 3, lower.tail = TRUE)
p4<-pt(0.4, df = 3, lower.tail = TRUE)
round(p3-p4, digits = 3)
#density curve & probability curve
x5<-seq(-3, 3, by = 0.2)
plot(x5, dt(x5, df = 3), type = 'l')
plot(x5, pt(x5, df = 3), type = 'l')

# F distribution
# calculate probability
p5<-pf(1.96, df1 = 3, df2 =5, lower.tail = TRUE)
# 1-p5
p6<-pf(1.96, df1 = 3, df2 =5, lower.tail = FALSE)
# calculate critical value
q1<-qf(0.05, df1 = 3, df2 = 5, lower.tail = FALSE)
# generate 50 random numbers that obey F(3, 5)
x6<-rf(30, 3, 5)
# calculate p(1.4< x < 1.6) x ~ F(3, 5)
p7<-pf(1.6, df1 =3, df2 = 5, lower.tail = TRUE)
p8<-pf(1.4, df1 = 3, df2 = 5, lower.tail = TRUE)
print(p7-p8)
# density curve 
x7<-seq(0, 3, by = 0.1)
plot(x7, df(x7, df1 = 3, df2 = 5), lty = 1, type ='l' )
# probability curve
plot(x7, pf(x7, df1 = 3, df2 = 5), lty = 1, type ='l' )

# chi-square distribution
# calculate probability
p11<-pchisq(1.96, df = 3, lower.tail = TRUE)
# significance 1-p11
p12<-pchisq(1.96, df = 3, lower.tail = FALSE)
# calculate critical value
q11<-qchisq(0.05, df = 3, lower.tail = FALSE)
# generate 30 random numbers
x11<-rchisq(30, df = 3)
# calculate p(0.4< x < 0.6) x ~ chi-square(3)
p11<-pchisq(0.6, df = 3, lower.tail = TRUE)
p12<-pchisq(0.4, df = 3, lower.tail = TRUE)
print(p11-p12)
# density curve
x11<-seq(0, 5, by = 0.1)
plot(x11, dchisq(x11, df = 3), type = 'o')
# probability curve
plot(x11, pchisq(x11, df = 3), type = 'o')


