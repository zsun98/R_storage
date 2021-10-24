dir.create('E:/Rdata/Exp5')
setwd('E:/Rdata/Exp5')
# hypothesis test
# 1. a test about mu
# a variable obeys normal distribution with a large sample size and a known variance
mu0<-0.081
n<-200
xbar<-0.076
sigma<-0.025
alpha<-0.05
z<-(xbar-mu0)/(sigma/sqrt(n))
p<-pnorm(z, mean = 0, sd = 1, lower.tail = TRUE)*2
p<-2*(1-pnorm(abs(z)))
if(p < alpha){
  print('rufuse null hypothesis')
}
# left test, a variable with a large sample size and an a known variance
mu0<-1000
xbar<-960
sigma<-200
alpha<-0.05
n<-100
z<-(xbar-mu0)/(sigma/sqrt(n))
p<-1-pnorm(abs(z))
if(p < alpha){
  print('refuse null hypothesis')
}
# right test, a variable obeys normal distribution with a small sample and a known variance
mu0<-1200
sigma<-150
n<-20
xbar<-1245
alpha<-0.05
z<-(xbar-mu0)/(sigma/sqrt(n))
p<-1-pnorm(abs(z))
if(p < alpha){
  print('refuse null hypothesis')
}else{
  print('can\'t refuse null hypothesis')
}
# two-sided test, a variable obeys normal distribution with a small sample and an unknown variance
mu0<-5
n<-10
xbar<-5.3
sigma<-0.3
alpha<-0.05
t<-(xbar-mu0)/(sigma/sqrt(n))
p<-2*(1-pt(t, df = n-1, lower.tail = TRUE))
if(p < alpha){
  print('refuse null hypothesis')
}else{
  print('can\'t refuse null hypothesis')
}

# 2. hypothesis test about proportion
pie<-0.147
n<-400
p<-57/400
alpha<-0.05
z<-(p-pie)/sqrt(pie*(1-pie)/n)
p<-2*(1-pnorm(abs(z), mean = 0, sd = 1, lower.tail = TRUE))
if(p < alpha){
  print('refuse null hypothesis')
}else{
  print('can\'t refuse null hypothesis')
}

# 3. test function in R (a large sample size and an unknown variance)
# prop.test() proportion hypothesis test
prop.test(57, 400, p = 0.147, alternative = 'two.sided', correct = FALSE)
# t.test() mean hypothesis test
