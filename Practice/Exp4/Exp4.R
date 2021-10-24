dir.create('E:/Rdata/Exp4')
setwd('E:/Rdata/Exp4')
path<-'E:/data/homework/module7/'
# 1. interval of mu
# normal distribution with an known variance
fweight<-read.csv(paste(path, '7.1.csv', sep = ''), header = TRUE)
n<-length(fweight$重量)
sigma<- 10
alpha<-0.05
q<-qnorm(1-alpha/2, 0, 1)
xbar<-mean(fweight$重量)
interval<-c(xbar - q*sigma/sqrt(n), xbar + q*sigma/sqrt(n))
# an unknown distribution with a large sample
age<-read.csv(paste(path, '7.2.csv', sep = ''), header = TRUE)
n<-length(age$年龄)
sigma<-sd(age$年龄)#sample standard deviation
alpha<-0.1
q<-qnorm(1-alpha/2, lower.tail = TRUE)
xbar<-mean(age$年龄)
interval<-c(xbar - q*sigma/sqrt(n), xbar + q*sigma/sqrt(n))
# a variable obeys normal distribution with a small sample and an  unknown variance
exp<-read.csv(paste(path, '7.3.csv', sep = ''), header = TRUE)
n<-length(exp$使用寿命)
sigma<-sd(exp$使用寿命)
xbar<-mean(exp$使用寿命)
alpha<-0.05
q<-qt(1-alpha/2, n-1, lower.tail = TRUE)
interval<-c(xbar - q*sigma/sqrt(n), xbar + q*sigma/sqrt(n))
# estimated proportion
n<-100
p<-0.65
alpha<-0.05
q<-qnorm(1-alpha/2, lower.tail = TRUE)
interval<-c(p-q*sqrt(p*(1-p)/n), p+q*sqrt(p*(1-p)/n))

# 3.calculate sample size n
# when estimating mu
sigma<-2000
e<-400
alpha<-0.05
q<-qnorm(1-alpha/2, lower.tail = TRUE)
n<-ceiling((q*sigma/e)^2)
# when estimating proportion
pie<-0.9
e<-0.05
alpha<-0.05
q<-qnorm(1-alpha/2, lower.tail = TRUE)
n<-ceiling(pie*(1-pie)*q^2/e^2)
