dir.create('E:/Rdata/eco-Gujar/M8')
setwd('E:/Rdata/eco-Gujar/M8')

install.packages('devtools')
install.packages("strucchange") 

library(strucchange)

# download data
devtools::install_github('https://github.com/brunoruas2/gujarati')
# 8.26
s01<-gujarati::Table8_10
s01$X2<-as.numeric(as.character(s01$X2))
s01$X3<-as.numeric(as.character(s01$X3))
s01$X4<-as.numeric(as.character(s01$X4))
s01$X5<-as.numeric(as.character(s01$X5))
s01$X6<-as.numeric(as.character(s01$X6))
s01$Y<-as.numeric(as.character(s01$Y))
# a. multiple linear regression
glm1<-lm(Y ~ X2 + X3 + X4 + X5 + X6, data = s01)
summary(glm1)
# analysis of variance
anova(glm1)
# 8.27
pf(2.083, 1, 25, lower.tail = FALSE)
# 8.31
pt(-2.21, 59)*2
pf(5.972,1,63, lower.tail = FALSE)
# 8.36
s02<-gujarati::Table8_11
str(s02)
s02$Savings<-as.numeric(as.character(s02$Savings))
s02$Income<-as.numeric(as.character(s02$Income))
s02$Year<-as.character(s02$Year)
# simple linear regression
glm2<-lm(Savings ~ Income, data = s02)
summary(glm2)
glm3<-lm(Savings ~ Income, data = tail(s02, 4))
summary(glm3)
# chow test(whole and part3)
sctest(Savings ~ Income, data = s02, point = 32, type = "Chow")
# chow test(whole and part1)
sctest(Savings ~ Income, data = s02, point = 12, type = "Chow")
# chow test(whole and part2)
sctest(Savings ~ Income, data = s02, from = 13,to = 32, type = "Chow")
