setwd('E:/Rdata/eco-Gujar/M10')

install.packages('car')

library(car)

# download data
devtools::install_github('https://github.com/brunoruas2/gujarati', force = TRUE)

# 10.26
#import data
s01<-gujarati::Table10_12
s01$Y<-as.numeric(as.character(s01$Y))
s01$X2<-as.numeric(as.character(s01$X2))
s01$X3<-as.numeric(as.character(s01$X3))
s01$X4<-as.numeric(as.character(s01$X4))
s01$Z<-s01$X2+s01$X3*0.75+s01$X4*0.625
# set a model
s01_lm1<-lm(Y ~ Z, data = s01)
summary(s01_lm1)
beta1<-as.numeric(coef(s01_lm1)[1])
beta2<-as.numeric(coef(s01_lm1)[2])
beta3<-beta2*0.75
beta4<-beta2*0.625

#10.27
# import data
s02<-gujarati::Table10_13
s02$CPI<-as.numeric(as.character(s02$CPI))
s02$GDP<-as.numeric(as.character(s02$GDP))
s02$Imports<-as.numeric(as.character(s02$Imports))
# set a model
s02_lm1<-lm(log(Imports) ~ log(GDP) + log(CPI), data = s02)
summary(s02_lm1)
vif(s02_lm1)
# set model2
s02_lm2<-lm(log(Imports) ~ log(GDP), data = s02)
summary(s02_lm2)
# set model3
s02_lm3<-lm(log(Imports) ~ log(CPI), data = s02)
summary(s02_lm3)
# set model4
s02_lm4<-lm(log(GDP) ~ log(CPI), data = s02)
summary(s02_lm4)
# set model5
s02_lm5<-lm(log(Imports) ~ log(GDP/CPI), data = s02)
summary(s02_lm5)

# 10.30
s03<-gujarati::Table10_15
s03$HRS<-as.numeric(as.character(s03$HRS))
s03$RATE<-as.numeric(as.character(s03$RATE))
s03$ERSP<-as.numeric(as.character(s03$ERSP))
s03$ERNO<-as.numeric(as.character(s03$ERNO))
s03$NEIN<-as.numeric(as.character(s03$NEIN))
s03$ASSET<-as.numeric(as.character(s03$ASSET))
s03$AGE<-as.numeric(as.character(s03$AGE))
s03$DEP<-as.numeric(as.character(s03$DEP))
s03$SCHOOL<-as.numeric(as.character(s03$SCHOOL))
# set model
s03_lm1<-lm(HRS ~ RATE +ERSP +ERNO +NEIN +ASSET +AGE +DEP +SCHOOL)
# set model
s03_lm1<-lm(HRS ~ RATE +ERSP +ERNO +NEIN +ASSET +AGE +DEP +SCHOOL, data = s03)
summary(s03_lm1)
# vif & TOL
vif1<-vif(s03_lm1)
tol1<-1/vif1
# 10.33
# import data
s04<-gujarati::Table10_17
s04$Y<-as.numeric(as.character(s04$Y))
s04$X1<-as.numeric(as.character(s04$X1))
s04$X2<-as.numeric(as.character(s04$X2))
s04$X3<-as.numeric(as.character(s04$X3))
s04$X4<-as.numeric(as.character(s04$X4))
s04$X5<-as.numeric(as.character(s04$X5))
s04$X6<-as.numeric(as.character(s04$X6))
# scatter chart
pairs(s04[3:8], main = 'Scatter between 6 Variables')
# correlation matrix
round(cor(s04[3:8]), 2)
# set a model
s04_lm1<-lm(Y ~ X1 +X2 +X3 +X4 +X5 +X6, data = s04)
summary(s04_lm1)
