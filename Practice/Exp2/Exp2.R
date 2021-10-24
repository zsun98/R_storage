setwd('E:/Rdata')
install.packages('fBasics')
library(fBasics)
path<-'E:/data/homework/module3/'
path1<-'E:/data/homework/module4/'
# 1. mode
drink<-read.csv(paste(path, '3.3.csv', sep = ''), header = TRUE, stringsAsFactors = TRUE)
y<-table(drink[,2])
m0<-rownames(y)[y == max(y)]
# 2. median
me<-rownames(y)[y == median(y)]
sort(y)# give an increasing order
# 3.quartile
income<-c(1500, 750, 780, 1080, 850, 960, 2000, 1250, 1630)
sort(income)
q1<-quantile(income, probs = 0.25, type = 4)
q2<-quantile(income, probs = 0.75, type = 4)
# 4.mean
g<-read.csv(paste(path, '3.6.csv', sep = ''))
range(g)
x<-seq(145, 235, by = 10)
g1<-cut(g$销售量, seq(140, 240, by = 10), right = FALSE)
f<-table(g1)
f1<-cbind(f)
f2<-as.numeric(f1)
mean<-weighted.mean(x, f2)
mean
# adjusted mean
x1<-c(88, 98, 92, 95, 97, 89, 90, 94, 97, 98)
mean1<-mean(x, trim = 0.1)# delete a maximium and a minimium
print(mean1)
# 5. geometric mean
x2 <- c(1, 104.5, 102.1, 125.5, 101.9) 
n<-length(x)
p<-cumprod(x)[n]
gmean<-p^(1/(n-1))
speed<-paste(round((gmean-100), digits = 2), '%', sep = '')
# 6.standard deviation
x3<-rep(x, f2)
var(x3)
sd(x3)
# standard score
x4<-c(1500, 750, 780, 1080, 850, 960, 2000, 1250, 1630)
xscore<-round(scale(x4), digits = 2)
data.frame(x4, xscore)
mean(xscore)
sd(xscore)
cv<-sd(x4)/mean(x4)
# another example
shoot<-read.csv(paste(path1, '4.14.csv', sep = ''))
head(shoot)
colnames(shoot)<-c('names', 'nation', 'pre', '1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th')
a1<-apply(shoot[, -(1:3)], 1, mean)# 1 means row and 2 means column
a2<-apply(shoot[, -(1:3)], 1, sd)
cv<-function(x){sd(x)/mean(x)}
a3<-apply(shoot[, -(1:3)], 1, cv)
shootdata<-data.frame(names = shoot$names,mean = a1, sd= a2, cv = a3)
# 7. skewness & kurtosis
skewness(x3)
kurtosis(x3)
