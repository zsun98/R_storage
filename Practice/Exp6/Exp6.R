dir.create('E:/Rdata/Exp6')
setwd('E:/Rdata/Exp6/')
path<-'E:/data/homework/module10/'
# import data
data<-read.csv(paste(path, '10.1.csv', sep = ''), header = TRUE, stringsAsFactors = FALSE)
# change the form of data
x1<-as.matrix(data)
x2<-as.vector(x1)
y<-rep(colnames(data), each = 7)
data1<-na.omit(data.frame(x2, y)) # omit the not available value
# one-way analysis of variance
data.aov<-aov(x2~y, data = data1)
summary(data.aov)
# multiple comparison procedures
TukeyHSD(data.aov)
plot(TukeyHSD(data.aov))
# two-way analysis of variance
data2<-read.csv(paste(path, '10.3.csv', sep = ''), header = TRUE)
data3<-data2[2:5, 3:7]
colnames(data3)<-data2[1, 3:7]
row.names(data3)<-data2[2:5, 2]
data3<-as.data.frame(lapply(data3, as.numeric))
data4<-as.matrix(data3)
y1<-as.vector(data4)
region<-rep(c('R1', 'R2', 'R3', 'R4', 'R5'), each = 4)
brand<-rep(c('B1', 'B2', 'B3', 'B4'), times = 5)
data5<-data.frame(region, brand, y1)
data.aov2<-aov(y1~brand*region, data = data5)
summary(data.aov2)
interaction.plot(brand, region, y1, type = 'b', col = c('red', 'blue'), pch = c(16, 18), main = 'Interaction between region and brand')
