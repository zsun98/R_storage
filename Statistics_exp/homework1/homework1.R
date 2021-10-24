aa<-'E:/Rdata/Bigdata/homework1'
dir.create(aa)
setwd(aa)
path<-'E:/data/homework/module3/'
# import data
g01<-read.csv(paste(path, '3.3.csv', sep = ''), header = TRUE, stringsAsFactors = FALSE)
head(g01)
# table
table(g01)
# frequency chart
g011<-table(g01$顾客性别)
Gender<-c('male', 'female')
fchart<-data.frame(Gender, g011)[,c(1, 3)]
fchart$Percent<-fchart$Freq/sum(fchart$Freq)*100
fchart$CumPercent<-cumsum(fchart$Percent)
# bar chart
barplot(table(g01$饮料类型), col = rainbow(5), xlab = 'Types of Drinks', ylab = 'Freq',ylim = c(0, 15), main = 'Barplot of Types of Drinks', beside = TRUE, legend = colnames(table(g01)))
barplot(table(g01$顾客性别), col = c('red', 'blue', 'green', 'yellow', 'grey'), xlab = 'Gender', ylab = 'Freq',  ylim = c(0, 30), main = 'Barplot of Gender', beside = TRUE, legend = c('Male', 'Female'))
barplot(table(g01), beside = TRUE, main = 'Barplot of Gender and Types of Drinks', legend.text = rownames(g011), args.legend = list(x = "topleft", bty="n", cex = 0.7, ncol = 2))

