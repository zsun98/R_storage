# data filter
# data filter
#抽取统计学成绩为75分的同学姓名
a<-scores$姓名[scores$统计学成绩 == 75]
a<-scores[scores$统计学成绩 == 75, 1]
print(a)
#二 数据排序
#名单按英语成绩从高到低排序
b<-scores[order(scores$英语成绩, decreasing = TRUE),]
print(b)
#提取数学成绩前三名的学生姓名
c<-scores[order(scores$数学成绩, decreasing = TRUE),1]
print(head(c, 3))
print(c[1:3])
#三 多个条件
#四门课都超过70分的学生姓名
d<-subset(scores, scores$统计学成绩>70 & scores$数学成绩>70 & scores$英语成绩>70 & scores$经济学成绩>70, select = c("姓名"))
print(d)
DATA_PATH = 'E:/data/homework'
# 2、创建频数表
setwd("E:/Rdata")
shdata<-read.csv(paste(path, '3.3.csv', sep=''), header = TRUE, stringsAsFactors = FALSE )
colnames(shdata)<-c('性别', '类型')
table(shdata$性别, shdata$类型)

# 3、累计频数/频数分布图
satsdata<-read.csv('E:/data/homework/module3/3.5.csv', header = TRUE, stringsAsFactors = FALSE)
# 绘制累计频数分布图
satsdata<-head(satsdata, 5)
satsdata$回答<-1:5
p1<-satsdata$甲城市户数/sum(satsdata$甲城市户数)*100
par(mfrow = c(1, 1))
plot(satsdata$回答, p1, las=2, type = 'o', pch = 19, cex = 1.5, lwd = 1.5, xlim = c(1, 5), ylim = c(1, 110), col = 5, xlab = '回答选项', ylab = '占比%')
plot(satsdata$回答, p1, las=1, type = 'o', pch = 19, cex = 1.5, lwd = 1.5, xlim = c(1, 5), ylim = c(1, 110), col = 5, xlab = '回答选项', ylab = '占比%')
# 甲向上累计
satsdata$cum<-cumsum(satsdata$甲城市户数)
# 甲向下累计
satsdata$累计向下<-rev(cumsum(rev(satsdata$甲城市户数)))
# 绘制向上累计频数分布图
plot(satsdata$回答, satsdata$cum, las = 1, type = 'o', pch = 19, xlab = '回答选项', ylab ='甲向上累计')
# 绘制向下累计频数分布图
plot(satsdata$回答, satsdata$累计向下, las = 1, type = 'o', pch =19, xlab ='回答选项', ylab = '甲累计向下')

# 4. group data
# import data
path<-'E:/data/homework/module3/'
compdata<-read.csv(paste(path, '3.6.csv', sep = ''))
# cut the data
range(compdata$销售量)
compdata$销售量<-cut(compdata$销售量, seq(140, 240, by = 10), right = FALSE)
# frequency statistics
table(compdata$销售量)
g<-cbind(frequency = table(compdata$销售量))
p<-prop.table(g, 2)# 2 means column, 1 means row
per<-round(p*100, digits = 2)# percentage
colnames(per) = 'percentage(%)'

# 4.boxplot
scoresdata<-read.csv(paste(path, '3.7.csv', sep = ''))
head(scoresdata)
# student boxplot
da<-head(scoresdata[-1 ,c(-1, -2)], 8)
colnames(da)<-c('st1' ,'st2', 'st3', 'st4', 'st5', 'st6', 'st7', 'st8', 'st9', 'st10', 'st11')
boxplot(da, las = 2, col = heat.colors(11, alpha = 0.5))
# courses boxoplot
row.names(da)<-c('en', 'ecomath', 'westeco', 'market', 'money', 'kuaiji', 'stats', 'cs')
da1<-t(da[,-12])
boxplot(da1, las = 1, col = topo.colors(10, alpha = 0.6))

# 5.line chart
expenditure<-read.csv(paste(path, '3.8.csv', sep = ''), header = TRUE, stringsAsFactors = FALSE)
x<-expenditure$年份
y<-expenditure$农村城镇消费水平
z<-expenditure$城镇居民消费水平
range(x)
range(y)
range(z)
plot(x, y, las = 1, type = 'o', pch = 19, lwd = 1.5, col = 2, ylim = c(3000, 30000), xlab = 'year', ylab = 'aver_expend(yuan)')
lines(x, z, las = 1, type = 'o', col = 1, pch = 19, lty = 2)

# 6.bubble chart
prod<-read.csv(paste(path, '3.9.csv', sep = ''), header = TRUE, stringsAsFactors = FALSE)
prod1<-head(prod, 7)[, 1:3]
x<-prod1$温度
y<-prod1$降雨量
z<-prod1$产量/1000
range(x)
range(y)
plot(x, y, cex = z, las = 1, pch = 19, col = 2, xlim = c(7, 25), ylim = c(35, 140), xlab = 'temperature(℃)', ylab ='rain(mm)')

# Bar chart
shdata<-read.csv(paste(path, '3.3.csv', sep=''), header = TRUE, stringsAsFactors = FALSE )
colnames(shdata)<-c('性别', '类型')
ss<-table(shdata$性别, shdata$类型)
barplot(ss, beside=TRUE, ylim = c(0, 10), legend = row.names(ss))
title(main = "饮料类型及人数统计", font.main = 4) # 加个标题

# pie chart
par(mfrow = c(2, 2))
ss1<-as.data.frame.array(t(ss))
p<-round(ss1$男/sum(ss1$男)*100, 2)
p1<-paste(p, '%', sep = '')
pie(ss[1, ], main = 'Different Types of Drink among Males', labels = paste(colnames(ss), p1))

# radar chart
install.packages('fmsb')
library(fmsb)
codata<-read.csv(paste(path, 'ex3.5.csv', sep = ''), header = TRUE, stringsAsFactors = TRUE)
codata1<-codata[-1, -1]
row.names(codata1)<-c('A', 'B', 'C', 'D', 'E')
colnames(codata1)<-c('x', 'y')
codata2<-as.data.frame.array(t(codata1))
codata21<-rbind(rep(20,5),rep(0,5), codata2)
t<-as.data.frame(lapply(codata21, as.numeric))
radarchart(t)
