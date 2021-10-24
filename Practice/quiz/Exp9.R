dir.create("E:/Rdata/Exp9")
sink('E:/Rdata/output.txt')
setwd("E:/Rdata/Exp9")
data<-read.csv('E:/data/homework/2021finalexam.csv', header = TRUE, stringsAsFactors = FALSE)
head(data, 10)
# 选取北京地区从事互联网、服饰、酒店餐饮这三个行业的企业总资产及净益率
data1<-subset(data, data$地区 == '北京',  select = c(1, 2, 4, 16 ))
data2<-subset(data1, data1$细分行业 == '互联网' | data1$细分行业 == '服饰'| data1$细分行业 == '酒店餐饮', select = c(1, 2, 3, 4))
colnames(data2)<-c('名称', '行业', '总资产', '净益率')
# 统计北京市三个行业频数
a<-table(data2$行业)
# 绘制三个行业在北京市的频数分布图
barplot(a, col = rainbow(3), xlab = '行业细分', ylab = '企业频数', main = '行业频数分布图')
# 绘制互联网行业总资产的直方图
data3<-data2[data2$行业 == '互联网', ]
range(data3$总资产)
length(data3$总资产)
data3$总资产<-cut(data3$总资产, seq(10, 180, by = 10), right = FALSE)
table(data3$总资产)
plot(data3$总资产, col = rainbow(17), xlab = '总资产（亿元）', ylab = '企业频数', main = '北京市互联网企业频数分布直方图')
# 计算偏态系数
library(fBasics)
skewness(as.numeric(data3$总资产))
# 比较北京市互联网、酒店餐饮、服饰企业净益率是否有显著差异
y<-data2$净益率
x<-data2$行业
data2.aov<-aov(y ~ x, data = data2[, c(2, 4)])
summary(data2.aov)
# 选取北京市从事互联网、服饰、酒店餐饮这三个行业的企业总资产、营业收入、净利润、净益率做相关性分析
data4<-subset(data, data$地区 == '北京',  select = c(1, 2, 4, 9, 11, 16 ))
data5<-subset(data4, data4$细分行业 == '互联网' | data4$细分行业 == '服饰'| data4$细分行业 == '酒店餐饮')
data5$净益率.<-as.numeric(data5$净益率.)
cor(x = data5[, 3:6], y = NULL, method = 'pearson')
cor.test(data5$净利润.亿., data5$净益率., method = 'pearson')
# 利用简单线性回归对净利润和净益率做回归分析
y1<-data5$净益率.
x1<-data5$净利润.亿.
g_lm<-lm(y1 ~ x1, data = data5[, c(5, 6)])
summary(g_lm)
aov(g_lm)# 对回归系数进行方差分析
sink()
