#R 4.0
co <- read.table('/Users/yanjiacheng/Desktop/毕业数据/液相做相关.txt', sep = '\t',header = T)
#默认person相关，算相关性r，可以切换其他算法
a.cor <- cor(co)#cor(co, method = 'spearman/kendall')
#用Hmisc带***
library(Hmisc)
library(corrplot)
co2 <- rcorr(as.matrix(co))#算相关性，得到r和p
#粗略出图
corrplot(co2$r, method = c('circle'), type = c('upper'), title = 'test')
#进阶
corrplot(co2$r, p.mat = co2$P, method = c('circle'), type = c('upper'), insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1.0, pch.col = "red", tl.col='blue')
#配上p检验*标志的装逼图
corrplot.mixed(co2$r, p.mat = co2$P, #确定数据源
               insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1.0, pch.col = "red",#添加p标注信息 
               upper = 'ellipse', lower.col = 'black', number.cex= .6,#设置上半部
               tl.col='blue', tl.pos = 'lt',#设置标签
               title = "Good")#设置大标题
write.table(co2$r,'/Users/yanjiacheng/Desktop/毕业数据/co.r.txt',sep = '\t')
write.table(co2$p,'/Users/yanjiacheng/Desktop/毕业数据/co.P.txt',sep = '\t')
write.table(co2$n,'/Users/yanjiacheng/Desktop/毕业数据/co.n.txt',sep = '\t')

