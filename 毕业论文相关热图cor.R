#R 4.0
co <- read.table('/Users/yanjiacheng/Desktop/毕业数据/液相做相关.txt', sep = '\t',header = T)
#默认person相关，算相关性r，可以切换其他算法
a.cor <- cor(co)#cor(co, method = 'spearman/kendall')
#用Hmisc带***
library(Hmisc)
library(corrplot)
co2 <- rcorr(as.matrix(co), type="spearman")#Hmisc算相关性，得到r和p，默认Pearson(要求“连续、正态、线性”)，建议Spearman省事
#粗略出图
corrplot(co2$r, method = c('circle'), type = c('upper'), title = 'test')
#进阶
corrplot(co2$r, p.mat = co2$P, method = c('circle'), type = c('upper'), insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1.5, pch.col = "red", tl.col='blue', tl.cex = 1.5)
#配上p检验*标志的装逼图
pdf('cor.pdf',width = 7,height = 7)
corrplot.mixed(co2$r, p.mat = co2$P, #确定数据源
               insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1.2, pch.col = "red",#添加p标注信息 
               upper = 'ellipse', lower.col = 'black', number.cex= .6,#设置上半部
               tl.col='blue', tl.pos = 'd', tl.cex = 1.2,#设置标签，lt左+上，d对角线，n无
               cl.pos = 'r',#色条位置，r右b下n无
               title(' ', hjust = -0.1, vjust = 0.5))#无效，这函数和ggplot不联通
               #addshade = 'positive', 
               #title = " ") #设置大标题，P里有NA报错，不知道怎么解决，选择无视
dev.off()
write.table(co2$r,'/Users/yanjiacheng/Desktop/毕业数据/Spco.r.txt',sep = '\t',col.names=NA)#第一列留空，不会错位
write.table(co2$P,'/Users/yanjiacheng/Desktop/毕业数据/Spco.P.txt',sep = '\t',col.names=NA)
write.table(co2$n,'/Users/yanjiacheng/Desktop/毕业数据/Spco.n.txt',sep = '\t',col.names=NA)
