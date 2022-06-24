#R 4.0
#提前用Excel把数据处理好，各小组求和，第一列要OTU，格式等尽量排布好
library(readxl)
bac <- read_xlsx('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/细菌抽平后OTU.xlsx', sheet = 3, col_names = T)
fun <- read_xlsx('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/真菌抽平后OTU.xlsx', sheet = 3, col_names = T)
bac <- as.data.frame(bac)
fun <- as.data.frame(fun)
bac1 <- bac[,2:7]
fun1 <- fun[,2:7]
bac1[bac1>0]=1
fun1[fun1>0]=1
as.data.frame(bac1)
as.data.frame(fun1)
bac2 <- cbind(bac$OTU,bac1)
fun2 <- cbind(fun$OTU,fun1)
View(bac2)
View(fun2)
#画图，另还有脚本upsetplot.R可参考，保存PDF或者最大化plot模块后保存
library(UpSetR)
pdf('upsetB.pdf', width = 14, height = 7)
upset(bac2,#数据源
      nsets = 6,#6个分组
      nintersects = NA,#显示所有交集
      point.size = 2.0, line.size = 0.6, mb.ratio = c(0.5,0.5),#点大小、线宽度、图比例
      mainbar.y.label = 'OTU numbers',#y-axis名
      order.by = "degree", #按程度排序
      decreasing = TRUE,#降序
      matrix.color = "red", main.bar.color = "blue",sets.bar.color = "gray23",#点颜色、棒颜色、图注颜色
      set_size.show = T, set_size.numbers_size = 8)#图注数值显示和大小
dev.off()
pdf('upsetF.pdf', width = 15, height = 7)
upset(fun2,      
      nsets = 6,
      nintersects = NA,
      point.size = 2.0, line.size = 0.6, mb.ratio = c(0.5,0.5),
      mainbar.y.label = 'OTU numbers',
      order.by = "degree", 
      decreasing = TRUE,
      matrix.color = "red", main.bar.color = "blue",sets.bar.color = "gray23",
      set_size.show = T, set_size.numbers_size = 8)
dev.off()
