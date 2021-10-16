#提前用Excel把数据处理好，各小组求和，第一列要OTU，格式等尽量排布好
library(readxl)
bac <- read_xlsx('/Users/yanjiacheng/Desktop/毕业数据/OTU信息/细菌抽平后OTU.xlsx', sheet = 3, col_names = T)
fun <- read_xlsx('/Users/yanjiacheng/Desktop/毕业数据/OTU信息/真菌抽平后OTU.xlsx', sheet = 3, col_names = T)
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
upset(bac2,nsets = 6,nintersects = 200,mb.ratio = c(0.5,0.5),order.by = "degree", decreasing = TRUE)
upset(fun2,nsets = 6,nintersects = 200,mb.ratio = c(0.5,0.5),order.by = "degree", decreasing = TRUE)
