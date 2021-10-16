#对各样本OTU按Phylum分类汇总和生态位计算

#设置路径并读入数据
setwd('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息')
library(readxl)
botu <- read_xlsx('细菌抽平后OTU.xlsx', sheet = 1)
fotu <- read_xlsx('真菌抽平后OTU.xlsx', sheet = 1)

#本文使用aggregate函数进行求和合并、分类汇总。
#求均值与求和差别意义不大，建议sum，后文计算spec.gen()要求整数，此处以平均数为例
library(dplyr)
botu$tc3sum <- rowSums(botu[,10:14])
botu$tc3mean = botu$tc3sum / 5
botu$tc5mean <- rowSums(botu[,15:19]) / 5
botu$tc9mean <- rowSums(botu[,20:24]) / 5
botu$wx3mean <- rowSums(botu[,25:29]) / 5
botu$wx5mean <- rowSums(botu[,30:34]) / 5
botu$wx9mean <- rowSums(botu[,35:39]) / 5
botu.flhz.tc3 <- aggregate(x = botu$tc3mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3=x)
botu.flhz.tc5 <- aggregate(x = botu$tc5mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5=x)
botu.flhz.tc9 <- aggregate(x = botu$tc9mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9=x)
botu.flhz.wx3 <- aggregate(x = botu$wx3mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3=x)
botu.flhz.wx5 <- aggregate(x = botu$wx5mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5=x)
botu.flhz.wx9 <- aggregate(x = botu$wx9mean, by = list(botu$Phylum), FUN = sum, 
                                      simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9=x)
botu.flhz <- cbind(botu.flhz.tc3,botu.flhz.tc5$tc5,botu.flhz.tc9$tc9,
                   botu.flhz.wx3$wx3,botu.flhz.wx5$wx5,botu.flhz.wx9$wx9)
names(botu.flhz) = c('Phylum','tc3','tc5','tc9','wx3','wx5','wx9')
fotu$tc3mean <- rowSums(fotu[,10:14]) / 5
fotu$tc5mean <- rowSums(fotu[,15:19]) / 5
fotu$tc9mean <- rowSums(fotu[,20:24]) / 5
fotu$wx3mean <- rowSums(fotu[,25:29]) / 5
fotu$wx5mean <- rowSums(fotu[,30:34]) / 5
fotu$wx9mean <- rowSums(fotu[,35:39]) / 5
fotu.flhz.tc3 <- aggregate(x = fotu$tc3mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3=x)
fotu.flhz.tc5 <- aggregate(x = fotu$tc5mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5=x)
fotu.flhz.tc9 <- aggregate(x = fotu$tc9mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9=x)
fotu.flhz.wx3 <- aggregate(x = fotu$wx3mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3=x)
fotu.flhz.wx5 <- aggregate(x = fotu$wx5mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5=x)
fotu.flhz.wx9 <- aggregate(x = fotu$wx9mean, by = list(fotu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9=x)
fotu.flhz <- cbind(fotu.flhz.tc3,fotu.flhz.tc5$tc5,fotu.flhz.tc9$tc9,
                   fotu.flhz.wx3$wx3,fotu.flhz.wx5$wx5,fotu.flhz.wx9$wx9)
names(fotu.flhz) = c('Phylum','tc3','tc5','tc9','wx3','wx5','wx9')#我不会合并时直接改名
#写出数据，后文计算生态位宽度不能识别首列字符串，不太会改，出此下策
#导出后首列是无用的排序要删掉或者另加列名，否则读取异常
#write.table(botu.flhz, file = 'botu.分类汇总.txt', sep = '\t', row.names = T, col.names = NA)
#会了，先转置再删掉首行
write.table(t(botu.flhz),'botu.t(分类汇总).txt',row.names = T, col.names = F, sep = '\t')
write.table(t(fotu.flhz),'fotu.t(分类汇总).txt',row.names = T, col.names = F, sep = '\t')

#求门级生态位宽度
library(spaa)
botu.flhz.niche <- read.table('botu.t(分类汇总).txt', header = T, row.names = 1, sep = '\t') 
botu.niche_width <- niche.width(botu.flhz.niche, method = 'levins')
fotu.flhz.niche <- read.table('fotu.t(分类汇总).txt', header = T, row.names = 1, sep = '\t') 
fotu.niche_width <- niche.width(fotu.flhz.niche, method = 'levins')

#随机重排模拟，看特种和泛种
library(EcolUtils)
set.seed(10086)
botu.niche.spec_gen <- spec.gen(botu.flhz.niche * 5, niche.width.method = 'levins', perm.method = 'quasiswap', 
                     n = 1000, probs = c(0.025, 0.975))#要求输入整数，所以mean*5=sum
tail(botu.niche.spec_gen)
write.table(botu.niche.spec_gen, 'botu.niche.spec_gen.txt', sep = '\t', col.names = NA, quote = FALSE)
fotu.niche.spec_gen <- spec.gen(fotu.flhz.niche * 5, niche.width.method = 'levins', perm.method = 'quasiswap', 
                                n = 1000, probs = c(0.025, 0.975))
tail(fotu.niche.spec_gen)
write.table(fotu.niche.spec_gen, 'fotu.niche.spec_gen.txt', sep = '\t', col.names = NA, quote = FALSE)

#写一个总表
botu.niche <- cbind(botu.niche.spec_gen, t(botu.niche_width))
write.csv(botu.niche, 'botu.niche.csv')
fotu.niche <- cbind(fotu.niche.spec_gen, t(fotu.niche_width))
write.csv(fotu.niche, 'fotu.niche.csv')


