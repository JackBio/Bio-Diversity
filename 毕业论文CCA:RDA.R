library(vegan)
#读取数据，OTU和env都需要处理，hellinger转化和log转化
sp.botu <- read.table('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/bOTU.txt', sep = '\t', header = T, row.names = 1)
sp.fotu <- read.table('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/fOTU.txt', sep = '\t', header = T, row.names = 1)
env <- read.table('/Users/yanjiacheng/Desktop/毕业数据/env_mena.txt', sep = '\t', header = T, row.names = 1)

sp.botu <- t(sp.botu)
sp.botu.hell <- decostand(sp.botu, 'hellinger')
sel.b <- decorana(sp.botu.hell)
sel.b

sp.fotu <- t(sp.fotu)
sp.fotu.hell <- decostand(sp.fotu, 'hellinger')
sel.f <- decorana(sp.fotu.hell)
sel.f
#DCA分析，第一轴(4, )=CCA;[3,4]=CCA/RDA;( ,3)=RDA，CCA比RDA普适，RDA可改良用db-RDA

#CCA:forward.sel, db-RDA:capscale

