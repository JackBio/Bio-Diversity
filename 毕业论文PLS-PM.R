library(plspm)
setwd('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/')
#fbsm = read.table("fbsm.txt",head=T,row.names=1) %>% t()
#fotu.name = read.table("fOTU.txt",head=T)
#botu.name = read.table("bOTU.txt",head=T)
#OTU数据太大后期出不来图，回过头来先对OTU分类汇总(毕业论文生态位计算.R)
library(readxl)
b.otu <- read_xlsx('细菌抽平后OTU.xlsx', sheet = 1)
f.otu <- read_xlsx('真菌抽平后OTU.xlsx', sheet = 1)
m.dat <- read.table('medicine.txt', header = T)
s.dat <- read.table('soil.txt', header = T)
library(dplyr)
b.otu$tc3mean <- rowSums(b.otu[,10:14]) / 5
b.otu$tc5mean <- rowSums(b.otu[,15:19]) / 5
b.otu$tc9mean <- rowSums(b.otu[,20:24]) / 5
b.otu$wx3mean <- rowSums(b.otu[,25:29]) / 5
b.otu$wx5mean <- rowSums(b.otu[,30:34]) / 5
b.otu$wx9mean <- rowSums(b.otu[,35:39]) / 5
f.otu$tc3mean <- rowSums(f.otu[,10:14]) / 5
f.otu$tc5mean <- rowSums(f.otu[,15:19]) / 5
f.otu$tc9mean <- rowSums(f.otu[,20:24]) / 5
f.otu$wx3mean <- rowSums(f.otu[,25:29]) / 5
f.otu$wx5mean <- rowSums(f.otu[,30:34]) / 5
f.otu$wx9mean <- rowSums(f.otu[,35:39]) / 5
m.dat$tc3mean <- rowSums(m.dat[,2:6]) / 5
m.dat$tc5mean <- rowSums(m.dat[,7:11]) / 5
m.dat$tc9mean <- rowSums(m.dat[,12:16]) / 5
m.dat$wx3mean <- rowSums(m.dat[,17:21]) / 5
m.dat$wx5mean <- rowSums(m.dat[,22:26]) / 5
m.dat$wx9mean <- rowSums(m.dat[,27:31]) / 5
s.dat$tc3mean <- rowSums(s.dat[,2:6]) / 5
s.dat$tc5mean <- rowSums(s.dat[,7:11]) / 5
s.dat$tc9mean <- rowSums(s.dat[,12:16]) / 5
s.dat$wx3mean <- rowSums(s.dat[,17:21]) / 5
s.dat$wx5mean <- rowSums(s.dat[,22:26]) / 5
s.dat$wx9mean <- rowSums(s.dat[,27:31]) / 5
b.flhz.tc3 <- aggregate(x = b.otu$tc3mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3=x)
b.flhz.tc5 <- aggregate(x = b.otu$tc5mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5=x)
b.flhz.tc9 <- aggregate(x = b.otu$tc9mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9=x)
b.flhz.wx3 <- aggregate(x = b.otu$wx3mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3=x)
b.flhz.wx5 <- aggregate(x = b.otu$wx5mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5=x)
b.flhz.wx9 <- aggregate(x = b.otu$wx9mean, by = list(b.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9=x)
b.flhz <- cbind(b.flhz.tc3,b.flhz.tc5$tc5,b.flhz.tc9$tc9,
                   b.flhz.wx3$wx3,b.flhz.wx5$wx5,b.flhz.wx9$wx9)
names(b.flhz) = c('ID','tc3','tc5','tc9','wx3','wx5','wx9')
f.flhz.tc3 <- aggregate(x = f.otu$tc3mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3=x)
f.flhz.tc5 <- aggregate(x = f.otu$tc5mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5=x)
f.flhz.tc9 <- aggregate(x = f.otu$tc9mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9=x)
f.flhz.wx3 <- aggregate(x = f.otu$wx3mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3=x)
f.flhz.wx5 <- aggregate(x = f.otu$wx5mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5=x)
f.flhz.wx9 <- aggregate(x = f.otu$wx9mean, by = list(f.otu$Phylum), FUN = sum, 
                           simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9=x)
f.flhz <- cbind(f.flhz.tc3,f.flhz.tc5$tc5,f.flhz.tc9$tc9,
                   f.flhz.wx3$wx3,f.flhz.wx5$wx5,f.flhz.wx9$wx9)
names(f.flhz) = c('ID','tc3','tc5','tc9','wx3','wx5','wx9')




##结构模型
#先假设模型，大框架是下三角的布尔化矩阵，自身不能相关，对角线及右上角都是0，左下角按“有无”分“1-0”
#筛因子和调关系多试一试。变量的顺序是否值得考虑？盲目按矩阵分布会与假设的指向有矛盾啊
#  s b f m
#s 0 0 0 0
#b 1 0 0 0
#f 1 1 0 0
#m 0 1 1 0
soil <- c(0,0,0,0)
bacteria <- c(1,0,0,0)
fungi <- c(1,1,0,0)
medicine <- c(0,1,1,0)
path <- rbind(soil,bacteria,fungi,medicine)
colnames(path) = rownames(path)
path
innerplot(path)

##测量模型
#blocks <- list(10299:10305,3160:10298,2:3159,10306:10308)
test = rbind(b.flhz,f.flhz)
write.table(test, '分类汇总门级总表.txt',row.names = F,col.names = T)
f.b. <- read.table('分类汇总门级总表.txt',header = T,row.names = 1)
m.dat <- cbind(m.dat$ID,m.dat[,32:37])
names(m.dat) <- c('ID','tc3','tc5','tc9','wx3','wx5','wx9')#行名相同后面才能bind
s.dat <- cbind(s.dat$ID,s.dat[,32:37])
names(s.dat) <- c('ID','tc3','tc5','tc9','wx3','wx5','wx9')
sbfm <- rbind(s.dat, test, m.dat)
write.csv(sbfm, 'plspm.sbfm.csv',row.names = F,col.names = F)
sbfm <- read.csv('plspm.sbfm.csv', header = T, row.names = 1) %>% t()
#blocks <- list(s.dat[1:6,1:7],b.flhz[1:6,8:49],f.flhz[1:6,50:64],m.dat[1:6,65:67])
#blocks <- list(1,3:5,7,8:49,50:64,65:67)#去掉了VIF筛除的因子
blocks <- list(1,3:5,7,8:49,50:64,67)#去掉了VIF筛除的因子和其他药用成分
#blocks <- list(1:7,8:49,50:64,65:67)
blocks <- list(
  c('moisture','NH4','AP','AK','pH'),
#  c('moisture','NO3','NH4','AP','AK','TOC','pH'),
  b.flhz$ID,
  f.flhz$ID,
#  c('paclitaxel','baccatineIII','10-DAB')
  c('10-DAB')
)
mod <- rep('A', 6)# <- c('A','A','A','A','A','A')
#fbsm = rbind(t(s.dat[32:37,]),t(b.flhz[2:7,]),t(f.flhz[2:7,]),t(m.dat[32:37,]))
pls = plspm(Data = sbfm, path_matrix = path, blocks = blocks, modes = mod)
pls$path_coefs
pls$gof#最好大于0.7
pls$inner_model
summary(pls) #pls, and THE OUTER MODEL: loading shoud greater than 0.7
innerplot(pls) #plot()
plot(pls, what = "loadings", arr.width = 0.1)
plot(pls, what = "weights", arr.width = 0.1)
plot(pls, what = "inner", 
     arr.width = 0.1, arr.pos = 0.7, cex.txt = 0.65, #箭头宽度、位置以及箭头上文字大小
     colpos = 'red', colneg = 'blue', lcol = NA, txt.col = 'black', #表示正负、边框、文字的颜色
     box.prop = 0.6, box.size = 0.1, box.cex = 0.8, box.col = 'pink', #铭牌比例、大小、文字大小和填充色
     #lwd = 1, box.lwd = 2, 
     )
#试过好几次，fun-med的系数>1，即存在较大共线性；指向也是负，不好解释。考虑VIF筛查或用NMDS数据
#1.可以计算X矩阵的秩qr(X)$rank，如果不是满秩的，说明其中有Xi可以用其他的X的线性组合表示；
#2.也可以计算条件数kappa(X)，k<100,说明共线性程度小；如果100<k<1000，则存在较多的多重共线性;若k>1000，存在严重的多重共线性。处理时可以进行逐步回归，用step()命令,比如一开始的模型是fm=lm(),step(fm)选择最小AIC信息统计量就可以了。这种方法是排除引起共线性的变量，是解决多重共线性的比较常用方法！
#3.可以使用方差膨胀因子(VIF)得到各个系数的方差膨胀因子，当0<VIF<10，不存在多重共线性（注意：在《R语言实战》第2版P182中认为VIF>4就存在多重共线性）；当10≤VIF<100，存在较强的多重共线性，当VIF>=100，多重共线性非常严重。是判断多重共线性的比较常用方法！
#岭回归，R中的MASS包提供了lm.ridge()函数用于实现岭回归。
#lasso回归，R中的lars包可以用于计算lasso回归。
#适应性lasso回归，R中的msgps包不仅提供了计算适应性lasso（alasso）回归的方法，还提供了弹性网络及广义弹性网络等方法。其最优策略包括Cp统计量、AIC信息准则、GCV准则以及BIC信息准则。
#偏最小二乘回归有点类似于主成分回归。R中的pls包可以用来计算偏最小二乘回归。
#VIF的值可以用软件包car中的vif()函数计算，条件数κ可以用R内置函数kappa()计算。一般认为，当κ>15时，有共线性问题，当κ>30时，说明有严重的共线性问题。

library(car)
vif.sbfm <- read.csv('plspm.sbfm.csv',header = T,row.names = 1) %>% t()

#系数大于1主要三个问题：数据没有标准化；存在很大共线性；模型结构设计或因子数量不合理
library(vegan)
library(ggrepel)
sbfm = decostand(sbfm, method='standardize', na.rm=T)
class(sbfm)#'data' must be a data.frame, not a matrix or an array. And as.data.frame() doesnt work
#write.csv(sbfm,'筛共线性.csv')
data <- read.csv('筛共线性（手作初表1）.csv', row.names = 1, header = TRUE)
class(data)
data = decostand(data, 'log', na.rm=T)#na.rm will effect it as well, but pH had been '-4.1...', result to NA 
spe.1 <- rda(data ~ moisture+NO3+NH4+AP+AK+TOC+pH+
               p__Abditibacteriota+p__Acidobacteriota+p__Actinobacteriota+p__Armatimonadota+	
               p__Bacteroidota+p__Bdellovibrionota+p__Chloroflexi+p__Cyanobacteria+p__Dadabacteria+p__Deferrisomatota+p__Deinococcota+
               p__Dependentiae+p__Desulfobacterota+p__Elusimicrobiota+p__Entotheonellaeota+p__FCPU426+p__Fibrobacterota+p__Firmicutes+
               p__Fusobacteriota+p__GAL15+p__Gemmatimonadota+p__Halanaerobiaeota+p__Hydrogenedentes+p__Latescibacterota+p__MBNT15+
               p__Methylomirabilota+p__Myxococcota+p__NB1-j+p__Nitrospirota+p__Patescibacteria+p__Planctomycetota+p__Proteobacteria+
               p__RCP2_54+p__Rs-K70_termite_group+p__SAR324_cladeMarine_group_B+p__Spirochaetota+p__Sumerlaeota+p__Synergistota+
               p__unclassified_k__norank_d__Bacteria+p__Verrucomicrobiota+p__WPS-2+p__WS2+p__Ascomycota+p__Basidiobolomycota+
               p__Basidiomycota+p__Blastocladiomycota+p__Calcarisporiellomycota+p__Chytridiomycota+p__Glomeromycota+p__Kickxellomycota+
               p__Monoblepharomycota+p__Mortierellomycota+p__Mucoromycota+p__Olpidiomycota+p__Rozellomycota+p__unclassified_k__Fungi+p__Zoopagomycota+
               paclitaxel+baccatineIII+DAB, data=data)#unexpected symbol in:ERROR:10DAB >all for '10' &#Error in terms.formula() invalid model formula in ExtractVars
spe.1 <- rda(data ~ moisture+NO3+NH4+AP+AK+TOC+pH, data = data)#factors more than groups will collapse model
vif.cca(spe.1)
spe.1 <- rda(data ~ paclitaxel+baccatineIII+DAB, data=data)
vif.cca(spe.1)

s.dat.1 = t(s.dat) %>% as.data.frame()
colnames(s.dat.1)=s.dat.1[1,]
s.dat.1=s.dat.1[-1,]
x1 <- cca(s.dat.1 ~ moisture+NO3+NH4+AP+AK+TOC+pH, data=s.dat.1)#Error:Error in rowSums(X) : 'x' must be numeric
sapply(s.dat.1,class)#rownames are chr
write.csv(s.dat.1,'s.dat.1.csv')
s.dat.1 = read.csv('s.dat.1.csv', row.names = 1)
#x1 <- cca(s.dat.1 ~ moisture+NO3+NH4+AP+AK+TOC+pH, data=s.dat.1)#cca(s.dat.1) will error:can be used only with constrained ordination
x1 <- cca(s.dat.1 ~ moisture+NO3+NH4, data=s.dat.1)#last 2 will be NA,maybe caused by degrees of freedom?
vif.cca(x1)
x2 <- cca(s.dat.1 ~ NH4+AP+AK+TOC+pH, data=s.dat.1)
vif.cca(x2)#results are different...

#####
write.csv(pls$outer_model,'pls.outer_model.csv')#filter |loading|>0.7 by Excel.app
#sbfm.LoadingSelected <- cbind(data$moisture,data$NO3,......)
#条件格式-介于-0.7～0.7标红-复制loading转置于'筛共线性.csv'，删除有红色的列
sbfm.LoadingSelected <- read.csv('loadingselected.csv', header = T, row.names = 1)
Id <- read.csv('loadingselected.names.csv')
blocks.new <- list(1:6,7:16,17:22,23:25)
blocks.new <- list(Id[1:6,1],Id[7:16,1],Id[17:22,1],Id[23:25,1])
mod.new <- rep('A', 6)
pls.new = plspm(Data = sbfm.LoadingSelected, path_matrix = path, blocks = blocks.new, modes = mod.new, boot.val = 100)
summary(pls.new)
pls.new$gof
pls.new$outer_model
plot(pls.new, what = "inner", 
     arr.width = 0.1, arr.pos = 0.72, cex.txt = 0.8,
     colpos = 'red', colneg = 'blue', lcol = NA, txt.col = 'black',
     box.prop = 0.6, box.size = 0.1, box.cex = 1.3, box.col = 'pink',
)

###NMDS & all simples without binding
soil <- c(0,0,0,0)
bacteria <- c(1,0,0,0)
fungi <- c(1,1,0,0)
medicine <- c(0,1,1,0)
path <- rbind(soil,bacteria,fungi,medicine)
colnames(path) = rownames(path)
damn <- read.csv('NMDS-env-med(t).csv',row.names = 1, header = T)
damn.name <- read.csv('NMDS-env-med.csv')
blocks.damn <- list(1:2,3:4,5:11,12:14)
blocks.damn <- list(damn.name[1:2,1],damn.name[3:4,1],damn.name[5:11,1],c('bacctineIII','DAB','paclitaxel'))#不识别10DAB，因为10在前
mod.damn <- rep('A', 30)
pls.damn = plspm(Data = damn, path_matrix = path, blocks = blocks.damn, modes = mod.damn, boot.val = 100)
summary(pls.damn)
pls.damn$gof
pls.damn$outer_model
blocks.damn.filtered <- list(2,4,5:10,12:13)
blocks.damn.filtered <- list(c('fNMDS2'),c('bNMDS2'),damn.name[5:10,1],c('bacctineIII','DAB'))
pls.damn.filtered = plspm(Data = damn, path_matrix = path, blocks = blocks.damn.filtered, modes = mod.damn, boot.val = 100)
pls.damn.filtered$gof
pls.damn.filtered$outer_model
plot(pls.damn.filtered, what = "inner", 
     arr.width = 0.1, arr.pos = 0.72, cex.txt = 0.8,
     colpos = 'red', colneg = 'blue', lcol = NA, txt.col = 'black',
     box.prop = 0.6, box.size = 0.1, box.cex = 1.3, box.col = 'pink'
)

##########
s.dat.1 = damn[,5:11]
x <- cca(s.dat.1 ~ water+NO3+NH4+AP+AK+TC+pH, data=s.dat.1)
vif.cca(x)#it worked! and delete which above 20: NO3&TC, as same as Majorbio.platform
plot(x)

######应该用所有样本算，而不是合并分组？
btc3a <- aggregate(x = b.otu$tc3a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3a=x)
btc3b <- aggregate(x = b.otu$tc3b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3b=x)
btc3c <- aggregate(x = b.otu$tc3c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3c=x)
btc3d <- aggregate(x = b.otu$tc3d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3d=x)
btc3e <- aggregate(x = b.otu$tc3e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3e=x)
btc5a <- aggregate(x = b.otu$tc5a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5a=x)
btc5b <- aggregate(x = b.otu$tc5b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5b=x)
btc5c <- aggregate(x = b.otu$tc5c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5c=x)
btc5d <- aggregate(x = b.otu$tc5d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5d=x)
btc5e <- aggregate(x = b.otu$tc5e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5e=x)
btc9a <- aggregate(x = b.otu$tc9a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9a=x)
btc9b <- aggregate(x = b.otu$tc9b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9b=x)
btc9c <- aggregate(x = b.otu$tc9c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9c=x)
btc9d <- aggregate(x = b.otu$tc9d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9d=x)
btc9e <- aggregate(x = b.otu$tc9e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9e=x)
bwx3a <- aggregate(x = b.otu$wx3a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3a=x)
bwx3b <- aggregate(x = b.otu$wx3b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3b=x)
bwx3c <- aggregate(x = b.otu$wx3c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3c=x)
bwx3d <- aggregate(x = b.otu$wx3d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3d=x)
bwx3e <- aggregate(x = b.otu$wx3e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3e=x)
bwx5a <- aggregate(x = b.otu$wx5a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5a=x)
bwx5b <- aggregate(x = b.otu$wx5b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5b=x)
bwx5c <- aggregate(x = b.otu$wx5c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5c=x)
bwx5d <- aggregate(x = b.otu$wx5d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5d=x)
bwx5e <- aggregate(x = b.otu$wx5e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5e=x)
bwx9a <- aggregate(x = b.otu$wx9a, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9a=x)
bwx9b <- aggregate(x = b.otu$wx9b, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9b=x)
bwx9c <- aggregate(x = b.otu$wx9c, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9c=x)
bwx9d <- aggregate(x = b.otu$wx9d, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9d=x)
bwx9e <- aggregate(x = b.otu$wx9e, by = list(b.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9e=x)

ftc3a <- aggregate(x = f.otu$tc3a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3a=x)
ftc3b <- aggregate(x = f.otu$tc3b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3b=x)
ftc3c <- aggregate(x = f.otu$tc3c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3c=x)
ftc3d <- aggregate(x = f.otu$tc3d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3d=x)
ftc3e <- aggregate(x = f.otu$tc3e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc3e=x)
ftc5a <- aggregate(x = f.otu$tc5a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5a=x)
ftc5b <- aggregate(x = f.otu$tc5b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5b=x)
ftc5c <- aggregate(x = f.otu$tc5c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5c=x)
ftc5d <- aggregate(x = f.otu$tc5d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5d=x)
ftc5e <- aggregate(x = f.otu$tc5e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc5e=x)
ftc9a <- aggregate(x = f.otu$tc9a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9a=x)
ftc9b <- aggregate(x = f.otu$tc9b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9b=x)
ftc9c <- aggregate(x = f.otu$tc9c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9c=x)
ftc9d <- aggregate(x = f.otu$tc9d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9d=x)
ftc9e <- aggregate(x = f.otu$tc9e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,tc9e=x)
fwx3a <- aggregate(x = f.otu$wx3a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3a=x)
fwx3b <- aggregate(x = f.otu$wx3b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3b=x)
fwx3c <- aggregate(x = f.otu$wx3c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3c=x)
fwx3d <- aggregate(x = f.otu$wx3d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3d=x)
fwx3e <- aggregate(x = f.otu$wx3e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx3e=x)
fwx5a <- aggregate(x = f.otu$wx5a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5a=x)
fwx5b <- aggregate(x = f.otu$wx5b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5b=x)
fwx5c <- aggregate(x = f.otu$wx5c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5c=x)
fwx5d <- aggregate(x = f.otu$wx5d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5d=x)
fwx5e <- aggregate(x = f.otu$wx5e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx5e=x)
fwx9a <- aggregate(x = f.otu$wx9a, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9a=x)
fwx9b <- aggregate(x = f.otu$wx9b, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9b=x)
fwx9c <- aggregate(x = f.otu$wx9c, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9c=x)
fwx9d <- aggregate(x = f.otu$wx9d, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9d=x)
fwx9e <- aggregate(x = f.otu$wx9e, by = list(f.otu$Phylum), FUN = sum, 
                   simplify = TRUE, drop = TRUE) %>% select(Phylum=Group.1,wx9e=x)

otub = cbind(btc3a,btc3b,btc3c,btc3d,btc3e,
             btc5a,btc5b,btc5c,btc5d,btc5e,
             btc9a,btc9b,btc9c,btc9d,btc9e,
             bwx3a,bwx3b,bwx3c,bwx3d,bwx3e,
             bwx5a,bwx5b,bwx5c,bwx5d,bwx5e,
             bwx9a,bwx9b,bwx9c,bwx9d,bwx9e)
otub = otub[,c(-3,-5,-7,-9,
               -11,-13,-15,-17,-19,
               -21,-23,-25,-27,-29,
               -31,-33,-35,-37,-39,
               -41,-43,-45,-47,-49,
               -51,-53,-55,-57,-59)]
otub
rownames(otub) = otub[,1]
otub = otub[,-1]
otub

otuf = cbind(ftc3a,ftc3b,ftc3c,ftc3d,ftc3e,
             ftc5a,ftc5b,ftc5c,ftc5d,ftc5e,
             ftc9a,ftc9b,ftc9c,ftc9d,ftc9e,
             fwx3a,fwx3b,fwx3c,fwx3d,fwx3e,
             fwx5a,fwx5b,fwx5c,fwx5d,fwx5e,
             fwx9a,fwx9b,fwx9c,fwx9d,fwx9e)
otuf = otuf[,c(-3,-5,-7,-9,
               -11,-13,-15,-17,-19,
               -21,-23,-25,-27,-29,
               -31,-33,-35,-37,-39,
               -41,-43,-45,-47,-49,
               -51,-53,-55,-57,-59)]
otuf
rownames(otuf) = otuf[,1]
otuf = otuf[,-1]
otuf

otu = rbind(otub,otuf)
env.med = damn[,5:14] %>% t()
fine = rbind(otu,env.med)
fine = decostand(fine, method='standardize', na.rm=T) 
fine.id = row.names(fine) %>% as.data.frame()
fine = t(fine)
nrow(otub)#It also nrow(na.omit(data)), but not 'rowSums(otub)'
nrow(otuf)#BTW, View() can see it too at bottom

soil <- c(0,0,0,0)
bacteria <- c(1,0,0,0)
fungi <- c(1,1,0,0)
medicine <- c(0,1,1,0)
path <- rbind(soil,bacteria,fungi,medicine)
colnames(path) = rownames(path)
path
block.fine <- list(1:42, 43:57, 58:64, 65:67)
block.fine <- list(fine.id[1:42,],fine.id[43:57,],fine.id[58:64,],fine.id[65:67,])
mod.fine <- rep('A', 30)
pls.fine = plspm(Data = fine, path_matrix = path, blocks = block.fine, modes = mod.fine, boot.val = 100)
pls.fine$gof
summary(pls.fine)
write.csv(fine,'fine0.csv')
write.csv(pls.fine$outer_model,'fine1.csv')#条件格式标记-0.7～0.7,loading转置到'fine0'里手动筛掉它们,为'fine2'
fine2 <- read.csv('fine2.csv', row.names = 1, header = T)
fine2.id = colnames(fine2) %>% as.data.frame()
block.fine2 <- list(1:24, 25:32, 33:38, 39:40)
block.fine2 <- list(fine2.id[1:24,],fine2.id[25:32,],fine2.id[33:38,],fine2.id[39:40,])
pls.fine2 = plspm(Data = fine2, path_matrix = path, blocks = block.fine2, modes = mod.fine, boot.val = 100)
pls.fine2$gof
summary(pls.fine2)
plot(pls.fine2, what = "inner", 
     arr.width = 0.1, arr.pos = 0.72, cex.txt = 0.8,
     colpos = 'red', colneg = 'blue', lcol = NA, txt.col = 'black',
     box.prop = 0.6, box.size = 0.1, box.cex = 1.3, box.col = 'pink')
####还是不对，标准化后还是出现大于1的路径系数

