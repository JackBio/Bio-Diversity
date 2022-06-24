#R 4.0
#读取excel文档xlsx的数据
library(readxl)
hplc <- read_excel(path = "/Users/yanjiacheng/Desktop/毕业数据/液相总表.xlsx", sheet = 2, col_names = T)
hplc$ids <- as.factor(hplc$ids)#否则后期glht(model, linfct())报错‘character’不是model的factor
#剥离所需数据
p <- hplc[1:30,15:16]
b <- hplc[31:60,15:16]
t_b <- hplc[61:90,15:16]
#是否正态分布（参数检验）p>0.05时为正态分布，非正态就用非参数
shapiro.test(p$val) #符合，样本df=n-1，总体df=n
shapiro.test(b$val) #不符合，去非参数或标准化
shapiro.test(t_b$val)#不符合，去非参数
#用Q-Q图展示正态，点应近似地在一条直线附近，斜率为标准差，截距为均值
library(car)
qqPlot(lm(val ~ ids, data=p),simulate=T)
qqPlot(lm(val ~ ids, data=b),simulate=T)#不符合
qqPlot(lm(val ~ ids, data=t_b),simulate=T)#不符合
#符合正态的数据检测方差齐性，p>0.05时为齐性
bartlett.test(val ~ ids, data=p)
#非正态、正态的数据都能检测，p>0.05表示齐性，R升级后leveneTest变成levene_test
#leveneTest(val ~ ids, data=p)
levene_test(val ~ ids, data=p)
#leveneTest(val ~ ids, data=b)#不齐
levene_test(val ~ ids, data=b)
#leveneTest(val ~ ids, data=t_b)
levene_test(val ~ ids, data=t_b)
#使用方差分析
library(multcomp)
#各组样本数量、含量均值和标准差
attach(p)
table(ids)
aggregate(p$val, by = list(p$ids), FUN = mean)#若报错找不到val，用p$val
aggregate(p$val, by = list(p$ids), FUN = sd)
#检验组间差异ANOVA
fit <- aov(val ~ ids)
summary(fit)
#多组Kruskal–Wallis的t检验
kruskal.test(val ~ ids, data = p)
#绘制各组均值及其置信区间的图形
library(gplots)
plotmeans(val ~ ids, xlab = '组名\nids', ylab = '含量\nval µg/g', main = '平均数MEAN\n95%CI置信区间')
detach(p)
#使用TukeyHSD多重比较（LSD、SSR、Q...?）准确来说是aov再tukey做posthoc
TukeyHSD(fit)
par(las = 1)#标签水平放置，2表示竖直放置
par(mar = c(5,8,4,2))#底左顶右的留白，增大左边界的空余面积
plot(TukeyHSD(fit))
par(mar = c(5,4,6,2))#增大了顶部面积摆放字母
tuk <- glht(fit, linfct = mcp(ids = 'Tukey'))#ids是可变的，与前文ids符合，示例叫tension
pdf('PTX.pdf',width = 4, height = 3)
plot(cld(tuk, level = .05), col = p$ids) 
dev.off()
#巴卡亭多重比较
attach(b)
fit2 <- aov(val ~ ids)
summary(fit2)
tuk2 <- glht(fit2, linfct = mcp(ids = 'Tukey'))
plot(cld(tuk2, level = .05), col = 'red')
detach(b)
#10-巴卡亭多重比较
attach(t_b)
fit3 <- aov(val ~ ids)
summary(fit3)
tuk3 <- glht(fit3, linfct = mcp(ids = 'Tukey'))
par(las = 2)#名称太长需要旋转
plot(cld(tuk3, level = .05), col = 'blue',)
detach(t_b)
#非参数检验，两样本用Wilcoxon秩和检验，三个或以上用Kruskal-Wallis秩和检验
kruskal.test(val ~ ids, data=b)#整体显著
kruskal.test(val ~ ids, data=t_b)#整体显著
#两两比较，pgirmess::kruskalmc()得不到具体p值，用PMCMR::Nemenyi()
#PMCER报错，用PMCMRplus::kwAllPairsNemenyiTest()
library(pgirmess)
#library(spdep)
kruskalmc(val ~ ids, data = b, probs = 0.05)#但得到整个hplc两两比较结果且不对劲
library(PMCMRplus)
#library(PMCMR)
#posthoc.kruskal.conover.test(val ~ ids, data = b, probs = 0.05)被提示defunct，Use 'PMCMRplus::kwAllPairsConoverTest' instead.
PMCMRplus::kwAllPairsConoverTest(val ~ ids, data = b, probs = 0.05)#posthoc=nemenyi
#posthoc.kruskal.nemenyi.test(val ~ ids, data = b)同上报错
PMCMRplus::kwAllPairsNemenyiTest(val ~ ids, data = b)#未校正，报警Ties are present, p-values are not corrected.
#posthoc.kruskal.nemenyi.test(val ~ ids, data = b, dist = "Chisq")
PMCMRplus::kwAllPairsNemenyiTest(val ~ ids, data = b, dist = "Chisq")#校正，报警Ties are present. Chi-sq was corrected for ties.
bt = kwAllPairsNemenyiTest(val ~ ids, data = b, dist = "Chisq") 
#posthoc.kruskal.nemenyi.test(val ~ ids, data = t_b, dist = "Chisq")
PMCMRplus::kwAllPairsNemenyiTest(val ~ ids, data = t_b, dist = "Chisq")
tt = kwAllPairsNemenyiTest(val ~ ids, data = t_b, dist = "Chisq") 
library(dplyr)#使用管道符
library(rstatix)#add sig
ad = adjust_pvalue(tt, method = 'bonferroni') %>%
  rstatix::add_significance()#结果没变，因为之前已经校正了
#画箱线图，配置背景，最后拿去PPT手动标星星
library(ggThemeAssist)
library(ggplot2)
bk <- ggplot(data = b, aes(x = ids, y = val, col = ids)) + geom_boxplot()
ggThemeAssistGadget(bk)#名称必须是纯字母才能识别为ggplot的“对象”
bk + theme(panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = "gray100"),
    legend.background = element_rect(fill = NA)) +labs(y = "val µg/g") +
    annotate("text", x = c(1,2,3,4,5,6), y = 120, 
             label = c("a","a","ab","ab","ab","b"), colour = 'black', size = 5) +
  labs(tag = "B")
######借鉴后文(正文图先出)######
bk + theme(panel.background = element_rect(fill = "gray100"), 
                       legend.key = element_rect(fill = "gray100"),
                       legend.background = element_rect(fill = NA),
                       legend.text = element_text(size = 17),
                       axis.title = element_text(size = 20),
                       axis.text.x = element_text(size = 17, colour = 'black'),
                       axis.text.y = element_text(size = 17),
                       plot.title = element_text(size = 25,hjust = -0.17, vjust = 0.27)) + 
  labs(y = "val µg/g", x = 'Groups')+
  annotate("text", x = c(1,2,3,4,5,6), y = 120, label = c("a","a","ab","ab","ab","b"), colour = 'black', size = 7.5)
ggsave('bacctine.pdf',dpi = 600,width = 7, height = 3)


library(ggpubr)#主要就是它了，会出星星——但我最后还是用的字母标记法,不会多重比较的分组标记,参https://www.cnblogs.com/shuaihe/p/15316615.html
library(ggsignif)#添加线段
library(rstatix)
library(patchwork)#添加编号,参https://blog.csdn.net/weixin_42350411/article/details/113316500
p.com = ggplot(data = t_b, aes(x = ids, y = val, col = ids)) + 
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "gray100"), 
                        legend.key = element_rect(fill = "gray100"),
                        legend.background = element_rect(fill = NA),
                        legend.text = element_text(size = 17),
                        axis.title = element_text(size = 20),
                        axis.text.x = element_text(size = 17, colour = 'black'),
                        axis.text.y = element_text(size = 17),
                        plot.title = element_text(size = 25,
                                  hjust = -0.17, vjust = 0.27)) +
                        labs(title = "a", y = "val µg/g", x = 'Groups')+
         
  #ggtitle(label = 'A') + #本图用title也可以达成编号的目标，但位置不够左上
  
  #annotate("text", x = 1, y = 800, label = "A", colour = 'black', size = 5) + #图编号,也可以geom_text(),但位置不太好弄
  annotate("text", x = c(1,2,3,4,5,6), y = 800, label = c("a","a","ab","ab","ab","b"), colour = 'black', size = 7.5) #+ #去tt里看p,用字母标记法 
  #labs(tag = "a",size = 15) #+ #可以使左上角标a但不能改变size，也可以设置title调节hjust/vjust

  stat_compare_means(aes(group = ids), label = 'p.format') +
  add_xy_position(t_b, x = 'Groups',y = 'val µg/g', dodge = 0.8) + #Error in is.data.frame(x) : argument "test" is missing, with no default
  stat_pvalue_manual(t_b, color = 'ids', step.group.by = 'ids', 
                     tip.length = o, step.increase = 0.1)
ggsave('多重比较.pdf',dpi = 600,width = 7,height = 3)#当前路径上一个ggplot图保存
#par(mfrow=c(1, 3))#1行3列出图，以下举例
#plot(x = b$ids, y = b$val)
#plot(b$ids~b$val, col = b$ids)
#plot(t_b$ids~t_b$val)
#plot(p$ids~p$val)

#预期sci的figure1所有图片  
patchwork <- (p.com + p.tre) / p.hea + plot_annotation(tag_levels = 'A') & #"A"表示“ABC”计数,必须凑齐所有的图再一起编号,代码不能单独设置"B"或者"C"
    theme(plot.tag = element_text(size = 20))#来自patchwork包,位置最合适,且后续还能拼图                       
  