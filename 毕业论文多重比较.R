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
shapiro.test(b$val) #不符合，去非参数
shapiro.test(t_b$val)#不符合，去非参数
#用Q-Q图展示正态，点应近似地在一条直线附近，斜率为标准差，截距为均值
library(car)
qqPlot(lm(val ~ ids, data=p),simulate=T)
qqPlot(lm(val ~ ids, data=b),simulate=T)#不符合
qqPlot(lm(val ~ ids, data=t_b),simulate=T)#不符合
#符合正态的数据检测方差齐性，p>0.05时为齐性
bartlett.test(val ~ ids, data=p)
#非正态、正态的数据都能检测，p>0.05表示齐性
leveneTest(val ~ ids, data=p)
leveneTest(val ~ ids, data=b)#不齐
leveneTest(val ~ ids, data=t_b)
#使用方差分析
library(multcomp)
#各组样本数量、含量均值和标准差
attach(p)
table(ids)
aggregate(val, by = list(ids), FUN = mean)
aggregate(val, by = list(ids), FUN = sd)
#检验组间差异ANOVA
fit <- aov(val ~ ids)
summary(fit)
#多组Kruskal–Wallis的t检验
kruskal.test(val ~ ids, data = p)
#绘制各组均值及其置信区间的图形
library(gplots)
plotmeans(val ~ ids, xlab = '组名\nids', ylab = '含量\nval', main = '平均数MEAN\n95%CI置信区间')
detach(p)
#使用TukeyHSD多重比较（LSD、SSR、Q...?）
TukeyHSD(fit)
par(las = 1)#标签水平放置，2表示竖直放置
par(mar = c(5,8,4,2))#底左顶右的留白，增大左边界的空余面积
plot(TukeyHSD(fit))
par(mar = c(5,4,6,2))#增大了顶部面积摆放字母
tuk <- glht(fit, linfct = mcp(ids = 'Tukey'))#ids是可变的，与前文ids符合
plot(cld(tuk, level = .05), col = 'lightgrey')
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
plot(cld(tuk3, level = .05), col = 'blue')
detach(t_b)
#非参数检验，两样本用Wilcoxon秩和检验，三个或以上用Kruskal-Wallis秩和检验
kruskal.test(val ~ ids, data=b)#整体显著
kruskal.test(val ~ ids, data=t_b)#整体显著
#两两比较，pgirmess::kruskalmc()得不到具体p值，用PMCMR::Nemenyi()
library(pgirmess)
#library(spdep)
kruskalmc(val ~ ids, data = b, probs = 0.05)#但得到整个hplc两两比较结果且不对劲
#library(PMCMRplus)
library(PMCMR)
posthoc.kruskal.conover.test(val ~ ids, data = b, probs = 0.05)
posthoc.kruskal.nemenyi.test(val ~ ids, data = b)#未校正，报警Ties are present, p-values are not corrected.
posthoc.kruskal.nemenyi.test(val ~ ids, data = b, dist = "Chisq")#校正，报警Ties are present. Chi-sq was corrected for ties.
posthoc.kruskal.nemenyi.test(val ~ ids, data = t_b, dist = "Chisq")
#画箱线图，配置背景，最后拿去PPT手动标星星
library(ggThemeAssist)
library(ggplot2)
bk <- ggplot(data = b, aes(x = ids, y = val, col = as.factor(ids))) + geom_boxplot()
ggThemeAssistGadget(bk)#名称必须是纯字母才能识别为ggplot的“对象”
ggplot(data = t_b, aes(x = ids, y = val, col = ids)) + geom_boxplot()+ theme(panel.background = element_rect(fill = "gray100"))
#par(mfrow=c(1, 3))#1行3列出图，以下举例
#plot(x = b$ids, y = b$val)
#plot(b$ids~b$val, col = b$ids)
#plot(t_b$ids~t_b$val)
#plot(p$ids~p$val)
