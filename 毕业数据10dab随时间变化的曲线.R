#读入数据，防止纯数字标题被加X，可以用check.names = F解决（第一版数据这样了，后来数据改了布局）
alldab <- read.table('/Users/yanjiacheng/Desktop/毕业数据/10dab含量.txt', header = T, sep = '\t', check.names=F)
library(ggplot2)
#library(ggThemeAssist)
#library(esquisse)
dab <- ggplot(data = alldab, mapping = aes(x = years, y = logcontent, colour = location)) + 
  geom_point() + #geom_tile(aes(fill=location)) + #scale_colour_brewer(alldab$location) + 
  geom_smooth(stat = 'smooth', method="loess", se = FALSE, formula = 'y ~x') +  #拟合区间关闭, method = lm这是直线)
  theme(panel.background = element_rect(colour = "black",linetype = "solid"),  #内框实心
          plot.background = element_rect(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) + #标题居中hjust和vjust
  labs(title = "Trend of 10-dab content in two regions with year") + 
  theme(plot.subtitle = element_text(colour = "gray34"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.background = element_rect(fill = NA))
dab
#ggThemeAssistGadget(dab)
#esquisse::esquisser()
#想把内框做成半框

