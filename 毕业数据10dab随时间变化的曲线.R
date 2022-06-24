#R 4.0
#读入数据，防止纯数字标题被加X，可以用check.names = F解决（第一版数据这样了，后来数据改了布局）
alldab <- read.table('/Users/yanjiacheng/Desktop/毕业数据/10dab含量.txt', header = T, sep = '\t', check.names=F)
library(ggplot2)
#library(ggThemeAssist)
#library(esquisse)
dab <- ggplot(data = alldab, mapping = aes(x = years, y = logcontent, colour = location)) + 
  geom_point(aes(shape=location),size = 3) + #geom_tile(aes(fill=location)) + #scale_colour_brewer(alldab$location) + 
  geom_smooth(stat = 'smooth', method="loess", se = FALSE, formula = 'y ~x') +  #拟合区间关闭, method = lm这是直线)
  theme(panel.background = element_rect(colour = "black",linetype = "solid"))+ #,  #内框实心
          #plot.background = element_rect(colour = "black"), #外框出现
        #plot.title = element_text(hjust = 0.5)) + #标题居中hjust和vjust
  #labs(title = "Trend of 10-dab content in two regions with year") + #该期刊子图不要标题
  theme(plot.subtitle = element_text(colour = "gray34"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.background = element_rect(fill = NA))
dab + plot_annotation(tag_levels = 'b') & #报错可换labs(tag = "b")
  theme(plot.tag = element_text(size = 20))
#尝试美化
ggThemeAssistGadget(dab)
dab + theme(axis.line = element_line(colour = "gray20"),
    axis.ticks = element_line(size = 1),
    panel.grid.major = element_line(size = 1,linetype = "blank"), 
    panel.grid.minor = element_line(size = 1,linetype = "blank"), 
    axis.title = element_text(size = 20),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    panel.background = element_rect(size = 0.5),#size=0，右半内框消失，很丑
    plot.background = element_rect(size = 1, colour = 'white'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 25,
                              hjust = -0.09, vjust = 0.27)) +
  labs(fill = 12, size = 17) +
  labs(title = "b")
ggsave('趋势预测.pdf',dpi = 600, width = 7, height = 3)
#esquisse::esquisser()
#想把内框做成半框？先做sci的事
p.tre = ggplot(data = alldab, mapping = aes(x = years, y = logcontent, colour = location)) + 
  geom_point() + 
  geom_smooth(stat = 'smooth', method="loess", se = FALSE, formula = 'y ~x') +  #拟合区间关闭, method = lm这是直线)
  theme(panel.background = element_rect(colour = "black",linetype = "solid"),  #内框实心
        plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(colour = "gray34"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = "white", colour = "white", size = 8), 
        legend.background = element_rect(fill = NA))  +
  labs(tag = "b") 
