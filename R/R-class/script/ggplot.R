require(ggplot2)
data(diamonds)
set.seed(42)
small <- diamonds[sample(nrow(diamonds),1000),]
head(small)

#全局参数
ggplot(data = small,mapping = aes(x=carat,y=price))

ggplot(data = small,mapping = aes(x=carat,y=price
                                  )) + geom_point()

ggplot(data = small,mapping = aes(x=carat,y=price,
                                  shape=cut
                                  )) +geom_point()

ggplot(data = small,mapping = aes(x=carat,y=price,
                                  shape=cut,
                                  colour=color
                                  )) +geom_point()


#局部设定，直方图，连续变量分区画柱子
ggplot(small) + geom_point(aes(x=carat,y = price, shape = cut, colour=color))

ggplot(small) + geom_histogram(aes(x=price))

ggplot(small) + geom_histogram(aes(x=price,fill=cut))#fill分组变量，频数图

ggplot(small) + geom_histogram(aes(x = price, fill=cut),position = "fill")#变成每一个price的各cut百分比图



#geom系列*********************************************************************************
#柱状图，离散变量分个画柱子
p <- ggplot(small)
p+geom_bar(aes(x=clarity))
p+geom_bar(aes(x=clarity,y=1000),stat = "identity")#改纵轴名字，并且原图纵坐标放大1000倍，必须哟stat

#密度
ggplot(small)+geom_density(aes(x=price,colour=cut))#cut分线和色的price密度曲线
ggplot(small)+geom_density(aes(x=price,fill=clarity))#用clarity分线并填充

#箱线图
ggplot(small)+geom_boxplot(aes(x=cut,y=price,fill=color))#用color分色

#其他几何绘图函数
p <- ggplot(small,mapping = aes(x=carat,y=price))
p+geom_point()+geom_abline(slope = 5000,intercept = 10)#加一条直线
p+geom_abline(slope = 1,intercept = 0)#过原点斜率为1

p+geom_blank()#空白图，坐标按照数据定好了

p+geom_dotplot(binwidth = 0.01)#点的大小
p+geom_errorbarh(xmin=0,xmax=1)#x在0-1之间的横向连线
ggplot(small)+geom_ribbon(aes(x=carat,y=price,colour=color),ymin=1,ymax=10000)#y在1-10000之间的横向连线
p+geom_hline(yintercept = 10)#画出y=10
ggplot(small)+geom_vline(aes(x=carat,y=price),xintercept = 1)#画出x=1
ggplot(small)+geom_line(aes(x=carat,y=price,colour=color))#按color分线的折线图


ggplot(small)+geom_step(aes(x=carat,y=price))#阶梯图

ggplot(small,aes(x=carat,y=price))+geom_bin2d()#按密度分块（马赛克图）
ggplot(small,aes(x=carat,y=price))+geom_crossbar(ymin=10,ymax=10000)#频谱线，长度10-10000
ggplot(small,aes(x=carat,y=price))+geom_density2d()#等密度线
ggplot(small,aes(x=carat))+geom_freqpoly()#频数折线图
ggplot(small,aes(x=carat,y=price))+geom_jitter()#带jitter的point
ggplot(small,aes(x=carat,y=price,colour=color))+geom_path()#路径图
ggplot(small,aes(x=carat,y=price))+geom_rect(xmin=0,xmax=3,ymin=0,ymax=10000)#涂灰一片
ggplot(small,aes(x=carat,y=price))+geom_rug()#横纵轴频谱图
ggplot(small,aes(x=carat,y=price))+geom_smooth()#拟合曲线以及置信区间
ggplot(small,aes(x=carat,y=price))+geom_text(aes(label=color))#用文字做点
#ggplot(small,aes(x=carat,y=price))+geom_spoke(angle=10,radius=5)#看不懂
#ggplot(small,aes(x=carat,y=price))+geom_violin()#看不懂
#ggplot(small)+geom_map(aes(x=carat,y=price))#好像会很厉害
#ggplot(small)+geom_polygon(aes(x=carat,y=price,colour=color))#看不懂
#ggplot(small)+geom_raster(aes(x=carat,y=price))#看不懂
#ggplot(small)+geom_segment(aes(x=depth,y=price),xend=70,yend=1000)#看不懂
#ggplot(small)+geom_tile(aes(x=carat,y=price))#看不懂
#ggplot(small)+geom_area(aes(x=carat,y=price,colour=color))#看不懂
#ggplot(small,aes(x=carat,y=price))+geom_errorbar(ymin=0,ymax=10000)#看不懂
#ggplot(small,aes(x=carat,y=price))+geom_linerange(ymin=10,ymax=1000)#看不懂
#ggplot(small,aes(x=carat,y=price))+geom_pointrange(ymin=10,ymax=1000)#看不懂
#ggplot(small,aes(x=carat,y=price,))+geom_quantile()#看不懂


#******************************************************************************************

#scale系列**********************************************************************************
p <- ggplot(small)+geom_point(aes(x=carat,y=price,shape=cut,colour=color))
p
p+scale_y_log10()#(出图后改纵坐标对数化

p+scale_y_log10()+scale_colour_manual(values=rainbow(7))#再把颜色改成彩虹色


#stat系列***************************************************************************
p <- ggplot(small,aes(x=carat,y=price))+geom_point()+scale_y_log10()
p
p+stat_smooth()#出图后加拟合线与置信区间
p+stat_contour()#看不懂
p+stat_identity(position = )#看不懂
p+stat_summary()#h好像没啥用
q <- ggplot(small,aes(x=carat))
q+stat_bin()#单变量自动直方图，离散直方图连续频率曲线
p+stat_bin2d()#多变量自动密度块图，每个色块代表一种数量
p+stat_boxplot(aes(group=color))#加上箱线图
q+stat_density()#加入密度曲线（有填充）
p+stat_ecdf()#加入cdf
q+stat_qq()#不会用
p+stat_summary2d()#不会用
p+stat_smooth()#添加拟合曲线
p+stat_sum(aes(colour=color))#就近合并，按大小表示点中包含个数
p+stat_unique()#去掉极端值
p+stat_ydensity()#画y的密度

#coord系列（坐标控制）*****************************************************************
ggplot(small)+geom_bar(aes(x=cut,fill=cut))+coord_flip()#转置
ggplot(small)+geom_bar(aes(x=factor(1),fill=cut))#按照cut分factor（1）的比例
ggplot(small)+geom_bar(aes(x=factor(1),fill=cut))+coord_polar(theta="y")#转化为极坐标图（饼图）
ggplot(small)+geom_bar(aes(x=factor(1),fill=cut))+coord_polar()#靶心图
ggplot(small)+geom_bar(aes(x=clarity,fill=cut))#按照cut分类填充的clarity频数图
ggplot(small)+geom_bar(aes(x=clarity,fill=cut))+coord_polar()#风玫瑰图


#facet系列（分面）*********************************************************************
p <- ggplot(small,aes(x=carat,y=price))+geom_point(aes(colour=cut))+scale_y_log10()
p+facet_grid(~cut)#按照不同cut分别画出5个图，横着排
p+facet_wrap(~cut)+stat_smooth()#分别在5个图画拟合
p+stat_smooth()+facet_wrap(~cut)#效果一样

#主题**********************************************************************
theme_bw()
p <- ggplot(small,aes(x=carat,y=price))+geom_point(aes(colour=cut))+scale_y_log10()+stat_smooth()+facet_wrap(~cut)#分别在5个图画拟合
p+theme_bw()#白色主题
install.packages("ggthemes")
library(ggthemes)
p+theme_wsj()
p+theme_few()
p+theme_foundation()
p+theme_map()
p+theme_hc()
p+theme_par()
p+theme_dark()
p+theme_calc()
p+theme_base()
p+theme_void()
p+theme_tufte()
p+theme_economist()
p+theme_economist_white()
p+theme_fivethirtyeight()
p+theme_gdocs()
p+theme_gray()
p+theme_bw()
p+theme_solid()
p+theme_stata()
p+theme_solarized()
p+theme_light()
p+theme_linedraw()

#二维密度图
ggplot(diamonds,aes(carat,price))+stat_density2d(aes(fill=..level..),geom="polygon")+scale_fill_continuous(high="darkred",low="darkgreen")


#蝴蝶图
theta <- data.frame(x=radius*sin(theta), y=radius*cos(theta))
ggplot(dd, aes(x, y))+geom_path()+theme_null()+xlab("")+ylab("")


