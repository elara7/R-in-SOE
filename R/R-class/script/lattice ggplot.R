xyplot(y ~ x | f * g, data, option)
library(datasets)
library(lattice)
## Convert 'Month' to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))
densityplot(~mpg | cyl, data=mtcars, main = "Density Plot by Number
            of Cylinders", xlab = "Miles per Gallon")
splom(mtcars[c(1, 3, 4, 5, 6)], main = "Scatter Plot Matrix for mtcars Data")
cloud(mpg ~ wt * qsec | cyl, data=mtcars, main = "3D Scatter Plots by Cylinders")
library(lattice)
mygraph<-densityplot(~mpg | cyl, data=mtcars,
                     main = "Density Plot by Number of Cylinders",
                     xlab = "Miles per Gallon")
mygraph
update(mygraph, col="red", pch=12, cex=0.8, lwd=3)

myshingle <- equal.count(x, number=#, overlap=proportion 
                                 )
library(lattice)
displacement <- equal.count(mtcars$disp, number=3, overlap=0)
xyplot(mpg~wt|displacement, data=mtcars,
       main = "Miles per Gallon vs. Weight by Engine Displacement",
       xlab = "Weight", ylab = "Miles per Gallon",
       layout=c(3, 1), aspect=1.5)


#ggplot
library("ggplot2")
qplot(displ,hwy,data=mpg)
qplot(displ,hwy,data=mpg,color=drv)#按照drv的数值对点分类标色
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))#添加拟合曲线
qplot(hwy, data = mpg, fill = drv)#直方图，用drv的数据分类来对柱子标色
qplot(displ, hwy, data = mpg, facets = . ~ drv)#按drv的值分类画3个散点图
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)#按照drv分组画3个直方图
library(car)
windows(7,7)
#对比，原版和qplot做分类散点
scatterplot(displ~hwy|drv,data=mpg)
qplot(displ,hwy,data=mpg,color=drv)
qplot(displ, hwy, data = mpg, color= drv, geom = c("point", "line"))#上一个命令加上连线
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))
qplot(trans, hwy, data = mpg, color= drv, geom = "boxplot")#drv分色箱线图
qplot(hwy, data = mpg, fill= drv, geom = "histogram")#直方图
qplot(displ, hwy, data = mpg, facets = . ~ drv)#drv分类散点（多图）
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)#drv分类直方图（多图）

qplot(clarity, data=diamonds, fill=cut, geom="bar")#cut分类标色直方图
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
p <- ggplot(data=mpg,aes(x=displ,y=hwy,colour=factor(cyl)))#画出坐标轴空图
p + geom_point() + geom_smooth()#画出图形（不同类型用不同颜色拟合线,因为colour在画坐标轴的时候就定下来了，成为全局设定）
p <- ggplot(mpg,aes(x=displ,y=hwy))
p + geom_point(aes(colour=factor(cyl))) + geom_smooth()#（拟合线用全部数据一起画，因为colour只在point里面生效，曲线部分没有规定）
p <- ggplot(mpg, aes(x=displ,y=hwy))
p + geom_point(aes(colour = cyl))#前面都用cyl的固定值（离散）作为分类，这个用cyl做连续变量画渐变
#改坐标轴
qplot(mpg, wt, data=mtcars, colour=factor(cyl), geom="point", xlab="Descr. of x-axis", ylab="Descr. of y-axis", main="Our Sample Plot")
#坐标轴字号
qplot(mpg, wt, data=mtcars, geom="point")
qplot(mpg, wt, data=mtcars, geom="point") + theme_bw()
qplot(mpg, wt, data=mtcars, geom="point") + theme_bw(18)






