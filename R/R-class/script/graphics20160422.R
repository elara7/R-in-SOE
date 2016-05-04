dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b")
title("An Example")#添加标题
par()
opar <- par(no.readonly=TRUE)#保存当前图线型点型设置
par(lty=2, pch=17)#设置新图设定
plot(dose, drugA, type="b")
par(opar)#载入旧图设定
plot(dose, drugA, type="b")
example(points)


names(pdfFonts())#列出所有字体名不一定都可以用
n <- 10
mycolors <- rainbow(n)#彩虹色
pie(rep(1, n), labels=mycolors, col=mycolors)#饼图
mygrays <- gray(0:n/n)#灰度图
pie(rep(1, n), labels=mygrays, col=mygrays)

par(pin=c(4,3), mai=c(1,.5, 1, .2))#设定边界4宽，3高，1下边距，1上边距，0.5左边距0.2右边距

plot(dose, drugA, type="b",
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A",#主标题顶部
     sub="This is hypothetical data",#副标题在底部
     xlab="Dosage", ylab="Drug Response",#xy轴标签
     xlim=c(0, 60), ylim=c(0, 70))#调整xy轴取值范围

plot(dose, drugA,
     title(main="My Title", #主标题颜色
           col.main="red",#横（主）标题颜色
           sub="My Sub-title", col.sub="blue",#副标题与颜色
           xlab="My X label", ylab="My Y label",#标签颜色
           col.lab="green", cex.lab=0.75), ann=FALSE)
           #标签颜色0.75，字体缩小25%，ann为T自动加标题

x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 8) + 0.1)
plot(x, y, type = "b", pch = 21, col = "red", yaxt = "n",
     lty = 3, ann = FALSE)#plot高水平绘图函数画出新图
#低水平绘图函数直接在原图加
lines(x, z, type = "b", pch = 22, col = "blue", lty = 2)#加一条线
axis(2, at = x, labels = x, col.axis = "red", las = 2)#加左纵轴数字
axis(4, at = z, labels = round(z, digits = 2), col.axis = "blue",
     las = 2, cex.axis = 0.7, tck = -0.01)#加右纵轴数字
mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")#右纵轴label
title("An Example of Creative Axes", xlab = "X values", ylab = "Y=X")#标题
abline(v=seq(1, 10, 2), lty=2, col="blue")#加辅助线
par(opar)

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red",
     ylim = c(0, 60), main = "Drug A vs. Drug B",
     xlab = "Drug Dosage", ylab = "Drug Response")
lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
abline(h = c(30), lwd = 1.5, lty = 2, col = "grey")
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)#轴加上小刻度，nx，x轴中一个大刻度里面有3个小刻度，ny同理，ratio刻度线长度比
legend("topleft", inset = 0.05, title = "Drug Type", c("A", "B"),
       lty = c(1, 2), pch = c(15, 17), col = c("red", "blue"))#加图例""
par(opar)

attach(mtcars)
plot(wt, mpg, main = "Milage vs. Car Weight", xlab = "Weight", ylab = "Mileage",
     pch = 18, col = "blue")#画出散点
text(wt, mpg, row.names(mtcars), cex = 0.6, pos = 4, col = "red")#在点上面加入文字标注cex字体大小pos位置
detach(mtcars)

opar <- par(no.readonly=TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")#在3，3生成文字
text(4,4,family="mono","Example of mono-spaced text")#family字体
text(5,5,family="serif","Example of serif text")
par(opar)

#用letax公式
plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))", cex = .8)

attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)

#自定义多个图在同一个图中的位置
windows(10,10)
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), widths=c(3, 1), heights=c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
opar <- par(no.readonly = TRUE)
par(fig = c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, xlab = "Miles Per Gallon", ylab = "Car Weight")
par(fig = c(0, 0.8, 0.5, 1), new = TRUE)#new是f的话不会出新图会在旧图改
boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)
par(fig = c(0.6, 1, 0, 0.8), new = TRUE)
boxplot(mtcars$mpg, axes = FALSE)
mtext("Enhanced Scatterplot", side = 3, outer = TRUE, line = -5)

library(car)
scatterplot(mpg ~ wt | cyl, data=mtcars, lwd=2,
            main="Scatter Plot of MPG vs. Weight by # Cylinders",
            xlab="Weight of Car (lbs/1000)",
            ylab="Miles Per Gallon",
            legend.plot=TRUE,
            id.method="identify",
            labels=row.names(mtcars),
            boxplots="xy"
)

set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
#普通图
with(mydata, plot(x, y, pch=19, main="Scatter Plot with 10,000 Observations"))
#透明度和集中度相关
with(mydata, smoothScatter(x, y, main="Scatterplot Colored by Smoothed Densities"))

#完整散点矩阵图
pairs(~mpg+disp+drat+wt, data=mtcars, main="Basic Scatter Plot Matrix")
#去掉对称的一半
pairs(~mpg+disp+drat+wt, data=mtcars, main="Basic Scatter Plot Matrix",upper.panel=NULL)

#带连线的变量相关矩阵图
library(car)
scatterplotMatrix(~ mpg + disp + drat + wt, data=mtcars,
                  spread=FALSE, lty.smooth=2, main="Scatter Plot Matrix via car Package")
scatterplotMatrix(~ mpg + disp + drat + wt | cyl,
                  data=mtcars, spread=FALSE, diagonal="histogram", main="Scatter Plot Matrix via car Package")

#3D
library(scatterplot3d)
attach(mtcars)
#普通散点
scatterplot3d(wt, disp, mpg, main="Basic 3D Scatter Plot")
#带点到地面连线和颜色
scatterplot3d(wt, disp, mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatter Plot with Vertical Lines")
#第一句画图，第二句回归，第三句把回归结果的回归平面加到图中
s3d <-scatterplot3d(wt, disp, mpg, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatter Plot with Vertical Lines and Regression Plane")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)
detach(mtcars)

#动图
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=5)
detach(mtcars)

library(Rcmdr)
scatter3d(wt, disp, mpg)

#
