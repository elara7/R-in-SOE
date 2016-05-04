#************************************************************
#茎叶图
x<-c(25, 45, 50, 54, 55, 61, 64, 68, 72, 75, 75,
     78, 79, 81, 83, 84, 84, 84, 85, 86, 86, 86,
     87, 89, 89, 89, 90, 91, 91, 92, 100)
stem(x)
##
## The decimal point is 1 digit(s) to the right of the |
##
## 2 | 5
## 3 |
## 4 | 5
## 5 | 045
## 6 | 148
## 7 | 25589
## 8 | 1344456667999
## 9 | 0112
## 10 | 0

stem(x,scale=2)#10分成2部分，0-4和5-9
##
## The decimal point is 1 digit(s) to the right of the |
##
## 2 | 5
## 3 |
## 3 |
## 4 |
## 4 | 5
## 5 | 04
## 5 | 5
## 6 | 14
## 6 | 8
## 7 | 2
## 7 | 5589
## 8 | 13444
## 8 | 56667999
## 9 | 0112
## 9 |
## 10 | 0

stem(x,scale=.5)#10分成0.5部分，相当于20为一组
##
## The decimal point is 1 digit(s) to the right of the |
##
## 2 | 5
## 4 | 5045
## 6 | 14825589
## 8 | 13444566679990112
## 10 | 0

#***************************************************
library(vcd)
counts <- table(Arthritis$Improved)
counts
#一维柱状图（横竖）
par(mfrow = c(1, 2))
barplot(counts, main = "Simple Bar Plot", xlab = "Improvement", ylab = "Frequency")
barplot(counts, main = "Horizontal Bar Plot", xlab = "Frequency",
        ylab = "Improvement", horiz = TRUE,
        names.arg=c("A","B","C"))
#
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
#分组分颜色累积柱状图，beside决定是堆叠还是并排
#opar<-par(no.readonly = TRUE)
par(mfrow = c(1, 2))
barplot(counts, main="Stacked Bar Plot", xlab="Treatment",
        ylab="Frequency", col=c("red", "yellow","green"), legend=rownames(counts))
barplot(counts, main="Grouped Bar Plot", xlab="Treatment",
        ylab="Frequency", col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)

#
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by = list(state.region),
                   FUN = mean)
means
## Group.1 x
## 1 Northeast 1.000000
## 2 South 1.737500
## 3 North Central 0.700000
## 4 West 1.023077
means <- means[order(means$x), ]
means
## Group.1 x
## 3 North Central 0.700000
## 1 Northeast 1.000000
## 4 West 1.023077
## 2 South 1.737500
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

#***************************************************
#饼图
par(mfrow = c(2, 2))
#1平面饼图
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main = "Simple Pie Chart")
#2平面饼图带百分比
pct <- round(slices/sum(slices) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls)),
    main = "Pie Chart with Percentages")
#3 3d饼图
library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, main = "3D Pie Chart ")
#4 饼图带频数
mytable <- table(state.region)
lbls <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls, main = "Pie Chart from a Table\n (with sample sizes)")

#***************************************************
#画密度函数
par(mfrow = c(2, 1))
d <- density(mtcars$mpg)
windows(7,7)
plot(d)
d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon")
polygon(d, col = "red", border = "blue")#填充红色，轮廓蓝色
rug(mtcars$mpg, col = "brown")#加须
#
par(lwd = 2)
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4, 6, 8), labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
title(main = "MPG Distribution by Car Cylinders")
colfill <- c(2:(1 + length(levels(cyl.f))))
cat("Use mouse to place legend...", "\n\n")
legend(locator(1), levels(cyl.f), fill = colfill)#鼠标指定放图例
detach(mtcars)
par(lwd = 1)


#*****************************************************
#直方图
windows(7,7)
par(mfrow = c(1, 2))
hist(mtcars$mpg, breaks = 7, col = "yellow", xlab = "Miles Per Gallon",
     main = "Colored histogram with 12 bins")
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", xlab = "Miles Per Gallon",
     main = "Histogram, rug plot, density curve")
rug(mtcars$mpg)
lines(density(mtcars$mpg), col = "blue", lwd = 2)
box()#把图围起来
library(shiny)#可视化编辑，左上角+号里面的shiny app

#***************************************************
boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallon")

boxplot.stats(mtcars$mpg)
## $stats
## [1] 10.40 15.35 19.20 22.80 33.90
##
## $n
## [1] 32
##
## $conf
## [1] 17.11916 21.28084
##
## $out
## numeric(0)
par(mfrow = c(1, 2))
boxplot(mpg ~ cyl, data = mtcars, main = "Car Milage Data",
        xlab = "Number of Cylinders", ylab = "Miles Per Gallon")
mtcars$cyl.f <- factor(mtcars$cyl, levels = c(4, 6, 8),
                       labels = c("4", "6", "8"))
mtcars$am.f <- factor(mtcars$am, levels = c(0, 1),
                      labels = c("auto", "standard"))
#分类箱线图并上色
boxplot(mpg ~ am.f * cyl.f, data = mtcars, varwidth = TRUE, col = c("gold", "darkgreen"),
        main = "MPG Distribution by Auto Type", xlab = "Auto Type")

#*****************************************************
#windows(width = 7, height = 4)
par(mfrow = c(1, 2))
windows(7,7)
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = 0.7,
         main = "Gas Milage for Car Models", xlab = "Miles Per Gallon")
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, labels = row.names(x), cex = 0.7, pch = 19,
         groups = x$cyl, gcolor = "black", color = x$color,
         main = "Gas Milage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")#groups按照cyl分组

#******************************************************
windows(7,7)
attach(mtcars)
#点的大小表示值大小
symbols(wt, mpg, circle=disp)

r <- sqrt(disp/pi)#缩小圈圈之间的大小差距（权重）
symbols(wt, mpg, circle=r, inches=0.30,
        fg="white", bg="lightblue",#加入填充色
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon", xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)
