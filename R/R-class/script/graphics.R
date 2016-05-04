#画散点图并作出回归直线
attach(mtcars)
## The following object is masked from package:ggplot2:
##
## mpg
plot(wt, mpg) ## 散点图
abline(lm(mpg ~ wt))#拟合直线
title("Regression of MPG on Weight") ## 标题
detach(mtcars)
#将图保存到pdf中
pdf("mygraph.pdf")#保存在工作文件夹
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()#退出pdf编辑
#保存成png
attach(mtcars)
plot(wt, mpg) ## Make plot appear on screen device
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight") ## Annotate with a title
dev.copy(png, file = "testpng.png") ## Copy my plot to a PNG file
dev.off() ## Don't forget to close the PNG device!
detach(mtcars)


#散点图
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b")
#par()查询和修改图形参数
par()
opar <- par(no.readonly=TRUE)#保存原始参数
par(lty=2, pch=17)#改成三角形x虚线
plot(dose, drugA, type="b")
par(opar)#回复原始参数
plot(dose, drugA, type="b")
#或者
plot(dose, drugA, type="b", lty=2, pch=17)
plot(dose, drugA, type="b", lty=3, lwd=3, pch=15, cex=2)
#看plot范例和参数对应图形
windows(6,6)
example(points)#pageUP和pageDOWN换页
#颜色
n <- 10
mycolors <- rainbow(n)
#画饼图（彩色）
pie(rep(1, n), labels=mycolors, col=mycolors)
mygrays <- gray(0:n/n)
#黑白
pie(rep(1, n), labels=mygrays, col=mygrays)
