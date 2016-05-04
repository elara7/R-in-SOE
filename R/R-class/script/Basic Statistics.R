#求算术平均数
arithmetic.mean <- function(x) sum(x)/length(x)
x<-c(1:100)
arithmetic.mean(x)
## [1] 50.5
mean(x)
## [1] 50.5
salary=c(2000,2100,2200,2300,2350,2450,2500,2700,2900,2850,3500,3800,2600,
         3000,3300,3200,4000,3100,4200)
mean(salary)
## [1] 2897.368

#trim去掉头尾的20%数据再求平均
mean(salary,trim=0.2)
## [1] 2826.923
mean(salary,trim=0.5)
## [1] 2850
#trim去掉的是前后各20%
x <- c(1,1,1,1,1,1,1,10,10,10)
mean(x,trim=0.2)

#ceiling取整，sort(x)[位置]取出sort以后的x里面某个位置的值
sort(x)[ceiling(length(x)/2)]
## [1] 50

#求中位数
med <- function(x) {
        #判断奇数偶数
        odd.even <- length(x)%%2
        if (odd.even == 0) (sort(x)[length(x)/2]+sort(x)[1+ length(x)/2])/2
        else
                sort(x)[ceiling(length(x)/2)]
}
med(x)
## [1] 50.5

#内置求中位数
median(x)
## [1] 50.5

med <- function(x) ifelse(length(x)%%2==1, sort(x)[ceiling(length(x)/2)],
                          (sort(x)[length(x)/2]+sort(x)[1+ length(x)/2])/2 )
salary=c(2000,2100,2200,2300,2350,2450,2500,2700,2900,2850,3500,3800,2600,
         3000,3300,3200,4000,3100,4200)
mean(salary)
## [1] 2897.368
med(salary)
## [1] 2850
salary=c(2000,2100,2200,2300,2350,2450,2500,2700,2900,2850,3500,3800,2600,
         3000,3300,3200,4000,3100,15000)
mean(salary)
## [1] 3465.789
med(salary)
## [1] 2850

#调和平均（考）
harmonic <- function (x) 1/mean(1/x)
harmonic(c(1,2,4,1))

#方差
y <- c(13,7,5,12,9,15,6,11,9,7,12)
variance <- function(x) sum((x - mean(x))^2)/(length(x)-1)
variance(y)
## [1] 10.25455
#内置方差
var(y)
## [1] 10.25455

#统一描述统计
data_outline <- function(x){
        n <- length(x)
        m <- mean(x)
        v <- var(x)
        s <- sd(x)
        me <- median(x)
        cv <- 100*s/m
        g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
        g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4
               -(3*(n-1)^2)/((n-2)*(n-3)))
        data.frame(N=n,Mean=m,var=v,std_dev=s,Median=me,
                   CV=cv,Skewness=g1,Kurtosis=g2,row.names=1)
}

#package的描述统计
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])
summary(mtcars[vars])

library(Hmisc)
describe(mtcars[vars])

library(pastecs)
stat.desc(mtcars[vars])

#加载包后被覆盖的命令，要使用的话用 包名::命令
#要看现在的命令是哪个包的用  ？命令
#1.3分组Descriptive statistics by group
#按照am分组求后面各项的均值和sd
aggregate(mtcars[vars], by = list(am = mtcars$am), mean)

aggregate(mtcars[vars], by = list(am = mtcars$am), sd)
#
library(psych)
describeBy(mtcars[vars], mtcars$am)

#自定义处理
mystats <- function(x, na.omit = FALSE) {
        #根据FLASE判断是否去掉缺失值
        if (na.omit) x <- x[!is.na(x)]
        m <- mean(x)
        n <- length(x)
        s <- sd(x)
        skew <- sum((x - m)^3/s^3)/n
        kurt <- sum((x - m)^4/s^4)/n - 3
        #return只能返回一个东西
        return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
library(reshape)
#融化数据，把数据按照数字一个一个分开
dfm <- melt(mtcars, measure.vars = c("mpg", "hp", "wt"), id.vars = c("am", "cyl"))
#按照~前面的作为列名，~后面的作为返回值，可以自定义（改return）
cast(dfm, am + cyl + variable ~ ., mystats)
#类似的用法
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)

#1.5Describe the data using a Graph
#开一个5x5绘图窗口
windows(5,5)
COM <- function (x) {
        par(mfrow=c(2,2)) # Combine four graph，4个图
        hist(x) # histogram
        dotchart(x) # dotchart点图
        boxplot(x,horizontal=T) # boxplot 箱线图
        qqnorm(x);qqline(x) # normal QQ图，点都集中在对角线上则服从正态分布
        par(mfrow=c(1,1)) # reset the enviroment回复普通模式
}
x <- rnorm(100)
COM(x)
