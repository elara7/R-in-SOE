#cor(x, use= , method= )
cor(1:10, 2:11)
states<- state.x77[,1:5]
cov(states)#协方差阵
cor(states)#相关系数矩阵
cor(states, method="spearman")#用spearman方法
x <- states[,c("Population", "Income", "Illiteracy")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)#得到的不是方阵

library(psych)
corr.test(states, use="complete")#相关性检验

library(ggm)
pcor(c(1,5,2,3,4), cov(states))
#1和5之间的相关度，控制变量为2 3 4

#MSG只是数据
library("MSG")
data("BinormCircle")
par(mfrow = c(1, 2), pch = 20, ann = FALSE, mar = c(2, 2, 0.5, 0.2))
plot(BinormCircle, col = rgb(1, 0, 0))
plot(BinormCircle, col = rgb(1, 0, 0, alpha = 0.01))
attach(BinormCircle)
cor(V1,V2)
detach(BinormCircle)


#两次随机不一样
sample(1:10, 4)
## [1] 3 4 5 7
set.seed(1)
#但是如果每次sample之前都setseed同一个值那么sample的是一样的
sample(1:10, 4)
## [1] 3 9 8 5
sample(letters, 5)
## [1] "q" "b" "e" "x" "p"
sample(1:10) ## permutation默认抽n个，相当于重排
## [1] 4 7 10 6 9 2 8 3 1 5
sample(1:10)
## [1] 2 3 4 1 9 5 10 8 6 7
sample(1:10, replace = TRUE) ## Sample w/replacement有放回
## [1] 2 9 7 8 2 8 5 9 7 8

windows(5,5)

par(mfrow=c(1,2))#画1x2的图
x <- pretty(c(-3,3), 60)#60个-3到3的随机数
y <- dnorm(x)#产生正态密度
z <- pnorm(x)#产生正态分布
plot(x, y, type = "l", xlab = "Normal Deviate", ylab = "Density", yaxs = "i" )
plot(x, z, type = "l", xlab = "Normal Deviate", ylab = "Distribution", yaxs = "i" )
pnorm(1.96)#1。96的累计分布概率
qnorm(.95)#0.95分位点
qnorm(.9, mean=500, sd=100)#指定均值和方差
rnorm(50, mean=50, sd=10)

#产生线性模型
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -6.41 -1.54 0.68 0.69 2.93 6.51
plot(x, y)

set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -3.49 -0.14 1.58 1.43 2.84 6.94
plot(x, y)

y <- rnorm(5000)
shapiro.test(y)

#qq图
qqnorm(y)
qqline(y,lty=2)

#buffon‘s needle problem浦丰投针问题
#现成动画
install.packages(animation)
buffon.needle(namx=500, interval=0)
#普通计算
buffon<-function(n, l=0.8, a=1){
k<-0
theta<-runif(n,0, pi); x<-runif(n,0, a/2)
for (i in 1:n){
if (x[i]<= l/2*sin(theta[i]))
k<-k+1
}
2*l*n/(k*a)
}
buffon(10000)

#圆占方的比例求pi
MC1 <- function(n){
k <- 0; x <- runif(n); y <- runif(n)
for (i in 1:n){
if (x[i]^2+y[i]^2 < 1)
k <- k+1
}
4*k/n
}
MC1(10000)

#通过中心极限定理用均匀分布（12个）生成正太随机数（1个）
MCN <- function(k){
y <- numeric()
for (i in 1:k){
x <- runif(n=12,min=0,max=1)
y[i]<-sum(x)-6
}
hist(y)
}
MCN(10000)

###MCMC蒙特塔罗实验
beta0=1:1000
beta1=1:1000
#设定真实值
b1=20;b2=0.6;n=25
x=10:34
#由真实值生成数据
for(i in 1:1000){
#构建模型
u=rnorm(25)
y=b1+b2*x+u
#每一个模型都跑一次lm，把系数提取出来
lm.m=lm(y~x)
beta0[i]=as.numeric(coef(lm.m)[1])
beta1[i]=as.numeric(coef(lm.m)[2])
}
#beta0
#beta1
#1000次模型把系数平均
mean(beta0)
## [1] 20
mean(beta1)
## [1] 0.6
