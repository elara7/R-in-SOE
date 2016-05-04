#Monte Carlo integration*****************************
#Calculate y=exp(-x)0-1
m <- 10000
#1随机打点法，f（x）下方的部分计数
k<-0
for (i in 1:m){
        x<-runif(1)
        y<-runif(1)
        ifelse(y<=exp(-x),k<-k+1,k<-k)
}
J<-k/m;J
## [1] 0.631
#2平均值法，求全部f（x）值的平均数
x <- runif(m)
theta.hat <- mean(exp(-x))
print(theta.hat)
## [1] 0.634
#TURE用cdf计算的真实值
print(1 - exp(-1))
## [1] 0.632
#********************************************
#1
m<-10000;k<-0
for (i in 1:m){
x<-runif(1)
y<-runif(1)
ifelse(y<=exp(((-x^2)/2))/sqrt(2*pi),k<-k+1,k<-k)
}
J<-k/m;J
## [1] 0.343
#2
x <- runif(m)
theta.hat <- mean(exp(((-x^2)/2))/sqrt(2*pi))
print(theta.hat)
## [1] 0.341
#TURE
#0.341344
#******************************************************

#bootstrap
library(bootstrap) #for the law data
head(law)

print(cor(law$LSAT, law$GPA))

#set up the bootstrap
B <- 200 #number of replicates
n <- nrow(law) #sample size
R <- numeric(B) #storage for replicates
#bootstrap estimate of standard error of R
for (b in 1:B) {
#randomly select the indices随机出i，再从样本中重新随机取样
#每一次循环都抽取了n个出来形成一个完整重复样本
i <- sample(1:n, size = n, replace = TRUE)
LSAT <- law$LSAT[i] #i is a vector of indices
GPA <- law$GPA[i]
R[b] <- cor(LSAT, GPA)#第b次抽出的样本（n个元素）的相关系数
}
#output用200次样本的R画直方图
hist(R, prob = TRUE)

