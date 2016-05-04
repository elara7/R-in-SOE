#apply####################################
#用apply求矩阵行列和,apply只对矩阵有用。向量不行
x <- matrix(1:24,nrow=4) 
x
#把x的每一个第i维度(行)带入fun中，全部返回到y，如行求和
#y <- apply(x, i, fun) 
rowSums = apply(x, 1, sum) 
rowMeans = apply(x, 1, mean) 
colSums = apply(x, 2, sum) 
colMeans = apply(x, 2, mean)
x <- matrix(rnorm(200), 20, 10)
apply(x, 1, quantile, probs = c(0.25, 0.75))
a <- array(rnorm(2 * 2 * 10), c(2, 2, 10)) 
apply(a, c(1, 2), mean)
rowMeans(a, dims = 2)

#mapply######################################
#mapply(fun（x，y），x的取值范围，y的取值范围)。xy会匹配使用
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
mapply(rep, 1:4, 4:1)
noise <- function(n, mean, sd) rnorm(n, mean, sd) 
noise(5,1,2)#生成一个随机数n是分位数
noise(1:5,1:5,2)#观察数为数字n或者n向量长度，均值范围，方差范围

mapply(noise, 1:5, 1:5, 2)#（1，1，2）（2，2，2）。。。。相当于下一行的
list(noise(1, 1, 2), noise(2, 2, 2), noise(3, 3, 2), noise(4, 4, 2), noise(5, 5, 2))

#lapply######################################
#1
#形成一个list，其中的a=1，2，3，4，5，b=10个正太随机
x <- list(a = 1:5, b = rnorm(10)) 
#对list的每一个item使用fun.apply不能对list元素起作用
lapply(x, mean)

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5)) 
lapply(x, mean)
#2生成1，2，3，4个0-1的均匀分布，对向量起作用，apply不能用
x <- 1:4 
lapply(x, runif)
#生成1，2，3，4个0-10均匀分布
lapply(x, runif, min = 0, max = 10)

#3
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2)) 
x
#取出2个item中的第一列
lapply(x, function(elt) elt[,1])

head(flags)
#返回flags每一列的class，返回list格式
cls_list <- lapply(flags, class)
as.character(cls_list)
#返回向量
cls_vect <- sapply(flags, class)
sum(flags$orange)
#取出关于颜色的列
flag_colors <- flags[, 11:17]
#颜色里面有这种颜色就是1没有就是0，求和得到每种颜色国家数量
lapply(flag_colors, sum)
sapply(flag_colors, sum)
#占比
sapply(flag_colors, mean)

#取出形状列，里面的数据是每种形状在某国旗中出现的次数
flag_shapes <- flags[, 19:23]
#求出每种形状在不同国旗中出现次数的范围
lshape <- lapply(flag_shapes, range)
mshape <- sapply(flag_shapes, range)
class(lshape)
class(mshape)
dim(mshape)
#合并重复内容
unique(c(3, 4, 5, 5, 5, 6, 6))
#把flags里面各个name项目下的内容取出重复，长短不一返回list
unique_vals <- lapply(flags, unique)
unique_vals <- sapply(flags, unique)
#求出去重以后数据里面各个项目的长度
sapply(unique_vals, length)

#出错
vapply(flags, unique, numeric(1))
#限定返回的是长度1的字符串向量
cflags<-vapply(flags, class, character(1))

#生成随机数
x <- c(rnorm(10), runif(10), rnorm(10, 1))
#生成3个level，分别重复10次
f <- gl(3, 10)
#把x按照f的标注的分类分别求mean
tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
#把x按照f的标注的分类分别求range
tapply(x, f, range)

table(flags$landmass)
table(flags$animate)
tapply(flags$animate, flags$landmass, mean)

#把x按照f因子分离，返回list
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
split(x, f)
#求分离后项目的均值
lapply(split(x, f), mean)

#eg
library(datasets)
head(airquality)
#按照month分组
s <- split(airquality, airquality$Month)
#对分离后的项目分别求要求项目的列均值
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))

#2种因子分类，求2个因子的交叉项返回1.1，1.2等
x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
interaction(f1, f2)
#按2个因子分类并返回数据信息
str(split(x, list(f1, f2)))
#删除空行
str(split(x, list(f1, f2), drop = TRUE))