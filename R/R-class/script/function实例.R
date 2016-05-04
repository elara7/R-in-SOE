#debug(函数名字)
#运行函数（带上输入）
#在Browse后面回车来逐步运行
#最后undebug（）
#或者用debugonce()

#eg1
"%g%" <- function(x,y) { #“”里面是function的名字
        print(x+y) 
        print(x-y) 
        print(x*y) 
        print(x/y) } #{}内是程序体
3%g%5

#eg2计算每一列的平均（有NA得出NA）
columnmean <- function(y){ 
        nc <- ncol(y) 
        #ncol取y的列数入nc
        means <- numeric(nc)
        #建立means为数值型变量，数值带入nc的
        for(i in 1:nc){
                #对1到nc列循环
                means[i] <- mean(y[,i]) 
                #means的第i项存入y的第i列的均值
                } 
        means 
        #输出最后结果
        } 
columnmean(airquality)
#用airquality实验

#eg3计算每一列的平均（有NA消除NA）
columnmean <- function(y,removeNA=TRUE){ 
        ######输入的时候可以写（y，removeNA=FALSE，改变处理方式）
        nc <- ncol(y) 
        means <- numeric(nc) 
        for(i in 1:nc){
                means[i] <- mean(y[,i], na.rm = removeNA)
                #把输入的removeNA的值带入mean函数的参数na.rm中，
                #如果是true则去掉na，flase的话不去掉na
        } 
        means
        } 
columnmean(airquality,removeNA = FALSE)
        
##########return#########

x <- c(1,9,2,8,3,7) 
y <- c(9,2,8,3,7,2) 
pmax(x,y)
#求2组数对应位置最大值的中位数
parmax <- function (a,b){ 
        c <- pmax(a,b) 
        median(c) 
} 
parmax(x,y)
#求2组数对应位置最大值和最小值的中位数
parboth <- function (a,b) { 
        c <- pmax(a,b) 
        d <- pmin(a,b) 
        answer <- list(median(c),median(d)) 
        names(answer)[[1]] <- "median of the parallel maxima" 
        names(answer)[[2]] <- "median of the parallel minima" 
        return(answer) 
} 
parboth(x,y)

#函数和系统自带函数名重复是可以的。用search（）在前面的是优先级高的
#全局（新写的）>包


#T检验
twosam <- function(y1, y2){ 
        n1 <- length(y1); n2 <- length(y2) 
        yb1 <- mean(y1); yb2 <- mean(y2) 
        s1 <- var(y1); s2 <- var(y2) 
        s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2) 
        tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2)) 
        tst 
        } 
A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02) 
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97) 
twosam(A,B)



