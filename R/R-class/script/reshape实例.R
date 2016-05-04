ID<-c(1,1,2,2) 
Time<-c(1,2,1,2) 
X1<-c(5,3,6,2) 
X2<-c(6,5,1,4)
mydata<-data.frame(ID,Time,X1,X2)
mydata
library(reshape)
md <- melt(mydata, id=(c("ID", "Time"))) 
#melt(要融合数据集，id=“要整合的项目名字”)，
#把所有维度都展开，按id和time展开，第一行代表ID1在time1的时候的v和v
#以此类推，每一行都是独一无二的
cast(md, ID+variable~Time)
#cast（数据集，保留项目~展开项目）
#保留ID和v把time的不同值作为新的变量展开接在后面
cast(md, Time~variable, mean)
#保留time，展开v，并求time=1的时候x1和x2均值以及time=2.。。。

#e.g. 2，面板数据整理
country<-c("China","USA","Japan") 
GDP2000<-c(5000,6000,7000) 
GDP2005<-c(5500,6500,7500) 
GDP2010<-c(5010,6010,7001) 
developed<-as.factor(c(0,1,1)) #用01指定是不是发达国家
Data<-data.frame(country,developed,GDP2000,GDP2005,GDP2010) 
Data
long<-reshape(Data,idvar="country",
              varying=list(names(Data)[3:5]), 
              v.names="GDP",timevar="year",
              times=c(2000,2005,2010),
              direction="long") 
#用country来区分，把data里面的3：5的数据附加成country.2000等
#新建GDP变量，和时间变量year并定义年份
long <- long[order(long$country),]
#按照国家排序
wide <- reshape(long, v.names="GDP", 
                idvar="country", timevar="year", 
                direction="wide")
#转回wide数据
rownames(wide) <- NULL
#消除行名恢复md的样子

#e.g 3
#"country","country isocode","year","rgdpl"
#"China Version 1","CHN","2007","5511.3609844"
#"China Version 1","CHN","2008","6013.4505546"
#"China Version 1","CHN","2009","6519.134412"
#"China Version 1","CHN","2010","7129.7392381"
#"Japan","JPN","2007","32775.849424"
#"Japan","JPN","2008","32384.250995"
#"Japan","JPN","2009","30094.166324"
#"Japan","JPN","2010","31453.080503"
Data1 <- read.csv("clipboard")
#长数据转化为宽
wide <- reshape(Data1, v.names="rgdpl", 
                idvar="country", timevar="year", 
                direction="wide")
long<-reshape(wide,idvar="country",
              varying=list(names(wide)[3:6]), 
              v.names="rgdpl",timevar="year",
              times=c(2007,2008,2009,2010),
              direction="long") 
rownames(long) <- NULL
long <- long[order(long$country),]