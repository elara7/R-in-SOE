setwd("~/soe/R/Data_Chap_2")
college=read.csv("college.csv")
head(college)
#修改x变成rownames
rownames(college)=college[,1]
college=college[,-1]
fix(college)
#生成配对的散点图矩阵
pairs(college[,1:10])
#箱线图
plot(college$Private,college$Outstate)
#生成全是no的Elite长度和college的row一样
Elite=rep("No",nrow(college))
#判断top10>50的作为yes
Elite[college$Top10perc>50]="Yes"
#转化Elite为因子
Elite=as.factor(Elite)
#colleged作为college和Elite的数据框
colleged=data.frame(college,Elite)
summary(colleged$Elite)
#箱线图
plot(colleged$Elite,colleged$Outstate)
#画出4连图(par参数决定几x几)
par(mfrow=c(2,2))
hist(colleged$Apps)
hist(colleged$perc.alumni,col=2)
hist(colleged$S.F.Ratio,col = 3,breaks = 10)
hist(colleged$Expend,breaks = 100)
par(mfrow=c(1,1))
#画散点图
plot(colleged$Outstate,colleged$Grad.Rate)
plot(colleged$Accept / colleged$Apps,colleged$S.F.Ratio)
plot(colleged$Top10perc,colleged$Grad.Rate)
#清空内存
rm(list = ls())