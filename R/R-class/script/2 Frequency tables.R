#One way tables
library("vcd")
## Loading required package: grid
#with绑定数据，调用table，返回一维列联表，三种效果的数量
mytable <- with(Arthritis, table(Improved))
mytable
#返回三种效果的频率
prop.table(mytable)
#返回百分比频率
prop.table(mytable)*100

#Two way tables
#返回二维列联表，2种变量交叉频数
mytable <- with(Arthritis, table(Treatment,Improved))
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)

DF <- as.data.frame(UCBAdmissions)
#返回数据~纬度名，数据
xtabs(Freq ~ Gender + Admit, DF)
#返回带行列频数求和的列联表
addmargins(mytable)
#返回带行列频率求和的列联表
addmargins(prop.table(mytable))