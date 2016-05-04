mydata <- rnorm(100) 
sd(mydata)
## [1] 0.9467659
sd(x = mydata)
## [1] 0.9467659
sd(x = mydata, na.rm = FALSE)
## [1] 0.9467659
sd(na.rm = FALSE, x = mydata)
## [1] 0.9467659
sd(na.rm = FALSE, mydata)#这个函数只有2个输入参数，确定其中一个另一个也确定
## [1] 0.9467659

#...写在前面不能按照位置匹配。
