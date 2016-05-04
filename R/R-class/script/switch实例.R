#switch
n <- 1 
switch(n, print("option1"), print("option2"), print("option3") )
## [1] "option1"

ccc <- c("b","QQ","a","A","bb")
for(ch in ccc)
        cat(ch,":",switch(EXPR = ch,a=1,b=2:3),"\n")
#EXPR=是固定开头，后面接判断元素
#a=1相当于如果ch=a，那么输出1