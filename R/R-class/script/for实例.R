#for
#eg1 
x <- c("a", "b", "c", "d") 
for(i in 1:4) { 
        print(x[i]) 
        }
#eg2 seq_along(x)相当于上面的1：4
for(i in seq_along(x)) { 
        print(x[i]) 
}
#eg3 
for(letter in x) { 
        print(letter) 
}
#eg4同一行不用{}
for(i in 1:4) print(x[i])
#eg5
iTotal <- 0 
for(i in 1:100) { 
        iTotal <- iTotal + i 
        } 
cat("Sum of 1-100:",iTotal,"\n",sep="")#类似printf。sep：连接符
#eg6 利用循环实现换行输出
szSymbols <- c("MSFT","GOOG","AAPL","INTL","ORCL","SYMC") 
for(SymbolName in szSymbols) { 
        cat(SymbolName,"\n",sep="") 
}
#eg7 嵌套循环
x <- matrix(1:6, 2, 3) 
x
for(i in seq_len(nrow(x))) { 
        for(j in seq_len(ncol(x))) {
                print(x[i, j]) 
        } 
        }




