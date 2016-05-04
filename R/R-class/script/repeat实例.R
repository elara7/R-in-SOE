i <- 1 
iTotal <- 0 
repeat { 
        iTotal <- iTotal + i 
        i <- i + 1 
        if(i <= 100) 
                next else break 
        } 
cat("Total:",iTotal,"\n",sep="")
