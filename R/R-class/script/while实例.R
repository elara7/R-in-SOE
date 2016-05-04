#while
#eg1
count <- 0 
while(count < 10) { 
        print(count) 
        count <- count + 1 
}
#eg2
i <- 1 
iTotal <- 0 
while(i <= 100) { 
        iTotal <- iTotal + i
        i <- i + 1
        } 
cat("Total:",iTotal,"\n",sep="")
#eg3 随机游走模型，输出每一次游走的z
z <- 5 
while(z >= 3 && z <= 10) { 
        print(z) 
        coin <- rbinom(1, 1, 0.5) #取伯努利分布p=0.5
        if(coin == 1) {  
                z <- z + 1 
        } 
        else { 
                z <- z - 1 
        } 
        }
