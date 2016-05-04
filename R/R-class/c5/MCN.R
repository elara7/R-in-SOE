MCN <- function(k){
        for (i in 1:k){
                x <- runif(n=12,min=0,max=1)
                y[i]<-sum(x)-6
        }
        hist(y)
}