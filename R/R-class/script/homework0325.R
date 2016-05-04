homework0325 <- function(n){
        {#判断是不是整数。输入数据默认是double，is.integer不好用#
                isInt<-function(n){
                if(mode(n)=="numeric")
                {
                        if(n%%1==0) return(TRUE)     
                        else return(FALSE)
                }
                else return(FALSE)
                }
        }
        {if(!isInt(n)|n <= 0) { 
                print("要求输入正整数") 
        }else{
                {while(n!=1) {
                                if(n%%2==0) { 
                                        n <- n/2
                                }else {n <- 3*n+1}
                             }
                }
                print("运算成功")
             }
        }
}
