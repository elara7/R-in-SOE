# Chow test
chow <-function(dat1,                                        #data No.1
                dat2)                                   #data No.2
{
        ess <- function(dat)                         # sum of squares
        {
                nc <- ncol(dat)
                ans <- lm(dat[,nc] ~ dat[,-nc])              #dependent variables
                return(sum(ans$residuals^2))            # sum of squares
        }
        
        method <- "Chow  TEST"
        data.name <- paste(deparse(substitute(dat1)), "and", deparse(substitute(dat2)))
        dat1 <- subset(dat1, complete.cases(dat1))   # exclud missing values
        dat2 <- subset(dat2, complete.cases(dat2))   # exclud missing values
        ess12 <- ess(dat1)+ess(dat2)
        essc <- ess(rbind(dat1, dat2))
        df1 <- ncol(dat1)                            # degree of freedom
        df2 <- nrow(dat1)+nrow(dat2)-2*df1           # degree of freedom
        f <- (essc-ess12)*df2/(df1*ess12)            # Test statistic
        p <- pf(f, df1, df2, lower.tail=FALSE)               # P value
        return(structure(list(statistic=c(F=f),
                              parameter=c(df1=df1, df2=df2), p.value=p,
                              method=method, data.name=data.name), class="htest"))
}
