library('faraway')
data("warpbreaks")
head(warpbreaks)
mod1 <- lm(warpbreaks$breaks ~ warpbreaks$wool)
mod2 <- lm(warpbreaks$breaks~ warpbreaks$tension)
mod3 <- lm(warpbreaks$breaks ~ warpbreaks$wool + warpbreaks$tension)
mod32 <- lm(warpbreaks$breaks ~ warpbreaks$tension+warpbreaks$wool)
mod4 <- aov(warpbreaks$breaks ~ warpbreaks$wool*warpbreaks$tension)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod32)
summary(mod4)
anova(mod3)
anova(mod32)
anova(mod4)
aov(warpbreaks$wool~warpbreaks$tension)
class(warpbreaks$tension)
resi <- residuals(mod4)
fitted <-predict(mod4)
plot(resi~fitted)
plot(TukeyHSD(mod4))
library(knitr)
data(Duncan)
modd <- lm(prestige~income,data =Duncan)
plot(modd)
influence(modd)
mean <- sapply(rep(10,100), function(i){
  mean(rep(rnorm(i),i))})
hist(var)
var <- sapply(rep(10,100), function(i){
  var(rep(rnorm(i),i))})
data <- read.table('Lime.prn',header = T)
head(data)
modl <- lm(ph~rate,data = al)
summary(modl)
lmmin <- lm.influence(modd)
sort(lmmin$hat)


al <- data[which(data$lime == 'AL'),]
mod22 <- lm(ph~factor(rate),data = al)
summary(mod22)
anova(mod22,modl)
deviance(mod22)
plot(mod22)
plot(ph~rate,data = al)
library(htmltools)
?gpar
library(RColorBrewer)
?mjs_labs
?mjs_grid

data.bin %>%
  filter(as.numeric(admit) %>% equals(1)) %>% 
  mjs_plot(gpa, width="300px", height="300px") %>%
  mjs_histogram(bins=20, bar_margin =2) %>% 
  mjs_labs(x_label = levels(data.bin$admit)[1]))


a<- ggplot(data.bin, aes(x = gpa)) + geom_histogram()
a$labels[[2]]
?plot
plot(mod.2)
as.numeric(data.bin$admit)

x <- rnorm(1000)
y <- rnorm(1000)
plot(x,y)
Highlightpoint <- function(i){
  a<-qplot(y = liky, x = likx, geom = 'line', xlab = 'Beta', ylab = 'Log likelihood') +
    geom_point(aes(x = my.nr$nr.mnew[c(1:i)], 
                   y = my.nr$loglik[c(1:i)]), color = 'blue') +
    ggtitle('Optimization of Log likelihood')
  
  print(a)
}
optlog <- function() {
  lapply(1:6, Highlightpoint)
}
saveGIF(optlog(), interval = .2, movie.name="opt.gif")

Highlightpoint(3)

par(mar = rep(3, 4))
for (i in 1:6) {
  plot(likx, liky, 'l')
  points(x = my.nr$nr.mnew[c(1:i)], y = my.nr$loglik[c(1:i)], pch = 10)
}

qplot(y = liky, x = likx, geom = 'line') + 
  geom_point(aes(x = likx[index], y = liky[index]), color = 'blue') + 
  geom_point(aes(x = as.numeric(n1), y = as.numeric(n1y)), color = 'red') + 
  annotation_custom(grob.org) + annotation_custom(grob.iter) +
  labs(x = 'Beta', y = 'Log likelihood', 
       title = '3.1 First step of Newton-Raphson')

?saveGIF
colors()
?points
aa <- 1:10
bb <- aa
??rainbow

colors()
plot(dnorm(seq(-4,4,0.01)), type = 'l')
hist(rnorm(length(seq(-4,4,0.01))))

?tapply
?lapply
?sapply
?by


rainbowPalette(16)

qplot(y = liky, x = likx, geom = 'line', xlab = 'Beta', ylab = 'Log likelihood') +
  geom_point(aes(x = my.nr$nr.mnew, 
                 y = my.nr$loglik), color = 'blue') +
  ggtitle('Optimization of Log likelihood') + 
  geom_text(aes(x = my.nr$nr.mnew, 
                y = my.nr$loglik,
                label = , size = 10,angle = 45, alpha = 1 )
            
            tmtest=function(x){n=length(x)
            
            
            Calpower <- function(d, alpha, n, n.rep){
              # Calculate the power of a two-sample t-test by simulations.
              #
              # Args:
              #   d: Difference between two population mean.
              #   alpha: Significance level.
              #   n: Sample size.
              #   n.rep: Number of replications for each simulation.
              #
              # Returns:
              #   Power of a specific two-sample t-test defined by args.
              A <- replicate(n.rep, rnorm(n, sd = 2, mean = d), simplify = 'matrix')
              B <- replicate(n.rep, rnorm(n, sd = 2, mean = 0), simplify = 'matrix')
              A.var <- apply(A, 2, var)
              B.var <- apply(B, 2, var)
              A.mean <- apply(A, 2, mean)
              B.mean <- apply(B, 2, mean)
              s <- sqrt((n - 1)*(A.var + B.var)/(2*n - 2))
              t.stat <- abs(A.mean - B.mean)/(s*(sqrt(2/n)))
              power <- mean(t.stat > qt(1-0.5*alpha, 2*n - 2)) + 
                mean(t.stat < qt(0.5*alpha, 2*n - 2))
              return(power)
            }
            
            
            
            
            
            k=trunc(0.3*n)
            y=sort(x)[(k+1):(n-k)]
            mean(y)/sd(y)}
            B=1000
            s=numeric(B)
            for (i in 1:B) s[i]=tmtest(rnorm(20))
            plot(density(s))
            length(s)
            CV=quantile(s,.975) # determine critical value by simulation
            a=numeric(10000)
            B=10000
            for (i in 1:B){x=rnorm(20)
            a[i]=(abs(tmtest(x))>CV)}
            
            k=trunc(0.3*100)
            ?trunc
            set.seed(44)
            Calpower(0.2, 0.05, 399, 1000)
            sapply(2:100, function(x) {Calpower(d= 0.8,alpha = 0.05, n.rep = 100, n = x)})
            a <- abs(rnorm(100))
            a.l <- log(a)
            var(a)
            var(a.l)
            CalSamplesize <- function(d, power, alpha, i){
              A <- rnorm(mean = 0, sd = 1, i)
              B <- rnorm(mean = d, sd = 1, i)
              s <- sqrt(((i - 1)*var(A) + (i - 1)*var(B))/(2*i - 2))
              stat[i] <- abs(mean(A) - mean(B))/(s*(sqrt(2/i)))
              while(mean(stat > qt(1 - alpha/2, 2*i - 2)) <= power) {
                i <- i+100
              }
              return(i)
            }
            CalSamplesize(0.8, 0.8, 0.05, 4)
            
            d = 0.5
            alpha = 0.05
            
            
            t.stat <- sapply(rep(84,10000),CalTstat, d, alpha)
            power.simu <- mean(t.stat > qt(0.975,166))
            
            hist(t.stat)
            power.simu <- mean(stat > qt(1 - alpha/2, 2*x - 2))
            return(power.simu)
            set.seed(4444)
            stat <- NULL
            for(i in 1:1000) {
              A <- rnorm(mean = 3, sd = 1, 64)
              B <- rnorm(mean = 2.5, sd = 1, 64)
              s <- sqrt(((64 - 1)*var(A) + (64 - 1)*var(B))/(2*64 - 2))
              stat[i] <- (abs(mean(A) - mean(B)))/(s*(sqrt(2/64)))
            }
            power.simu <- mean(stat > qt(0.975, 2*30 - 2))
            
            ?replicate
            n <- 64
            p1 <- replicate(100000, rnorm(n, sd = 1, mean = 3), simplify = 'matrix')
            p2 <- replicate(100000, rnorm(n, sd = 1, mean = 2.5), simplify = 'matrix')
            p1.var <- apply(p1, 2, var)
            p2.var <- apply(p2, 2, var)
            p1.mean <- apply(p1, 2, mean)
            p2.mean <- apply(p2, 2, mean)
            s <- sqrt((n - 1)*(p1.var + p2.var)/(2*n - 2))
            t.stat <- abs(p1.mean - p2.mean)/(s*(sqrt(2/n)))
            power <- mean(t.stat > qt(0.975, 2*n - 2))
            power
            
            
            
            t.stat <- sapply(rep(64,100000),CalTstat, d = 0.5, alpha = 0.05)
            stat <- rep(0,100000)
            mean(t.stat > qt(0.975,126))
            for (i in 1:100000){
              A <- rnorm(mean = 3, sd = 1, n = 64)
              B <- rnorm(mean = 2.5, sd = 1, n = 64)
              s <- sqrt((63*var(A) + 63*var(B))/126)
              stat[i] <- (mean(A) - mean(B))/(s*(sqrt(2/64)))
            }
            hist(stat)
            mean(stat > qt(0.975,126)) + mean(stat < qt(0.025,126))
            
            
            set.seed(4444)
            
            CalTstat <- function(x, d, alpha){
              A <- rnorm(mean = 0, sd = 1, x)
              B <- rnorm(mean = d, sd = 1, x)
              s <- sqrt(((x - 1)*var(A) + (x - 1)*var(B))/(2*x - 2))
              stat <- abs(mean(A) - mean(B))/(s*(sqrt(2/x)))
            }
            system.time(t.stat <- sapply(rep(64,100),CalTstat, d = 0.5, alpha = 0.05))
            
            CalSamplesize <- function(d, power, alpha){
              for(i in 3:20000) {
                t.stat <- sapply(rep(i,1000),CalTstat, d, alpha)
                power.simu <- mean(t.stat > qt(0.975,2*i-2)) + mean(t.stat < qt(0.025,2*i - 2))
                if(power.simu >= power)
                  break
              }
              return(i)
            }
            
            CalSamplesize(d = 0.8, power = 0.9, alpha = 0.05)
            
            
            
            ptm <- proc.time()
            set.seed(4444)
            n <- 64
            p1 <- replicate(2000, rnorm(n, sd = 1, mean = 3), simplify = 'matrix')
            p2 <- replicate(2000, rnorm(n, sd = 1, mean = 2.5), simplify = 'matrix')
            p1.var <- apply(p1, 2, var)
            p2.var <- apply(p2, 2, var)
            p1.mean <- apply(p1, 2, mean)
            p2.mean <- apply(p2, 2, mean)
            s <- sqrt((n - 1)*(p1.var + p2.var)/(2*n - 2))
            t.stat <- abs(p1.mean - p2.mean)/(s*(sqrt(2/n)))
            proc.time() - ptm
            CalSamplesize(0.5,0.9,0.05)
            
            Calpower <- function(d, alpha, n){
              p1 <- replicate(3000, rnorm(n, sd = 1, mean = 0), simplify = 'matrix')
              p2 <- replicate(3000, rnorm(n, sd = 1, mean = d), simplify = 'matrix')
              p1.var <- apply(p1, 2, var)
              p2.var <- apply(p2, 2, var)
              p1.mean <- apply(p1, 2, mean)
              p2.mean <- apply(p2, 2, mean)
              s <- sqrt((n - 1)*(p1.var + p2.var)/(2*n - 2))
              t.stat <- abs(p1.mean - p2.mean)/(s*(sqrt(2/n)))
              power <- mean(t.stat > qt(1-0.5*alpha, 2*n - 2))
              return(power)
            }
            set.seed(4444)
            CalSamplesize.bi <- function(d, alpha, n.max, power){
              n.l <- 3
              n.r <- n.max
              power.l <- Calpower(d, alpha, n.l)
              power.r <- Calpower(d, alpha, n.r)
              diff <- abs(n.r - n.l)
              while(diff > 0){  
                n.mid <- round((n.l + n.r)/2)
                power.mid <- Calpower(d, alpha, n.mid)
                if (power.mid < power)
                  n.l <- n.mid
                if (power.mid > power)
                  n.r <- n.mid
                if (power.mid == power) {
                  n.r <- n.mid
                  n.l <- n.mid
                  break
                }
                diff <- abs(n.r - n.l)
              }
              return(n.l)
            }
            set.seed(4444)
            CalSamplesize.bi(0.2, 0.05, 10000, 0.8)
            set.seed(4444)
            s1 <- sapply(c(0.2,0.5,0.8), CalSamplesize.bi, alpha = 0.05, n.max = 1000, power = 0.8)
            s2 <- sapply(c(0.2,0.5,0.8), CalSamplesize.bi, alpha = 0.05, n.max = 1000, power = 0.9)
            matrix(c(s1,s2),ncol = 3, byrow = T)
            
            ?lapply
            
            
            
            
            CalSamplesize.bi <- function(d, alpha, n.max, power){
              n.l <- 2
              n.r <- n.max
              diff <- abs(n.r - n.l)
              while (diff > 0) {
                power.l <- Calpower(d, alpha, n.l, n.rep = 200)
                power.r <- Calpower(d, alpha, n.r, n.rep = 200)
                if (power.l == power) {
                  print(n.l)
                }
                else if (power.r == power) {
                  print(n.r)
                }
                else if ((power.l - power) + (power.r - power) < 0) {
                  repeat {
                    n.mid <- (n.r + n.l)/2
                    power.mid <- Calpower(d, alpha, n.mid, 200)
                    if (diff = 0) 
                      break
                    if (power.l * power.mid < 0) 
                      n.r <- n.mid
                    else n.l <- n.mid
                  }
                  print(n.mid)
                }
                diff <- n.r - n.l
              }
            }
            set.seed(3)
            replicate(3,rnorm(6),simplify = 'matrix')
            set.seed(3)
            rnorm(6)
            data("longley")
            g <- lm(Employed ~ ., longley)
            summary(g)
            round(cor(longley))
            x <- as.matrix(longley[,-7])
            e <- eigen(t(x) %*% x)
            e$val
            sqrt(e$val[1]/e$val)
            summary(lm(x[,1] ~ x[,-1]))$r.squared
            
            data("divusa")
            head(divusa)
            g <- lm(data = divusa[,2:7], divorce ~.)
            x <- as.matrix(divusa[,3:7])
            e <- eigen(t(x)%*%x)$values
            ev <- e[1]/e
            ev
            vif(x)
            cor(x)
            summary(g)
            set.seed(4444)
            Calpower(d = 0.5, alpha = 0.05, n, n.rep = 100)
            set.seed(4444)
            Calpower(d = 0.5, alpha = 0.05, n, n.rep = 1000)
            set.seed(4444)
            Calpower(d = 0.5, alpha = 0.05, n, n.rep = 10000)