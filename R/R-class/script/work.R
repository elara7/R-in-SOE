a <- function(angle){
        for (i in 1:angle){
                pie(sort(table(fit$Course),decreasing = F),col=rainbow(ceiling(runif(1,60,length(table(fit$Course))))),labels = rep("",length(table(fit$Course))),init.angle = i) 
        }
}
a(360)
library(animation)
library(ggplot2)

saveHTML(
{
x <- seq(from=0,to=4*pi,by=0.01)
y <- sin(x)
m <- data.frame(x,y)
p <- ggplot(data=m,aes(x=m$x,y=m$y))
ani.options(interval=0.02)
for (i in 1:100) {
print(p+geom_point(color="pink"))
ani.pause()
}
},
img.name = "Sinewave", 
htmlfile = "Sinewave.html"
)

