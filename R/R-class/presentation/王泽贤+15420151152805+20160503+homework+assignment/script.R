library(ggplot2)
library(animation)

#generate a animation in which 30 random points
ani.options(interval = 0.2, nmax = 10)
for (i in 1:ani.options("nmax")) {plot(rnorm(30)); ani.pause()}

#several points moving randomly in a circle
BM.circle(n = 20, col = rainbow(7))

#Brownian motion
brownian.motion(n = 10, xlim = c(-20, 20), ylim = c(-20, 20))

#a simulation for the problem of Buffon’s Needle
buffon.needle(l = 0.8, d = 1, redraw = TRUE, 
              mat =matrix(c(1, 3, 2, 3), 2), 
              heights = c(3, 2), 
              col = c("lightgray","red", "gray", "red", "blue", "black", "red"), 
              expand = 0.4,type = "l")

#save to html
saveHTML(
        
        { par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, 
              
              cex.axis = 0.8, cex.lab = 0.8, cex.main = 1) ;ani.options(
                      
                      interval = 0.05, nmax = ifelse(interactive(), 150, 2)) ;
                
                buffon.needle(l = 0.8, d = 1, redraw = TRUE, 
                              
                              mat = matrix(c(1, 3, 2, 3), 2), heights = c(3, 2), col = 
                                      
                                      c("lightgray", "red", "gray", "red", "blue", "black", "red"), 
                              
                              expand = 0.4, type = "l") }, img.name = "buffon.needle", 
        
        htmlfile = "buffon.needle.html",navigator = FALSE, 
        
        description = c("a simulation for the problem of Buffon’s 
                        
                        Needle", "(without the navigation panel)"))



#my example
library(animation);library(ggplot2);
saveHTML({x<-seq(from=0,to=4*pi,by=0.01);
y<-sin(x);m <- data.frame(x,y);
p<-ggplot(data=m,aes(x=m$x,y=m$y));
ani.options(interval=0.02);
for(i in 1:100){m$y<-sin(m$x+i);
k<-p+geom_point(color="pink");print(k);
ani.pause()}},img.name="Sinewave",
htmlfile = "Sinewave.html")