set.seed(12450)
y <- rnorm(100)
n <- length(y)
plot(0,1,xlim = c(1,n), ylim = c(-10 , 10),type = "n" )
plot(1:n,y,xlim = c(1,n),ylim = c(-10,10),)
for (i in 1:200) {
        abline()
}
plot(0, 1 ,xlim = c(0, 200), ylim = c(3.5,6.5), type = "n")

m <- data.frame(x=c(1:n),y)
library(ggplot2)
library(animation)
windows(7,7)
oopt <- ani.options(interval=0.1)
m <- data.frame(x=c(1:length(y)),y=y)
p <- ggplot(data = m ,mapping = aes(x=x,y=y))
for (i in 1:100) {
m1 <- data.frame(x=c(1:i),y=y[1:i])
p+geom_point(data = m1)
print(p)
ani.pause()
}
ani.options(oopt)
boot.iid(x = runif(20), statistic = mean, m = length(x), mat = matrix(1:2, 2),
         widths = rep(1, ncol(mat)), heights = rep(1, nrow(mat)), col = c("black", "red",
                                                                          "bisque", "red", "gray"), cex = c(1.5, 0.8), main)
brownian.motion(n = 10, xlim = c(-20, 20), ylim = c(-20, 20))
buffon.needle(l = 0.8, d = 1, redraw = TRUE, mat = matrix(c(1, 3, 2, 3), 2),
              heights = c(3, 2), col = c("lightgray", "red", "gray", "red", "blue", "black",
                                         "red"), expand = 0.4, type = "l")
ani.options("C:/Software/LyX/etc/ImageMagick/convert.exe")

saveGIF({ brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow") }, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30, ani.width = 600, ani.height = 600)

des = c("Random walk of 10 points on the 2D plane:", "for each point (x, y),",
        "x = x + rnorm(1) and y = y + rnorm(1).")
saveHTML({
        par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8,
            cex.lab = 0.8, cex.main = 1)
        ani.options(interval = 0.05, nmax = ifelse(interactive(), 150,
                                                   2))
        buffon.needle(l = 0.8, d = 1, redraw = TRUE, mat = matrix(c(1, 3, 2, 3), 2),
                      heights = c(3, 2), col = c("lightgray", "red", "gray", "red", "blue", "black",
                                                 "red"), expand = 0.4, type = "l")
}, img.name = "buffon.needle", htmlfile = "buffon.needle.html")