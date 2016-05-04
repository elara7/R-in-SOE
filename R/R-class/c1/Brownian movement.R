install.packages("R2SWF")
library(R2SWF)
if (capabilities("cairo")) {
        olddir = setwd(tempdir())
        svg("Rplot%03d.svg", onefile = FALSE)
        set.seed(123)
        x = rnorm(5)
        y = rnorm(5)
        for (i in 1:100) {
                plot(x <- x + 0.1 * rnorm(5), y <- y + 0.1 * rnorm(5), xlim = c(-3,
                                                                                3), ylim = c(-3, 3), col = "steelblue", pch = 16, cex = 2, xlab = "x",
                     ylab = "y")
        }
        dev.off()
        output = svg2swf(sprintf("Rplot%03d.svg", 1:100), interval = 0.1)
        swf2html(output)
        setwd(olddir)
}