install.packages("animation")
library(animation)
output = dev2swf({
        par(mar = c(3, 3, 1, 1.5), mgp = c(1.5, 0.5, 0))
        kmeans.ani()
}, output = "test.swf")
swf2html(output)