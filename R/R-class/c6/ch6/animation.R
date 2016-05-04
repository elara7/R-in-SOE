library(animation)
saveLatex({
        par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8,
            cex.lab = 0.8, cex.main = 1)
        brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow",
                        main = "Demonstration of Brownian Motion")
}, img.name = "BM", ani.opts = "controls,loop,width=0.95\\textwidth", 
latex.filename = ifelse(interactive(), "brownian_motion.tex", ""),  
interval = 0.15, nmax = 30, ani.dev = "pdf", ani.type = "pdf", ani.width = 7, 
ani.height = 7, documentclass = paste("\\documentclass{article}", 
                                      "\\usepackage[papersize={7in,7in},margin=0.3in]{geometry}",
                                      sep = "\n"))