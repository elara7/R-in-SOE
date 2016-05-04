library(jiebaR)
library(stringr)
keyworker = worker("keywords", topn = 10)
cutter = worker()
vector_keywords(cutter["texrt"],keyworker)
p2 <- readLines("part2.txt")
detitem <- "江泽民|旁白|记者："
p2 <- str_split(string = p2,pattern =  detitem)
p2 <- unlist(p2)
l <- length(p2)
p2all <- NULL
for(i in 1:61){
        p2all <- paste(p2all,p2[i])
}
vector_keywords(cutter[p2all],keyworker)