#环境设定
setwd("~/soe/GHL")
library(jiebaR)
library(stringr)
#数据预处理
part1.ori <- readLines("part1.txt")
#detitem <- "主持人：|受访者：|比如说|比较|可能|觉得|东西|大概|其实|一下|一个|现在|天无|再就是|这种|之后|那种|有没有|很多|或者说|应该|
#是吧|还好|他家|不错|包括|了吧|一次|刚才|多大|不好|不太|一般来讲|一家|大部分|没有|偶尔|接下来|知道|用得|相对来说"
#part1.no <- str_split(string = part1.ori,pattern =  "主持人|受访者")
#part1.no <- str_split(string = part1.ori,pattern =  detitem)
#part1.no <- unlist(part1.no)
#part1.no <- na.omit(part1.no)
p1all <- NULL
for(i in seq_along(part1.ori)){
        p1all <- paste(p1all,part1.ori[i])
}
p1all <- na.omit(p1all)
#停止词设置
filter_words = c("主持人","受访者","比如说","比较","可能","觉得","东西","大概","其实","一下","一个","现在","天无","再就是","这种",
                 "之后","那种","有没有","很多","或者说","应该","是吧","还好","他家","不错","包括","了吧","一次","刚才","多大","不好",
                 "不太","一般来讲","一家","大部分","没有","偶尔","接下来","知道","用得","相对来说","包得","较方","这样的","样的","可能会",
                 "接下","一直","天无理","")
#关键词引擎初始化，50个关键词
keyworker = worker("keywords", topn = 50)
#分词（mix算法）提取
cutter = worker(type="mix")
result_segment.mix <-cutter[p1all] 
result.mix <- filter_segment(result_segment.mix,filter_words)
keywords.mix <- vector_keywords(result.mix,keyworker)
keywords.mix
#分词（hmm算法）提取
cutter = worker(type="hmm")
result_segment.hmm <-cutter[p1all] 
result.hmm <- filter_segment(result_segment.hmm,filter_words)
keywords.hmm <- vector_keywords(result.hmm,keyworker)
keywords.hmm
#分词（mp算法）提取
cutter = worker(type="mp")
result_segment.mp <-cutter[p1all] 
result.mp <- filter_segment(result_segment.mp,filter_words)
keywords.mp <- vector_keywords(result.mp,keyworker)
keywords.mp


