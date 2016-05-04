library(rvest)
library(RCurl)
##eg1

%%R
library(rvest)
# vignette("selectorgadget")
#把网址内容读取进入legomovie
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

 %%R
#lego赋值给rating
 rating <- lego_movie %>% 
         #rating传输给nodes的x，提取span节点
         html_nodes("strong span") %>%
         #nodes传递给text的x，提取任意文本
         html_text() %>%
         #提取的文本传输给as，转化为数字
         as.numeric()
 #最后结果返回给rating
 #输出rating
 rating
 
 %%R
 cast <- lego_movie %>%
         #提取titleCast .itemprop span之间的节点
         html_nodes("#titleCast .itemprop span") %>%
         html_text()
 cast
 
 %%R
 poster <- lego_movie %>%
         #没有说明则使用css。需要xpath需要说明。
         #格式//div[@属性名='属性值'] /下一级节点名/下一级节点名
         html_nodes(xpath="//div[@class='poster']/a/img") %>%
         html_attr("src")
 poster
 
 
 poster <- lego_movie %>%
         #没有说明则使用css。需要xpath需要说明。
         #格式//div[@属性名='属性值'] /下一级节点名/下一级节点名
         html_nodes(xpath="//div[@class='slate']/a/img") %>%
         html_attr("src")
 poster
 
 
 