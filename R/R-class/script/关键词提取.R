library(jiebaR)
#初始化启动单文本关键词算法
keys = worker("keywords", topn = 1)
#赋予文本，返回关键词和TF-IDF
keys <= "我爱北京天安门"
keys <= "一个文件路径.txt"
#初始化启用文本向量关键词算法
keyworker = worker("keywords", topn = 5)
#分词器启动
cutter = worker()
#提取关键词，cutter[]表示对[]内的文本分词得到文本向量
vector_keywords(cutter["这是一个比较天猫的的厉害的的测试文本。"],keyworker)