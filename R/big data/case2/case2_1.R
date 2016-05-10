# TOTAL WEEKLY SALES OF IMPORT AND DOMESTIC NON VQA RED & WHITE TABLE WINE 
# WITHIN MUNICIPALITY OF VANCOUVER IN UNITS AND LITRES
# FROM WEEK ENDING APRIL 4, 2009 TO WEEK ENDING MAY 28, 2011
# Using readxl package to read Excel spreadsheet: read_excel
setwd("~/R-in-SOE/R/big data")
# install.packages("readxl")
library(readxl) 
# wine<-read_excel("C:/Course16/EC510/data/Vancouver_Non_VQA_Sls_Apr1toMay31_rev.xlsx",na="NA",skip=5)
# wine<-read_excel("C:\\Users\\44180\\Documents\\Surface-workandstudy\\soe\\bigdata\\Vancouver_Non_VQA_Sls_Apr1toMay31.xlsx")
library(readr) 
# download.file("http://web.pdx.edu/~crkl/WISE2016/data/Vancouver_Non_VQA_Sls_Apr1toMay31.csv","Vancouver_Non_VQA_Sls_Apr1toMay31.csv",mode="wb")
# wine<-read_csv("http://web.pdx.edu/~crkl/WISE2016/data/Vancouver_Non_VQA_Sls_Apr1toMay31.csv",na="NA")
wine<-read_csv("C:\\Users\\44180\\Documents\\Surface-workandstudy\\soe\\bigdata\\Vancouver_Non_VQA_Sls_Apr1toMay31.csv",na="NA")
dim(wine)
names(wine)
summary(wine)

wine$what<-factor(wine$`Store Category Minor Name`)   # Red or White
wine$where<-factor(wine$`Store Category Sub Name`)    # import from ...
wine$quantity<-wine$`Total Weekly Selling Unit`

# price analysis
wine$price<-wine$`Current Display Price`
wine$price_level<-cut(wine$price,breaks=c(-Inf,50,100,1000,Inf),labels=c("Low","Medium","High","Expensive"))

t1<-with(wine,table(where,price_level))
t1
barplot(t1)

t2<-with(wine,table(what,price_level))
t2
barplot(t2)

with(wine,boxplot(price~price_level))
with(wine,hist(price))

# reduce dataset to consider only the case of price<=100
varsel<-c("what","where","quantity","price","price_level")
wine100<-subset(wine,price<100 & quantity>0,varsel) # not including returns
rm(wine)

with(wine100,hist(price))
with(wine100,hist(price,freq=F))
with(wine100,lines(density(price),col="blue"))

# demand analysis
model1<-lm(quantity~price,data=wine100)
summary(model1)

with(wine100,
     plot(price,quantity,col="lightblue",xlab="Price",ylab="Quantity",main="Weekly Sales of Table Wine in Vancouver BC"))
abline(model1,col="darkblue")

# log-model could perform better
model2<-lm(log(quantity)~log(price),data=wine100)
summary(model2)

with(wine100,
     plot(log(price),log(quantity),col="lightgreen",xlab="log(Price)",ylab="log(Quantity)",main="Weekly Sales of Table Wine in Vancouver BC"))
abline(model2,col="darkgreen")
