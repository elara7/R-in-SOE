# TOTAL WEEKLY SALES OF IMPORT AND DOMESTIC NON VQA RED & WHITE TABLE WINE 
# WITHIN MUNICIPALITY OF VANCOUVER IN UNITS AND LITRES
# FROM WEEK ENDING APRIL 4, 2009 TO WEEK ENDING MAY 28, 2011
# Using readxl package to read Excel spreadsheet: read_excel
# Using qplot() of ggplot2 package for data visualization
# install.packages("ggplot2")
# install.packages("readxl")
# library(readxl) 
# wine<-read_excel("C:/Course16/EC510/data/Vancouver_Non_VQA_Sls_Apr1toMay31_rev.xlsx",na="NA",skip=5)
# wine<-read_excel("C:/Course16/EC510/data/Vancouver_Non_VQA_Sls_Apr1toMay31.xlsx",sheet=2)
library(readr) 
# wine<-read_csv("http://web.pdx.edu/~crkl/WISE2016/data/Vancouver_Non_VQA_Sls_Apr1toMay31.csv",na="NA")
load("C:\\Users\\44180\\Documents\\R-in-SOE\\R\\big data\\case2\\wine.RData")
dim(wine)
names(wine)
summary(wine)

where<-wine$`Store Category Sub Name`    # import from ...
where<-ifelse(substr(where,1,6)=="FRANCE","FRANCE",where)   # combine all france wines
where<-ifelse(substr(where,1,7)=="GERMANY","GERMANY",where) # combine all germany wines
big5<-c("AUSTRALIA","UNITED STATES","FRANCE","ITALY","OTHERS")
where5<-ifelse(!is.element(where,big5),"OTHERS",where)
wine$where<-factor(where5)
wine$what<-factor(wine$`Store Category Minor Name`)   # Red or White
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

library(ggplot2)

qplot(x=price_level,y=price,geom="boxplot",data=wine)
qplot(x=price_level,y=price,geom="jitter",data=wine)
qplot(price,geom="histogram",data=wine)

# reduce dataset to consider only the case of price<=100
varsel<-c("what","where","quantity","price","price_level")   #,"france")
wine100<-subset(wine,price<100 & quantity>0,varsel) # not including returns
rm(wine)

qplot(x=where,y=price,geom="boxplot",data=wine100)
qplot(x=what,y=price,geom="jitter",data=wine100)

qplot(price,data=wine100) # histogram
qplot(price,geom="freqpoly",data=wine100)
qplot(price,geom="density",color=I("blue"),data=wine100)
qplot(price,geom="histogram",fill=what,data=wine100)
# qplot(price,geom="bar",fill=what,data=wine100)
qplot(price,geom="histogram",fill=where,data=wine100)
qplot(price,geom="density",fill=where,data=wine100)
# Using Facets
qplot(price,geom="bar",fill=what,data=wine100)
qplot(price,geom="bar",data=wine100) + facet_grid(what ~ .)
qplot(price,geom="bar",data=wine100) + facet_grid(. ~ what)
qplot(price,geom="bar",data=wine100) + facet_grid(where ~ .)
qplot(price,geom="bar",data=wine100) + facet_grid(. ~ where)
# demand analysis
model1<-lm(quantity~price,data=wine100)
summary(model1)

qplot(price,quantity,data=wine100,alpha=I(1/3))
qplot(price,quantity,data=wine100,alpha=I(1/3),col=what)
qplot(price,quantity,data=wine100,alpha=I(1/3),col=where)
qplot(price,quantity,data=wine100,alpha=I(1/3),col=where,
      geom=c("point","smooth"),method="lm")

# log-model could perform better
model2<-lm(log(quantity)~log(price),data=wine100)
summary(model2)

qplot(price,quantity,data=wine100,alpha=I(1/3),log="xy")
qplot(price,quantity,data=wine100,alpha=I(1/3),col=what,log="xy")
qplot(price,quantity,data=wine100,alpha=I(1/3),col=where,log="xy")
qplot(price,quantity,data=wine100,alpha=I(1/3),
      geom=c("point","smooth"),method="lm",log="xy")
qplot(price,quantity,data=wine100,alpha=I(1/3),col=what,
      geom=c("point","smooth"),method="lm",log="xy")
qplot(price,quantity,data=wine100,alpha=I(1/3),
      geom=c("point","smooth"),method="lm",log="xy") + facet_grid(what ~ .)
qplot(price,quantity,data=wine100,alpha=I(1/3),
      geom=c("point","smooth"),method="lm",log="xy") + facet_grid(where ~ .)

# xlim, ylim, xlab, ylab, main may be used as in basic plot