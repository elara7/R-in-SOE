# Walmart Sales
# Fill in all missing values to prepare for forecast
#
library(readr)
# Store Dept Date Weekly_Sales IsHoliday: 2010-02-05 ~ 2012-10-26
data1<-read_csv("C:/Course16/WISE2016/data/Walmart_train.csv")
# Store Type Size
data2<-read_csv("C:/Course16/WISE2016/data/Walmart_stores.csv")
# Store Date Temperature Fuel_Price Markdown1-5 CPI Unemplyment IsHoliday
data3<-read_csv("C:/Course16/WISE2016/data/Walmart_features.csv")
# Store Dept Date IsHoliday: 2012-11-02 ~ 2013-07-26
data4<-read_csv("C:/Course16/WISE2016/data/Walmart_test.csv")

# Need to fill missing values in data3
# (1) MarkDown1~5 are misterious with lots of missings
data3$MarkDown1[is.na(data3$MarkDown1)]<-0
data3$MarkDown2[is.na(data3$MarkDown2)]<-0
data3$MarkDown3[is.na(data3$MarkDown3)]<-0
data3$MarkDown4[is.na(data3$MarkDown4)]<-0
data3$MarkDown5[is.na(data3$MarkDown5)]<-0
# (2) Assume CPI and Unemployment the same as the last avialable month
data3$CPI[is.na(data3$CPI)]<-193.5893
data3$Unemployment[is.na(data3$Unemployment)]<-8.335

data4<-read_csv("C:/Course16/ec510/data/Walmart/test.csv")
# 2010-02-05 ~ 2012-10-26
train<-merge(data1,merge(data2,data3))
summary(train)  # make sure no missing values
# 2012-11-02 ~ 2013-07-26
test<-merge(data4,merge(data2,data3))
summary(test)   # make sure no missing values
rm(data1,data2,data3,data4)

train$Date<-as.Date(train$Date)
train$Store<-as.factor(train$Store)
train$Dept<-as.factor(train$Dept)
train$Type<-as.factor(train$Type)

# using OLS with Store and Dept dummyies
# Note: Size is correlated with Store dummyies
m1<-lm(Weekly_Sales~Store+Dept
       +IsHoliday+Temperature+Fuel_Price+CPI+Unemployment
       +MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5,data=train)
summary(m1)

test$Date<-as.Date(test$Date)
test$Store<-as.factor(test$Store)
test$Dept<-as.factor(test$Dept)
test$Type<-as.factor(test$Type)

test$Weekly_Sales<-predict(m1,newdata=test)

# Sales<-train[,c("Store","Date","Dept","Weekly_Sales")]
# 117 weeks forecasted
Sales_Total<-aggregate(test$Weekly_Sales,by=list(Date=test$Date,Type=test$Type),sum)
# 45 stores, 117 weeks
Sales_Store<-aggregate(test$Weekly_Sales,by=list(Store=test$Store,Date=test$Date,Type=test$Type),sum)
# 99 departments, 45 stores, 117 weeks
Sales_Dept<-aggregate(test$Weekly_Sales,by=list(Dept=test$Dept,Date=test$Date,Type=test$Type),sum)                       

library(ggplot2)
# Weekly_Sales forecasts
g0<-labs(title="Walmart Sales Forecasts",x="Date",y="Weekly Sales")
g1<-ggplot(data=Sales_Store,aes(x=Date,y=x)) + geom_line(aes(col=Store))
g1+g0
g1+facet_grid(Type ~ .)+g0
g2<-ggplot(data=Sales_Dept,aes(x=Date,y=x)) + geom_line(aes(col=Dept)) 
g2+g0
g2+facet_grid(Type ~ .)+g0
g4<-ggplot(data=Sales_Total,aes(x=Date,y=x)) + geom_line(aes(col=Type)) 
g4+g0

