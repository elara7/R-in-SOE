# Walmart Sales
# Fill in all missing values to prepare for forecast
# Using Panel Data Structure to extract time series
# Using last week's sale to improve the model, but difficult to forecast
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

# Analysis for train data only
library(plm)
# using panel data frame to create lag variables
train$ID<-paste(train$Store,train$Dept,sep="_")
train.pd<-pdata.frame(train,c("ID","Date"))
train.pd$Weekly_Sales1<-lag(train.pd$Weekly_Sales,1)

# using OLS with Store and Dept dummy variables
m1<-lm(Weekly_Sales~Store+Dept+Weekly_Sales1
       +IsHoliday+Temperature+Fuel_Price+CPI+Unemployment
       +MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5,data=train.pd)
summary(m1)

# Forecasting will be difficult, because Weekly_Sale1 must be forecasted first
# as part of predictors...

