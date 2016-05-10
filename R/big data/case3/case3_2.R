# Walmart Sales
# Regression Analysis
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

# 2010-02-05 ~ 2012-10-26
train<-merge(data1,merge(data2,data3))
# 2012-11-02 ~ 2013-07-26
test<-merge(data4,merge(data2,data3))

rm(data1,data2,data3,data4,test)
# Analysis for train data only
summary(train)
dim(train)

train$Date<-as.Date(train$Date)
train$Store<-as.factor(train$Store)
train$Dept<-as.factor(train$Dept)
train$Type<-as.factor(train$Type)

# MarkDown1~5 are misterious with lots of missings
train$MarkDown1[is.na(train$MarkDown1)]<-0
train$MarkDown2[is.na(train$MarkDown2)]<-0
train$MarkDown3[is.na(train$MarkDown3)]<-0
train$MarkDown4[is.na(train$MarkDown4)]<-0
train$MarkDown5[is.na(train$MarkDown5)]<-0

# separate data by the store types, select one to use for OLS
train_A<-subset(train,Type=="A")
train_B<-subset(train,Type=="B")
train_C<-subset(train,Type=="C")
data_used<-train_C

# using OLS with Store and Dept dummyies
# equivalent to panel data fixed effects
m1<-lm(Weekly_Sales~Size+Store+Dept,data=data_used)
summary(m1)
anova(m1)

m2<-update(m1,.~.+IsHoliday+Temperature+Fuel_Price+CPI+Unemployment)
summary(m2)
anova(m1,m2)

m3<-update(m2,.~.+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5)
summary(m3)
anova(m2,m3)

