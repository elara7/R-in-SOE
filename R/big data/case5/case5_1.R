# AmEx Credit Scoring and Default
# DEFAULT|CARDHLDR ~ AGE+INCOME+OWNRENT+SELFEMPL+ACADMOS, used by Greene's lecture
# setwd("C:/Course16/WISE2016/R")
AmEx<-read.csv("C:/Course16/WISE2016/data/AmEx.csv")
# exclude<-c(1,2,"BANKBOTH","CREDGAS","UNEMP","DRUGSTOR")
table(AmEx$CARDHLDR)
table(AmEx$DEFAULT)
with(AmEx,table(DEFAULT,CARDHLDR))
xtabs(~ DEFAULT+CARDHLDR,data=AmEx)
# (9503+996)/13444=78% card holders
# 996/(9503+996)=9.5% card holders will default
# DEFAULT|CARDHLDR=1
AmEx1<-subset(AmEx,CARDHLDR==1,select=-c(1,6,12,17,39:56))
summary(AmEx1)
Y<-as.matrix(AmEx1[,"DEFAULT"])
X<-as.matrix(AmEx1[,c("AGE","INCOME","OWNRENT","SELFEMPL","ACADMOS")])

ols<-lm(Y~X)
summary(ols)

logit<-glm(Y~X,family="binomial")
summary(logit)

# stepwise logit regression
logit0<-glm(DEFAULT~1,data=AmEx1,family="binomial")
logit1<-glm(DEFAULT~.,data=AmEx1,family="binomial")
logit2<-step(logit0d,scope=list(lower=logit0,upper=logit1),direction="forward")
summary(logit2)

library(boot)
# cross validation can take a long time to run
logit2.cv<-cv.glm(glmfit=logit2,data=AmEx1)
summary(logit2.cv)
# use glmnet pacakge to select variables and run CV will be more efficient


