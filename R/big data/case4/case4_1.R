# Xavier X. Sala-I-Martin: I Just Ran Two Million Regression (AER, 1997)
# Growth Convergence of 72 countries with 41 variables
# Using stepwise regressions
# setwd("C:/Course16/WISE2016/R")
growth <- read.csv("C:/Course16/WISE2016/data/FLS-data.csv")
summary(growth)

# check for multicolinearity
X <- as.matrix(growth[,-1])
# Variance, Covariance, Correlation of X
cov(X)
cor(X)
# Condition Number
Xm <- as.matrix(cbind(rep(1,nrow(X)),X))
# Xm <- cbind(rep(1,nrow(X)),X)
ev <- eigen(t(Xm)%*%Xm)
cond <- sqrt(max(ev$values)/min(ev$values))
cond

# regression with 41 variables
ols1 <- lm(y ~ ., data=growth)  # full model (.^2 includes interactions)
summary(ols1)

# using car package to check for multicolinearity
library(car)
vif(ols1)    # variance inflation factor
kappa(ols1)  # condition number

# Stepwise Regression begins with null model
ols0 <- lm(y ~ 1, data=growth)    # null model

model.f <- step(ols0,scope=list(lower=ols0,upper=ols1),direction="forward")
summary(model.f)
model.b <- step(ols1, direction="backward")
summary(model.b)
model <- step(ols1)  # k=log(n) for BIC criteria
summary(model)

