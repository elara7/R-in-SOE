# Xavier X. Sala-I-Martin: I Just Ran Two Million Regression (AER, 1997)
# Growth Convergence of 72 countries with 41 variables
# Principal Components Analysis
# setwd("C:/Course16/WISE2016/R")
growth <- read.csv("C:/Course16/WISE2016/data/FLS-data.csv")
summary(growth)

# principal components analysis
# working with data matrices
X <- as.matrix(growth[,-1])
Y <- as.matrix(growth[,1])

# PCA on X
# compute correlation matrix of X
R <- cor(X)
# find eigenvalues and eigenvectors of R
EV <- eigen(R)
str(EV)
# EV$Values are the variances of PC
# EV$Vectors are the loadings
PCsd <- sqrt(EV$values)
PCloadings <- EV$vectors
rownames(PCloadings) <- colnames(X)

PCscores <- X %*% PCloadings
print(PCsd)
print(PCloadings)
print(PCscores)
plot(PCscores[,1], PCscores[,2], xlab="PCA 1", ylab="PCA 2",
     type="n", xlim=c(min(PCscores[,1:2]), max(PCscores[,1:2])),
     ylim=c(min(PCscores[,1:2]), max(PCscores[,1:2])))
arrows(0,0,PCloadings[,1]*3,PCloadings[,2]*3, length=0.1,
       angle=20, col="red")
text(PCloadings[,1]*3*1.2,PCloadings[,2]*3*1.2,
     rownames(PCloadings), col="red", cex=0.7)
text(PCscores[,1],PCscores[,2], rownames(PCscores), col="blue",
     cex=0.7)

# principal components analysis
# using princomp, prcomp, factanal packages
# data will be scaled in computation

# PCA on X
pca <- princomp(X, cor=TRUE)
summary(pca)
pca$scores
pca$loadings
plot(pca$scores[,1])
barplot(pca$scores[,1])

reg1 <- lm(Y~pca$scores[,1:12])
summary(reg1)

reg2 <- lm(Y~pca$scores[,21:41])
summary(reg2)

# using prcomp package
# data must be scaled, otherwise set center and scale.
prc <- prcomp(X)
summary(prc)
prc$sdev
prc$rotation  # loadings
prc$x         # scores

#using factanal package
fa <- factanal(X, factors=12, scores="regression")
summary(fa)
fa$loadings
fa$scores

reg3 <- lm(Y~fa$scores)
summary(reg3)

