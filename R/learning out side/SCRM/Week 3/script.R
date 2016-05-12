setwd('C:\\Users\\SONY\\Desktop\\LM')
head(img)
summary(img)
table(img)
str(img.df)
img[, 1, 1]
head(ftable(img))
str(data.frame(rep(1:120275, 4),c(gl(4, 120275)), c(img[, , 1]), c(img[, , 2]), c(img[, , 3]), c(img[, , 4])))

?kmeans

head(img.kmeans$centers, 10)
str(img.kmeans$centers)
str(img.kmeans$cluster)

X <- img.df[,RGB]
img.df <- data.frame(rep(1:283, 425), c(gl(425, 283)), 
                     img)
colnames(img.df) <- c("row", "column", RGB, "alpha")

str(cmp1.2d[1])
ma <-  matrix(cmp1.2d[1], nrow = 283, ncol =425)

getwd()
setwd('C:\\Users\\SONY\\Desktop\\SCR\\Week 3')
matrix(0 ,3, 3, rownames = RGB)

head(matrix(unlist(ma), 283, 425))


img.km.cen <- matrix(img.kmeans$centers, ncol = 3)
colnames(img.km.cen) <- RGB
img.km.cls <- img.kmeans$cluster
cmp1.2d <- data.frame(img.km.cen[c(img.km.cls), ])

img.df[sample(1:nrow(img.df),3, replace = F), ]
?sample

centroids <- InitCentroids(X = img.df[1:15, RGB], k = 16)
M <- matrix(unlist(rep(img.df[1, RGB],16)), ncol = 3, nrow = 16, byrow = T)
Z <- which.min(rowSums((M - centroids)^2))


k <- nrow(centroids)
M <- img.df[1, RGB]
Z <- which.min(rowSums((M - centroids)^2))
C <- matrix(unlist(rep(centroids, nrow(img.df))), ncol = 3)

find.index <- function(X, k, centro){
  M <- matrix(unlist(rep(X,nrow(centro))), ncol = 3, byrow = T)
  Z <- which.min(rowSums((M - centro)^2))
  return(Z)
}
head(img.df[, RGB] - c(centroids))
find.index(img.df[1:3, RGB], centro = centroids)

colMeans(img.df[which(clst.nr == i),RGB])
ss<-split(img.df[, RGB], clst.nr)
matrix(unlist(lapply(ss, colMeans)), ncol = 3, byrow = T)
?split


SiOfClosestMu <- function(X, centroids) {
  # X: data.frame
  # centroids: K * 3 matrix
  
  # Returns: integer vector with cluster numbers
  
  Ind <- apply(X, 1, 
               function(X, centro){
                 M <- matrix(unlist(rep(X,nrow(centro))), ncol = 3, byrow = T)
                 Z <- which.min(rowSums((M - centro)^2))
                 return(Z)}, centro = centroids)
  return(Ind)
}



rowSums(M %% centroids)



SiOfClosestMu <- function(X, centroids) {
  # X: data.frame
  # centroids: K * 3 matrix
  
  # Returns: integer vector with cluster numbers
  
  Ind <- apply(X, 1, 
               function(X, centro){
                 M <- matrix(unlist(rep(X,nrow(centro))), ncol = 3, byrow = T)
                 Z <- which.min(rowSums(M^2 + centroids^2 - 2*M*centroids))
                 return(Z)}, centro = centroids)
  return(Ind)
}
system.time(clst.nr <- SiOfClosestMu(X = img.df[, RGB], centroids))
M
0.02352941^2

sqrt(rowSums((M -centroids)^2))
sqrt(rowSums(M^2 + centroids^2 - 2*M*centroids))

img.df[1:32, RGB] - centroids

system.time(computeCentroids(X = img.df[, RGB], centroids, clst.nr))


centroids <- InitCentroids(img.df[,RGB], 16)
X <- img.df[,RGB]
for(i in 1:10){
  clst.nr <- SiOfClosestMu(img.df[,RGB], centroids)
  centroids.new <- computeCentroids(X, centroids, clst.nr)
  diff <- sum(sqrt(rowSums((X - centroids)^2)))/length(X) - sum(sqrt(rowSums((X - centroids.new)^2)))/length(X)
  if (diff <= (.Machine$double.eps)^(1/3))
    break
  return(centroids.new)
  else
    centroids.new <- centroids
}

T <- c(table(clst.nr))
class(T)
A <- matrix(rep(centroids, ,T), ncol = 3)
head(A, 30)
system.time(matrix(rep(c(centroids),T), ncol = 3))
?rep


vect1 <- c(4, 5, 10, 3, 1)
rep(c(5,3,2),c(5,1,1))
N <- 0
Cc<-centroids[rep(1:16, c(T)),]
SSS<-matrix(unlist(S), ncol = 3)
sum((SSS-Cc)^2)/120275

class(c(centroids))
str(c(centroids))
SS<-apply(centroids, 1,rep, c(1:16))
length(rep(1:16, c(T)))

xxx<-centroids.new[rep(1:16, c(T)),]
SSS<-matrix(unlist(S), ncol = 3)
sum((SSS-xxx)^2)/120275




M <- matrix(unlist(rep(,16)), ncol = 3, byrow = T)
Z <- apply((sum((M - centroids)^2)),1,which.min)

I <- img.df[,RGB]
system.time(which.min(rowSums((matrix(I[1,])- centroids)^2)))
class(centroids)

B <- matrix(img.df[, RGB], ncol = 3)
class(as.matrix(centroids))

computeCentroids <- function(X, centroids, clst.nr) {
  # X: data.frame
  # centroids: K by ncol(X) matrix
  # clst.nrs: integer vector of length ncol(X)
  S <- split(X, clst.nr)
  C <- as.matrix(lapply(S, colMeans))
  C
}

system.time()

matrix(1, 2,3) - c(1,0,0)
sweep(matrix(1,2,3), 2, c(1,0,0))
?sweep()
head()
XXX <- split(img.df[,RGB], clst.nr)
lapply(XXX, function(x) sum((x- centroids)^2))
table(clst.nr)
c <- as.matrix(centroids.1)[clst.nr, ]
sum((img.df[, RGB]-c)^2)/120275
length(img.df)
dim(img.df)[1]

cmp22 <- array(1, dim = dim(img))
for (i in 1:3) {
  cmp22[, , i] <- matrix(unlist(Yi[,i]), nrow = 283, ncol = 425)
}
head(cmp22)
head(cmp1.2d)

table(Yi[, 1])
class(Yi)

cov(centroids)
dist(rbind(centroids[1,],centroids[2,]))
sum((centroids[1,] - centroids[2,])^2)

unique(matrix(1,3,3))
sample(A, 2)
which(any(sort(unique(clst.nr))) != c(1:16))

a <- c(1:16)
index <- a[-sort(unique(Ind))]

Ind <- c(Ind,index)


Ind <- apply(as.matrix(img.df), 1, 
             function(x)
               which.min(rowSums((x - as.matrix(centroids))^2)))
Ind <- apply(as.matrix(X), 1, 
             function(x)
               which.min(rowSums((x - as.matrix(centroids))^2)))

test<- data.frame(x = c(1:10, 1:3), y=1:13)

test[!duplicated(test$x), ]


if (k1 < k2)
  a <- 1:16
index <- a[-c(sort(unique(clst.nr)))]
centroids.new <- rbind(centroids.1,as.matrix(centroids[index, ]))

dataf <- as.data.frame(img.df[,RGB])
head(dataf)
if (k1 < k2) 
  k1 <- nrow(centroids)
k2 <- unique(clst.nr)
a <- c(1:k1)
S <- split(X, clst.nr)
c.missing <- a[-c(sort(k2))]
C <- matrix(unlist(lapply(S, colMeans)), ncol = ncol(X), byrow = T)
centroids.new <- rbind(centroids.1,centroids[c(index), ])



k1 <- nrow(centroids)
k2 <- length(unique(clst.nr))
a <- c(1:k1)
S <- split(X, clst.nr)
centroids.new <- matrix(unlist(lapply(S, colMeans)), 
                        ncol = ncol(X), byrow = T)
if (k1 < k2) 
  centroids.new <- rbind(centroids.new,
                         as.matrix
                         (centroids[a[-sort(unique(clst.nr))], ]))

centroids.new


dim(as.matrix(centroids))
head(img.df)
c(t(as.matrix(img.df[,RGB])))[1:6]


InitCentroids <- function(X, k) {
  # X = data.frame 
  # K = number of centroids
  # N = data.frame with sampling result
  #
  # Returns: an k by ncol(X) dimensional matrix
  x <- unique(X)
    N <- x[sample(1:nrow(x), k, replace = F), ] 
    rownames(N) <- 1:k
    return(N)
}

tt <- img.df[1:3,RGB]
aa <- img.df[11:13, RGB]
sqrt(apply(((tt-aa)^2),1,sum))
apply(as.matrix(abs(tt-aa),byrow = T, ncol = 3),1,sum)

k1 <- nrow(centroids)
k2 <- unique(clst.nr)
a <- c(1:k1)
S <- split(X, clst.nr)
centroids.new <- matrix(unlist(lapply(S, colMeans)), ncol = ncol(X), byrow = T)