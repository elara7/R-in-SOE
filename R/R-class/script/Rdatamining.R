#*****  PCA主成份分析*******#

prcomp(USArrests, scale = TRUE) #scaling is needed
##########################
#Standard deviations: 每一个主成份的载荷
#        [1] 1.5748783 0.9948694 0.5971291
#[4] 0.4164494

#Rotation: PC表示主成份，各个主成份如何由原变量线性变化
#        PC1        PC2
#Murder   -0.5358995  0.4181809
#Assault  -0.5831836  0.1879856
#UrbanPop -0.2781909 -0.8728062
#Rape     -0.5434321 -0.1673186
#PC3         PC4
#Murder   -0.3412327  0.64922780
#Assault  -0.2681484 -0.74340748
#UrbanPop -0.3780158  0.13387773
#Rape      0.8177779  0.08902432
##########################
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
#Standard deviations:返回以murder assault rape进行的主成份分析结果，3个主成份
#        [1] 1.5357670 0.6767949 0.4282154
#
#Rotation:
#        PC1        PC2
#Murder  -0.5826006  0.5339532
#Assault -0.6079818  0.2140236
#Rape    -0.5393836 -0.8179779
#PC3
#Murder  -0.6127565
#Assault  0.7645600
#Rape    -0.1999436
windows(7,7)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))


#******************************************************************

# install.packages("HSAUR")
#载入数据
data("heptathlon", package = "HSAUR")
#数据处理：修改成最大值距离
# score all the seven events in the same direction
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

heptathlon_pca <- prcomp(heptathlon[, -8], scale = TRUE)
print(heptathlon_pca)
summary(heptathlon_pca)
plot(heptathlon_pca)
windows(7,7)


#******************************************************************


#*****CCA典型相关分析******#


#******MDS********#
#已经知道两两距离，把点还原到坐标上
require(graphics)
windows(7,7)
loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top
plot(x, y, type = "p", pch = 20, xlab = "", ylab = "", asp = 1, axes = TRUE,
     main = "cmdscale(eurodist)", col = "lightblue")
text(x, y, rownames(loc), cex = 0.6)



