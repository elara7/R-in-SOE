data=library(datasets)
windows(7,7)
attach(airquality)
a5 <- subset(airquality,Month==5)
ao <- subset(airquality,Month!=5)
plot(ao$Wind, ao$Ozone,xlab="Wind", ylab="Ozone",col="red",
     xlim=c(0, 23), ylim=c(0, 170))
points(a5$Wind, a5$Ozone,col="blue")
title("15420151152805王泽贤")
abline(lm(Ozone ~ Wind),lwd=2)
legend("topright",c("May","Other Months"),pch = c(21,21),col=c("blue","red"))
detach(airquality)



windows(7,7)
attach(airquality)
par(fig=c(0,1/3,0,0.9))
plot(Wind, Ozone,xlab="Wind", ylab="Ozone",xlim=c(0, 23), ylim=c(0, 170),main = "Ozone and Wind")
par(fig=c(1/3,2/3,0,0.9),new=TRUE)
plot(Solar.R, Ozone,xlab="Solar.R", ylab="Ozone",main = "Ozone and Solar Radiation")
par(fig=c(2/3,1,0,0.9),new=TRUE)
plot(Temp, Ozone,xlab="Temp", ylab="Ozone",main = "Ozone and Temperature")
mtext("15420151152805王泽贤", side = 3, outer = TRUE, line = -3)
detach(airquality)

setwd("C:\\Users\\44180\\Google 云端硬盘\\soe\\R\\ch7")
polution <- read.csv("avgpm25.csv")
windows(7,7)
#####
par(fig=c(0,0.5,0.45,0.9))
aw <- subset(polution,region=="west")
ae <- subset(polution,region!="west")
plot(aw$latitude, aw$pm25,xlab="latitude", ylab="pm25",col="red")
points(ae$latitude,ae$pm25)
abline(h=12, lty=2,lwd=2)
####
par(fig=c(0.5,1,0.45,0.9),new=TRUE)
hist(polution$pm25,col="green",xlab = "polution$pm25")
abline(v=10, lty=1,lwd=3,col="#FF00FF")
abline(v=12, lty=1,lwd=2)
####
par(fig=c(0,0.5,0,0.45),new=TRUE)
boxplot(polution$pm25~polution$region,col="red")
####
par(fig=c(0.5,1,0,0.45),new=TRUE)
library(plotrix)
pwest <- sum(polution$pm25[polution$region=="west"])
peast <- sum(polution$pm25[polution$region=="east"])
piedata <- c(peast,pwest)
piename <- c("east","west")
pie3D(piedata, labels = piename, explode = 0.3,main="Number of Counties in Each Region")
####
mtext("15420151152805王泽贤", side = 3, outer = TRUE, line = -3)
