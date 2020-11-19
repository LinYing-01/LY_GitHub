getwd()
#1.1Boxplot
pacman::p_load(ggplot2)
head(ToothGrowth)
ToothGrowth$dose<-as.factor(ToothGrowth$dose)
ggboxplot(ToothGrowth, x="dose", y="len", fill="dose", palette = "npg", 
          add="point")
          
#1.2
library(astsa)
plot(soi)
t <- decompose(soi)
plot(t)
acf(soi)
pacf(soi)
ts_new <- t$random[!is.na(t$random)]
ts_new <- ts(ts_new, start=c(1970,10),frequency=20)
ts_new_d1 <- diff(ts_new)
plot(ts_new_d1)

#1.3
#1.3.1
qplot(carat,data=diamonds,geom=c("histogram"))
#1.3.2
Height<-c(144,166,163,143,152,169,130,159,160,175,161,170,146,159,150,183,165,146,169)
hist(Height,col="lightblue",border="red",labels=TRUE,ylim=c(0,7.2))
hist(Height,breaks=12,freq=FALSE,density=10,angle=60)

#1.4
attach(mtcars)
plot(wt,mpg,
     main='Scatter plot',
     xlab='Weight',
     ylab='Miles Per Gallon',
     pch=25)
abline(lm(mpg~wt),col='purple',lwd=2,lty=1)
lines(lowess(wt,mpg),col='red',lwd=2,lty=2)

#1.5
install.packages("fields")
install.packages("maps")
install.packages("RNetCDF")
library(fields)
library(maps)
library(RNetCDF)
ex.nc     <- open.nc("air.mon.ltm.nc")
print.nc(ex.nc)
Lat       <- var.get.nc(ex.nc, "lat")
Lon       <- var.get.nc(ex.nc, "lon")
Air_T     <- var.get.nc(ex.nc, "air") 
close.nc(ex.nc)
Lat <- rev(Lat)
Air_T_Jan <- Air_T[,,1]
Air_T_Jan <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  Air_T_Jan[,row] <- Air_T[, (length(Lat)+1-row),1 ]
}
image.plot(Lon, Lat, Air_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [k]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("JAN"),
      cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="purple")
box(lwd=5)