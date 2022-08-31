# Additional examples
library("BHH2") #functions & data sets reproducing examples in Box, Hunter & Hunter 
data(shoes.data)
shoes.data
attach(shoes.data)
t.test(matA,mu=10)
# F-test for variances:
var.test(matA,matB)
var(matA)
var(matB)
t.test(matA,matB,var.equal = T)
t.test(matA,matB,var.equal = F)
t.test(matA,matB,paired=T)


#Density estimation
attach(faithful)
hist(eruptions,breaks=15,xlim=c(0.5,6),col='lightblue',prob=T)
lines(density(eruptions))
dev.copy2pdf(file="faithful_eruptions.pdf")

library(MASS)
attach(geyser)
par(mfrow=c(2,2))
plot(duration, waiting,xlab="previous (mins)",ylim=c(40,110),xlim=c(0.5,6),
     ylab="waiting (mins)",pch=19)
f1=kde2d(duration,waiting,n=50,lims=c(0.5,6,40,110))
image(f1,xlab="previous (mins)",ylab="waiting")
#points(duration,waiting,cex=0.8)
f2=kde2d(duration,waiting,n=50,lims=c(0.5,6,40,110),h=c(width.SJ(duration),width.SJ(waiting)))
image(f2,xlab="previous (mins)",ylab="waiting")
#points(duration,waiting,cex=0.8)
persp(f2,phi=15,theta=20,d=10,xlab="previous duration",ylab="waiting",zlab="")
dev.copy2pdf(file="faithful_2_density.pdf")

