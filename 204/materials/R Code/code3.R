library(MASS)
data()
?mammals 
head(mammals)
summary(mammals)

#png(filename='mammals_1.png')
par(mfrow=c(1,2))
boxplot(mammals,col=c('green','blue'))
plot(mammals,pch=19)
#dev.off()

summary(log(mammals))

#png(filename='log_mammals.png')
par(mfrow=c(1,2))
boxplot(log(mammals),names=c("log(body)", "log(brain)"),col=c('green','blue'))
plot(log(mammals$body),log(mammals$brain),pch=19,ylab="log(brain)",xlab="log(body)")
#dev.off()

# Correlation

cor(mammals)
cor(log(mammals))

#########################
## Analysis of bivariate data by group
#########################

twins = read.table("twinIQ.txt", header=TRUE)

head(twins)

summary(twins)
#png(filename='twins_boxplot.png')
par(mfrow=c(1,1))
boxplot(Foster - Biological ~ Social, twins)
#dev.off()

attach(twins)
status = as.integer(Social)
status

#png(filename='twins_scatter.png')
plot(Foster ~ Biological, data=twins, pch=status,col=status)
legend("topleft", c("high","low","middle"), pch=1:3, col=1:3,inset=.02)
abline(0,1)
#dev.off()



# conditional plots 
#png(filename='twins_conditional_1.png')
coplot(Foster ~ Biological|Social, data=twins)
#dev.off()
library(lattice)
#png(filename='twins_conditional_2.png')
xyplot(Foster ~ Biological|Social, data=twins, pch=20,  layout=c(2,2))
#dev.off()

brain = read.table("brainsize.txt", header=TRUE)
summary(brain)

mean(brain$Weight)
mean(brain$Weight, na.rm=TRUE)
by(data=brain[, 2], INDICES=brain$Gender, FUN=mean, na.rm=TRUE)

attach(brain)
gender = as.integer(Gender) #need integer for plot symbol, color
#png(filename='brain_1.png')
plot(Weight, MRI_Count, pch=gender, col=gender)
legend("topleft", c("Female", "Male"), pch=1:2, col=1:2, inset=.02)
#dev.off()

#png(filename='brain_2.png')
pairs(brain[, 2:7])
#dev.off()

round(cor(brain[, 2:7]), 2)
round(cor(brain[, 2:7], use="pairwise.complete.obs"), 2)

mri = MRI_Count / Weight
cor(FSIQ, mri, use="pairwise.complete.obs")

# Missing data... 
which(is.na(brain), arr.ind=TRUE)
brain[c(2, 21), ]

brain[2, 5] = mean(brain$Weight, na.rm=TRUE)
brain[21, 5:6] = c(mean(brain$Weight, na.rm=TRUE),
  mean(brain$Height, na.rm=TRUE))

brain[c(2, 22), ]

####### Time series data....

nhtemp
#png(file='temperatures.png')
plot(nhtemp,ylab="Average Yearly Temp, New Haven")
points(nhtemp,pch=19)
lines(lowess(nhtemp),lwd=3,col='red')
#dev.off()

d=diff(nhtemp)
#png(file='temperatures_2.png')
plot(d,ylab="first differences of mean annual temperature")
points(d,pch=19)
abline(h=0,lty=3,lwd=2,col='blue')
lines(lowess(d),lwd=3,col='red')
#dev.off()

#### Sample means and the central limit theorem
?randu
apply(randu,2,mean)
apply(randu,2,var)
cor(randu)

#png(file='randu_cloudplot.png')
library(lattice)
cloud(z~x+y,data=randu)
#dev.off()

means=apply(randu,1,mean)
#png(file='randu_hist_means.png')
hist(means,col='lightblue')
#dev.off()

#png(file='randu_hist_means_density.png')
truehist(means,col='lightblue')
curve(dnorm(x,1/2,sd=sqrt(1/36)),add=TRUE)
#dev.off()

#png(file='randu_qqplot.png')
qqnorm(means)
qqline(means)
#dev.off()

