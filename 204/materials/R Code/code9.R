attach(cars) #attach the data
pdf(file="plot.pdf",pointsize=16)
plot(cars, pch=19,ylab="Distance",xlab="Speed") #construct scatterplot
dev.off()

cars #display the help page for cars data
plot(cars, pch=19,ylab="Distance",xlab="Speed") #construct scatterplot


L1 = lm(dist ~ speed)
print(L1)

pdf(file="plot.pdf",pointsize=16)
plot(cars, main="dist = -17.579 + 3.932 speed",pch=19, xlim=c(0, 25))
#line with intercept=-17.579, slope=3.932
abline(-17.579, 3.932)
curve(-17.579 + 3.932*x, add=TRUE) #same thing
dev.off()

pdf(file="plot.pdf",pointsize=16)
plot(L1, which=1, add.smooth=FALSE)
dev.off()

L2 = lm(dist ~ 0 + speed)
#same as L2=lm(dist ~ speed -1)
L2

pdf(file="plot.pdf",pointsize=16)
plot(cars, main="dist = 2.909 speed",pch=19, xlim=c(0,25))
#line with intercept=0, slope=2.909
abline(0, L2$coeff[1])
dev.off()

#LSEs
SSxx=sum((speed-mean(speed))^2)
SSyy=sum((dist-mean(dist))^2)
SSxy=sum((speed-mean(speed))*(dist-mean(dist)))
beta1_hat=SSxy/SSxx
beta0_hat=mean(dist)-beta1_hat*mean(speed)
beta0_hat
beta1_hat

#Standard error
SSE=sum((L1$residuals)^2)
sigma2_hat=SSE/(length(dist)-2)
sigma_hat=sqrt(sigma2_hat)
sigma_hat

#t-test for slope
t_statistic=beta1_hat/(sigma_hat/sqrt(SSxx))
t_statistic

#R^2
R2=1-(SSE/SSyy)
R2

#prediction x=c(7,12)
new=data.frame(speed=c(7,12))
ynew_hat=L1$coef[1]+L1$coef[2]*new
ynew_hat
bound=qt(0.975,df=48)*sigma_hat*sqrt(1/50+(new-mean(speed))^2/SSxx)

low_ci=ynew_hat-bound
up_ci=ynew_hat+bound

low_ci
up_ci

predict(L1,new,interval="prediction")
bound_predict=qt(0.975,df=48)*sigma_hat*sqrt(1+1/50+(new-mean(speed))^2/SSxx)
ynew_hat-bound_predict
ynew_hat+bound_predict

ggplot(cars,aes(x=speed,y=dist)) + theme_bw()+
geom_point() +
geom_smooth(method="lm") 

#####################################
Trees = trees
names(Trees)[1] = "Diam"
attach(Trees)

pairs(Trees)
cor(Trees)

M1 = lm(Volume ~ Diam)
print(M1)

plot(Diam, Volume) #response vs predictor
abline(M1$coef) #add fitted line

new = data.frame(Diam=16)
predict(M1, new)

plot(M1, which=1:2)

M2 = lm(Volume ~ Diam + Height)
print(M2)

plot(M2, which=1:2)

M3 = lm(Volume ~ Diam + I(Diam^2) + Height)
print(M3)

plot(M3, which=1:2)

summary(M3)

anova(M3)

anova(M1, M2, M3)

new = data.frame(Diam=16, Height=70)
predict(M3, newdata=new)

predict(M3, newdata=new, interval="pred")

diameter = 16
height = seq(65, 70, 1)
new = data.frame(Diam=diameter, Height=height)
predict(M3, newdata=new, interval="conf")

lunatics = read.table("lunatics.txt", header=TRUE)
attach(lunatics)

plot(DIST, PHOME)
cor(DIST, PHOME)

RDIST = 1/DIST
plot(RDIST, PHOME)
cor(RDIST, PHOME)

M = lm(PHOME ~ RDIST)
M

abline(M)

plot(DIST, PHOME)
curve(M$coef[1] + M$coef[2] / x, add=TRUE)

plot(M$fitted, M$resid, xlab="fitted", ylab="residuals")

abline(h=0, lty=2)

lab = abbreviate(COUNTY)
identify(M$fitted.values, M$residuals, n=1, labels=lab)

lunatics[13, ]

detach(lunatics)

CPUspeed = read.table("CPUspeed.txt", header=TRUE)

head(CPUspeed)

years = CPUspeed$time - 1994
speed = CPUspeed$speed
log2speed = CPUspeed$log10speed / log10(2)

plot(years, speed)
plot(years, log2speed)

L = lm(log2speed ~ years)
print(L)

plot(years, speed)
curve(2^(-3.6581 + 0.5637 * x), add=TRUE)

plot(years, log2speed)
abline(L)

plot(L, which=1:2)

CPUspeed[c(16, 26, 27), ]

summary(L)$r.squared

new = data.frame(years = 2005 + 316.5 / 365 - 1994)
lyhat = predict(L, newdata=new)
lyhat

2^lyhat
