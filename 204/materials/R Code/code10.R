Trees = trees
names(Trees)[1] = "Diam"
attach(Trees)

pairs(Trees,pch=19)

cor(Trees)
M1 = lm(Volume ~ Diam)
print(M1)

plot(Diam, Volume) #response vs predictor
abline(M1$coef) #add fitted line

new = data.frame(Diam=16)
predict(M1, new)

plot(M1, which=1:2)


M2 = lm(Volume ~ Diam + Height)
summary(M2)
print(M2)

#pdf(file='plot.pdf',pointsize = 18,height=10,width=10)
par(mfrow=c(2,1))
plot(M2, which=1:2,pch=19)
#dev.off()

M3 = lm(Volume ~ Diam + I(Diam^2) + Height)
print(M3)

par(mfrow=c(2,1))
plot(M3, which=1:2,pch=19)

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

CPUspeed = read.table("CPUspeed.txt", header=TRUE)

head(CPUspeed)

years = CPUspeed$time - 1994
speed = CPUspeed$speed
log2speed = CPUspeed$log10speed / log10(2)

par(mfrow=c(2,1))
plot(years, speed,pch=19,main="Time vs. Speed")
plot(years, log2speed,pch=19, main="Time vs. Log Speed")
L = lm(log2speed ~ years)
print(L)

par(mfrow=c(1,2))
plot(years, speed,pch=19,main="Speed vs. Years")
curve(2^(-3.6581 + 0.5637 * x), add=TRUE)
plot(years, log2speed,pch=19,main="Log2-Speed vs Years")
abline(L)


par(mfrow=c(1,2))
plot(L, which=1,pch=19)
plot(L, which=2,pch=19)

############### swiss data #####################

head(swiss)
attach(swiss)
swiss_model=lm(Fertility~1)
add1(swiss_model,.~Agriculture+Examination+Education+Catholic+Infant.Mortality)

swiss_model=lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality)
drop1(swiss_model)
step(swiss_model,test="F")
swiss_model_2=lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality)

