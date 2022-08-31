flicker = read.table(file="http://www.statsci.org/data/general/flicker.txt",
                     header=TRUE,stringAsFactors=TRUE)

is.factor(flicker$Colour)
levels(flicker$Colour)

unclass(flicker$Colour)

attach(flicker)
colors_flicker=c('blue','brown','green')

#pdf(file='plot.pdf',pointsize = 18,height=10,width=10)
boxplot(Flicker ~ Colour, ylab = "Flicker",col=colors_flicker)
#dev.off()

stripchart(Flicker ~ Colour, vertical=TRUE, col=colors_flicker,pch=19)

by(Flicker, Colour, FUN=mean)

meansd = function(x) c(mean=mean(x), sd=sd(x))

by(Flicker, Colour, FUN=meansd)

oneway.test(Flicker ~ Colour)

oneway.test(Flicker ~ Colour, var.equal=TRUE)

L = lm(Flicker ~ Colour)
L

predict(L)

M = aov(Flicker ~ Colour)
model.tables(M, type="means")
model.tables(M)

L1=lm(Flicker ~ Colour - 1)
summary(L1)


options(show.signif.stars=FALSE)
anova(L1)

#plot residuals vs fits
par(mfrow=c(1,2))
plot(L$fit, L$res,pch=19,xlab="fitted values",ylab="residuals",
     main="Residuals vs Fitted Values",ylim=c(-3,3))
abline(h=0,lty=2) #add horizontal line through 0
qqnorm(L$res,pch=19)
qqline(L$res,lty=2)
#Normal-QQ plot of residuals with reference line


MSE = 2.3944
t97.5 = qt(.975, df=16) #97.5th percentile of t
n = table(Colour) #sample sizes
means = by(Flicker, Colour, mean) #treatment means
outer(means, means, "-")

t97.5 * sqrt(MSE * outer(1/n, 1/n, "+"))
pairwise.t.test(Flicker, Colour)

qtukey(.95, nmeans=3, df=16)
summary(M)
TukeyHSD(M)

plot(TukeyHSD(M))

#################### randomized block design

library(bootstrap)
head(scor)

sapply(scor, mean, data=scor)

scor.long = stack(scor)
block = factor(rep(1:88, times=5))
scor.long = data.frame(scor.long, block)

head(scor.long) #top
tail(scor.long) #bottom

names(scor.long) = c("score", "exam", "student")

str(scor.long)

L = aov(score ~ exam + student, data=scor.long)
summary(L)

model.tables(L, cterms="exam")
model.tables(L, cterms="exam", type="mean")

CIs = TukeyHSD(L, which=1)
CIs

par(mfrow=c(1,1))
plot(CIs, las=1)

par(mfrow=c(1,2))
plot(L, which=1:2,pch=19)

boxplot(score ~ student,
 xlab="Student Number", ylab="Score", data=scor.long)

######### 2 way anova
poison = read.csv("poison.csv")

L = aov(Time ~ Poison * Treatment, data = poison)
anova(L)

par(mfrow=c(1,1))
with(data=poison, expr={
#  interaction.plot(Poison, Treatment, response=Time,lwd=2)
  interaction.plot(Treatment, Poison, response=Time,lwd=2)
 })


model.tables(L, type="means")

TukeyHSD(L1, which=c("Poison", "Treatment"))

par(mfrow=c(1,2))
plot(TukeyHSD(L1, which=c("Poison", "Treatment")))
dev.off()

par(mfrow=c(1,2))
plot(model_L1,which=1:2)

model_log=lm(log(Time) ~ Poison + Treatment, data=poison)
anova(aov(model_log))
par(mfrow=c(1,2))
plot(model_log,which=1:2)

model_reciprocal=lm(1/Time ~ Poison + Treatment, data=poison)
anova(aov(model_reciprocal))
par(mfrow=c(1,2))
plot(model_reciprocal,which=1:2)

