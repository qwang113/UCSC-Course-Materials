dat = read.table("college.txt", header=TRUE, sep="\t")
college = subset(dat, complete.cases(dat))
head(college)
attach(college)

stripchart(Retention, method="stack", pch=19, xlab="Retention Percentage")
#dev.copy2pdf(file='retention.pdf',width=8,height=6)

stripchart(Retention ~ Tier, method="stack", pch=19,xlab="Retention Percentage",
ylab="Tier", xlim=c(50, 100), data=college)
#dev.copy2pdf(file='retention_tier.pdf',width=8,height=6)

identify(Retention, Tier, n=2, labels=School)

b.output = boxplot(Retention ~ Tier, data=college, horizontal=TRUE, ylab="Tier", xlab="Retention")
#dev.copy2pdf(file='retention_tier_boxplots.pdf',width=8,height=6)

b.output$stats

b.output$out

b.output$group

plot(Retention, Grad.rate,xlab="Retention", ylab="Graduation Rate")
#dev.copy2pdf(file='graduationrate_retention.pdf',width=6,height=6)

plot(Retention, Grad.rate,xlab="Retention", ylab="Graduation Rate")
fit = line(Retention, Grad.rate)
coef(fit)
plot(Retention, Grad.rate,xlab="Retention", ylab="Graduation Rate")
#dev.copy2pdf(file='graduationrate_retention.pdf',width=6,height=6)

plot(Retention, Grad.rate,xlab="Retention", ylab="Graduation Rate")
fit = line(Retention, Grad.rate)
coef(fit)
abline(coef(fit),col='red')
coef(lm(Grad.rate~Retention))
abline(lm(Grad.rate~Retention),col='blue')
#dev.copy2pdf(file='graduationrate_retention_lines.pdf',width=6,height=6)


plot(Retention, fit$residuals, xlab="Retention", ylab="Residual")
abline(h=0)
identify(Retention, fit$residuals, n=2, labels=college$School)
#dev.copy2pdf(file='graduationrate_retention_residuals.pdf',width=6,height=6)

detach(college)
################## Reexpression 
bgsu = read.table("bgsu.txt", header=TRUE, sep="\t")
attach(bgsu)

plot(Year, Enrollment, pch=19)
#dev.copy2pdf(file='bgsu.pdf',width=6,height=6)

par(mfrow=c(1,2))
fit = lm(Enrollment ~ Year, data=bgsu)
plot(Year, Enrollment, pch=19)
abline(fit)
plot(Year, fit$residuals,xlab="Year",ylab="Residuals",pch=19)
abline(h=0)
#dev.copy2pdf(file='bgsu_residuals.pdf',width=8,height=6)

bgsu$log.Enrollment = log(bgsu$Enrollment)
attach(bgsu)


par(mfrow=c(1,2))
plot(Year, log.Enrollment, ylab="Log(Enrollment)",pch=19)
fit2 = lm(log.Enrollment ~ Year, data=bgsu)
fit2$coef
abline(fit2)
plot(Year, fit2$residuals,ylab="Residuals",pch=19)
abline(h=0)
#dev.copy2pdf(file='log_bgsu_residuals.pdf',width=8,height=6)

