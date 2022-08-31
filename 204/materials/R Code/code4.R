tosses=scan(what="character")
table(tosses)
#png(file='barplot_tosses.png')
barplot(table(tosses))
#dev.off()

as.factor(tosses)

accidents=c(20,20,22,22,29,36,31)
sum(accidents)
expected_accidents=rep(180/7,7)
days=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

names(accidents)=days
names(expected_accidents)=days
#png(file='accidents_barplot.png')
barplot(cbind(accidents,expected_accidents),beside=TRUE)
#dev.off()

chi_statistic=sum((accidents-expected_accidents)^2/expected_accidents)
chi_statistic # Compare with chi-square with k-1 degrees of freedom 
1-pchisq(chi_statistic,6) # p-value 
# R function for chi-square test:
chisq.test(accidents)
# Compare with multinomial experiment...
multinomial_experiment=rmultinom(1000000,size=180,prob=rep(1/7,7))

##################### ANALYSIS OF TWINS DATA #############
twn=read.table("twins.dat.txt",header=TRUE,sep=",",na.strings=".")
head(twn)
table(twn$EDUCL)
table(twn$EDUCH)

c.EDUCL = cut(twn$EDUCL, breaks=c(0, 12, 15, 16, 24),
  labels=c("High School", "Some College", "College Degree",
  "Graduate School"))
c.EDUCH = cut(twn$EDUCH, breaks=c(0, 12, 15, 16, 24),
  labels=c("High School", "Some College", "College Degree",
  "Graduate School"))

table(c.EDUCL)

prop.table(table(c.EDUCL))

#png(file="barplot_EUCL.png")
barplot(prop.table(table(c.EDUCL)))
#dev.off()

#png(file="mosaic_EUCL.png")
mosaicplot(table(c.EDUCL))
#dev.off()

table(c.EDUCL, c.EDUCH)

T1=table(c.EDUCL, c.EDUCH)
diag(T1)

sum(diag(T1)) / sum(T1)

#png(file="mosaic_both.png",width=480,height=380)
mosaicplot(T1,color=1:4,las=1,main="",xlab="Education Twin 1",ylab="Education Twin 2")
#dev.off()

mosaicplot(T1,color=1:4,las=1,main="",xlab="Education Twin 1",ylab="Education Twin 2")
#dev.copy2pdf(file="mosaic_both.pdf",width=8,height=6)

hist(twn$HRWAGEL, main="Salary for Twin 1",xlab="Hourly wage (dollars)",
col="lightblue")
#dev.copy2pdf(file="hist_wage.pdf",width=6,height=6)

c.wage = cut(twn$HRWAGEL, c(0, 7, 13, 20, 150))

table(c.wage)

table(c.EDUCL, c.wage)

T2 = table(c.EDUCL, c.wage)

prop.table(T2, margin=1)

P = prop.table(T2, 1)
barplot(t(P), ylim=c(0, 1.5), ylab="PROPORTION",
        legend.text=dimnames(P)$c.wage,
        args.legend=list(x = "topleft",cex=0.5))
#dev.copy2pdf(file="contingency_barplot_wage.pdf",width=6,height=6)

barplot(t(P), beside=T,ylab="PROPORTION",ylim=c(0,1),
        legend.text=dimnames(P)$c.wage,
        args.legend=list(x = "top",cex=0.9))
dev.copy2pdf(file="contingency_barplotside_wage.pdf",width=6,height=6)

# We could also do
barplot(P, beside=T, legend.text=dimnames(P)$c.EDUCL, ylim=c(0,1),
        args.legend=list(x="topleft",cex=0.8), ylab="PROPORTION")


T2 = table(c.EDUCL, c.wage)
T2
S = chisq.test(T2)
print(S)

#
A=matrix(rep(rowSums(T2),4),4,4,byrow=T)
B=matrix(rep(colSums(T2),4),4,4)
Expected=t(A*B/sum(T2))
Expected
S$expected

sum((T2 - S$expected)^2 / S$expected)

1 - pchisq(54.57759, df=9)

names(S)

S$residuals

mosaicplot(T2, shade=FALSE,main="Twin 1: Educational level & wage",las=1,
xlab="Education Level",ylab="Wage")
#dev.copy2pdf(file="education_wage_mosaic.pdf",width=6,height=6)

mosaicplot(T2, shade=TRUE,main="Twin 1: Educational level & wage",las=1,
xlab="Education Level",ylab="Wage")

