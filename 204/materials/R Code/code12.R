library(ISwR)
head(hellung)
hellung$glucose=factor(hellung$glucose,labels=c("Yes","No"))
attach(hellung)
#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
par(mfrow=c(1,2))
plot(conc,diameter,pch=(as.double(glucose)+17),col=(as.double(glucose)+1),lwd=2)
boxplot(diameter~glucose,data=hellung,col=c(2,3),ylab="diameter",
        xlab="glucose")
#dev.off()

#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
par(mfrow=c(1,1))
plot(log(conc),(diameter),pch=(as.double(glucose)+17),col=(as.double(glucose)+1),lwd=2)
#dev.off()

#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
par(mfrow=c(1,1))
plot(log(conc),log(diameter),pch=(as.double(glucose)+17),col=(as.double(glucose)+1),
     lwd=2)
#dev.off()

L1=lm(log(diameter)~log(conc)*glucose)
summary(L1)
#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
par(mfrow=c(1,1))
plot(log(conc),log(diameter),pch=(as.double(glucose)+17),
     col=(as.double(glucose)+1),lwd=2)
abline(a=L1$coeff[1],b=L1$coeff[2],lty=1,lwd=2,col=2)
abline(a=L1$coeff[1]+L1$coeff[3],b=L1$coeff[2]+L1$coeff[4],
       lwd=2,col=3)
#dev.off()

L2=lm(log(diameter)~log(conc)+glucose)
summary(L2)
#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
par(mfrow=c(1,1))
plot(log(conc),log(diameter),pch=(as.double(glucose)+17),
     col=(as.double(glucose)+1),lwd=2)
abline(a=L2$coeff[1],b=L2$coeff[2],lty=1,lwd=2,col=2)
abline(a=L2$coeff[1]+L2$coeff[3],b=L2$coeff[2],
       lwd=2,col=3)
#dev.off()

### Logistic regression 
# Hypertension example 
no.yes=c("No","Yes")
smoking=gl(2,1,8,no.yes)
obesity=gl(2,2,8,no.yes)
snoring=gl(2,4,8,no.yes)
n.tot=c(60,17,8,2,187,85,51,23)
n.hyp=c(5,2,1,0,35,13,15,8)
hypertension=data.frame(smoking,obesity,snoring,n.tot,n.hyp)
hyp.table=cbind(hypertension$n.hyp,hypertension$n.tot-hypertension$n.hyp)
hyp.table
M1=glm(hyp.table~smoking+obesity+snoring,family = binomial("logit"))
summary(M1)

#Alternatively...
prop.hyp=n.hyp/n.tot
M1_2=glm(prop.hyp~smoking+obesity+snoring,family=binomial,weights=n.tot)
summary(M1_2)

M2=glm(hyp.table~obesity+snoring,family = binomial("logit"))
anova(M2,test="Chisq")

#Odds ratio estimates 
library(MASS)
exp(cbind(OR=coef(M2),confint(M2)))

###################################

options(contrasts=c("contr.treatment","contr.poly"))
ldose=rep(0:5,2)
numdead=c(1,4,9,13,18,20,0,2,6,10,12,16)
sex=factor(rep(c("M","F"),c(6,6)))
SF=cbind(numdead,numalive=20-numdead)
M1=glm(SF~sex*ldose,family=binomial)
summary(M1)

#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
plot(c(1,32),c(0,1),type="n",xlab="dose",ylab="prob",log="x")
text(2^ldose,numdead/20,labels=as.character(sex))
ld=seq(0,5,0.1)
lines(2^ld,predict(M1,
  data.frame(ldose=ld,sex=factor(rep("M",length(ld)),
  levels=levels(sex))),type="response"),col=3,lwd=2)
lines(2^ld,predict(M1,
  data.frame(ldose=ld,sex=factor(rep("F",length(ld)),
  levels=levels(sex))),type="response"),lty=2,col=2,lwd=2)
#dev.off()

M2=glm(SF~sex+ldose,family=binomial)
summary(M2)
#pdf(file='plot.pdf',height=10,width=14,pointsize=18)
plot(c(1,32),c(0,1),type="n",xlab="dose",ylab="prob",log="x")
text(2^ldose,numdead/20,labels=as.character(sex))
ld=seq(0,5,0.1)
lines(2^ld,predict(M2,
                   data.frame(ldose=ld,sex=factor(rep("M",length(ld)),
                                                  levels=levels(sex))),type="response"),col=3,lwd=2)
lines(2^ld,predict(M2,
                   data.frame(ldose=ld,sex=factor(rep("F",length(ld)),
                                                  levels=levels(sex))),type="response"),lty=2,col=2,lwd=2)
#dev.off()

