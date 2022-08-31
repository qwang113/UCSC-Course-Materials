library(titanic)
str(titanic_train)
sapply(titanic_train,function(x) sum(is.na(x)))
sapply(titanic_train,function(x) length(unique(x)))
# We consider the following variables:
# Survived (0-1 response)
# Pclass
# Sex
# Age
# SibSp
# Parch
# Fare
attach(titanic_train)
my_titanic=data.frame(Survived=Survived[1:800],Pclass=as.factor(Pclass[1:800]),
                      Age=Age[1:800],
                      SibSp=SibSp[1:800],Sex=Sex[1:800],
                      Parch=Parch[1:800],Fare=Fare[1:800])
my_titanic_test=data.frame(Survived=Survived[801:891],Pclass=as.factor(Pclass[801:891]),
                      Age=Age[801:891],
                      SibSp=SibSp[801:891],Sex=Sex[801:891],
                      Parch=Parch[801:891],Fare=Fare[801:891])
missing_index=(1:91)[apply(apply((my_titanic_test),2,is.na),1,sum)==1]
my_titanic_test=my_titanic_test[-missing_index,]
detach(titanic_train)

M1=glm(Survived~.,family=binomial,data=my_titanic)
summary(M1)
# We now fit a model without Parch and Fare 
M2=glm(Survived~Pclass+Age+SibSp+Sex,family=binomial,data=my_titanic)
summary(M2)
exp(cbind(OR=coef(M2),confint(M2)))
anova(M2)

fitted_results=predict(M2,my_titanic_test,type='response')
fitted_results<-ifelse(fitted_results>0.5,1,0)
mean(fitted_results==my_titanic_test$Survived)

library(ROCR)
p <- predict(M2,my_titanic_test,type="response")
pr <- prediction(p, my_titanic_test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(v=0.5,col='blue',lwd=2,lty=2)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

