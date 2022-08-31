probs=c(.45,.05,.01,.48,.70,.50,.07,.25,.49)
P=matrix(probs,nrow=3,ncol=3)
P
rownames(P)<-colnames(P)<-c("lower","middle","upper")
P
apply(P,MARGIN=1,FUN=sum)
P2=P%*%P
P2
##################
A=matrix(1:16,4,4,byrow=TRUE)

##################
a=c(1,2,-1,4,0)
summarizex=function(x,ff){
z=ff(x)
return(z)}
summarizex(a,mean)
summarizex(a,median)

x=rexp(1000000,rate=1)
logown=function(x){
z=rep(0,length(x))
for (i in 1:length(x)){
z[i]=log(x[i])}
return(z)}

system.time(log(x))
system.time(logown(x))

x=matrix(rnorm(60000),nrow=10000,ncol=6)
apply(x,2,mean)
z=numeric(6)
for (i in 1:6){
 z[i]=mean(x[,i])
}
z
apply(x,2,quantile,c(0.025,0.975))

x=seq(-2,2,length=50)
y=seq(-2,2,length=50)
f=function(x,y) x^2+y^2
f(x,y)
z=expand.grid(x,y)
f(z[,1],z[,2])
system.time(f(z[,1],z[,2]))

fgrid=function(x,y){
 n1=length(x)
 n2=length(y)
 M=matrix(NA,n1,n2)
 for (i in 1:n1){
  for (j in 1:n2){
   M[i,j]=f(x[i],y[j])}
  }
}
system.time(fgrid(x,y))
system.time(outer(x,y,f))

#######################################
authors <- data.frame(
  surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core"),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))
m1=merge(authors,books,by.x="surname",by.y="name")

