k = 500
#Generate W and c

w <- runif(k, -1, 1)
c <- runif(k, -1, 1)

# Calculating H
alpha = 0
H_all <- matrix(NA, nrow = length(x_1d), ncol = k)

for (i in 1:length(x_1d)) {
  H_all[i,] <- w*x_1d[i] + c
}

sgm <- function(x){
  out <- exp(x)/(1+exp(x))
  return(out)
}
if(alpha == 0){
  gh_0 = sgm(H_all)
  gh <- cbind(1,gh_0)
}else{
  gh_0 = apply(H_all, c(1,2), max, 0)
  gh <- cbind(1,gh_0)
}
#model_1 <- lm(y_1d ~ gh)

#M <- summary(model_1)$coefficients[,1]

param <- matrix(ginv(gh) %*% y_1d, ncol = 1)

xv <- seq(from = -2, to = 12, by = 0.01)
H_v <- matrix(NA, nrow = length(xv), ncol = k)
for (i in 1:length(xv)) {
  H_v[i,] <- w*xv[i] + c
}
if(alpha == 0){
gv_0 <- sgm(H_v)
gv <- cbind(1, gv_0)
}else{
  gv_0 = apply(H_v, c(1,2), max, 0)
  gv <- cbind(1,gv_0)
  
}


yhat <- gv%*%param
#naparam <- which(is.na(model_1$coefficients))

#omg <- as.matrix(M, ncol = 1)
#if(length(naparam)==0){
#  yhat <- gv %*% omg
#}else{
#  yhat <- gv[,-naparam] %*% omg
#}
par(mfrow = c(1,2))
plot(x_all, y, type = 'l', lwd = 2, col = "red", xlab = "X", main = "True Function")
plot(x = seq(from = -2, to = 12, by = 0.01 ), y = yhat, type = 'l', lwd = 2, xlab = "X", main = "Estimated Function(k=10)")
