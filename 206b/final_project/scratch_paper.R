iter <- 100000
beta_0 <- matrix(NA, nrow = I, ncol = iter)
beta_1 <- beta_2 <- sigma <- tau <- beta_0_bar <- rep(NA, iter)
rho <- matrix(NA, nrow = 2, ncol = iter)
beta_0[,1] <- beta_0_bar[1] <- 30
beta_1[1] <- 10
beta_2[1] <- 0
sigma[1] <- tau[1] <- 100 


#Specify hyper parameter

beta_1_bar <- 10
beta_2_bar <- -0.15
u_1 <- 100
u_2 <- 100
mu_0 <- 30
v <- 1000
a_sig <- 1
b_sig <- 1
a_tau <- 1
b_tau <- 1

#Write functions for Metropolis Hasting part:

exp_pi_b1 <- function(beta0, beta1,beta2, beta1bar, u1, sig){
  
  y = exp(-(beta1-beta1bar)^2 / (2*u1) - sum((weight - beta0/(1+beta1*exp(beta2*time)) )^2, na.rm = TRUE) /(2*sig))
  
  return(y)
}

exp_pi_b2 <- function(beta0, beta1,beta2, beta2bar, u2, sig){
  
  y = exp(-(beta2-beta2bar)^2 / (2*u2) - sum((weight - beta0/(1+beta1*exp(beta2*time)) )^2, na.rm = TRUE) /(2*sig))
  
  return(y)
}


i <- 1
for (i in 1:(iter-1)) {
  #Generate beta0
  
  
  mean_vec <-  ( rowSums( weight / ( 1 + beta_1[i]*exp(beta_2[i]*time) ), na.rm = TRUE) / sigma[i] + beta_0_bar[i] / tau[i] ) /
    (   rowSums(   1 / ( 1 + beta_1[i]*exp(beta_2[i]*time) )^2 , na.rm = TRUE) / sigma[i] + 1/tau[i]    )
  
  var_vec <- 1 / (   rowSums(   1 / ( 1 + beta_1[i]*exp(beta_2[i]*time) )^2 , na.rm = TRUE) / sigma[i] + 1/tau[i]    )
  
  beta_0[,i+1] <- mvtnorm::rmvnorm(1, mean = mean_vec, sigma = diag(var_vec, I )) 
  
  
  #Generate beta0bar
  
  mean_beta0bar <- (sum(beta_0[,i+1])/tau[i] + mu_0/v) / (I/tau[i] + 1/v)
  var_beta0bar <- 1 / (I/tau[i] + 1/v)
  beta_0_bar[i+1] <- rnorm(1, mean = mean_beta0bar, sd = sqrt(var_beta0bar))
  
  
  #Generate beta1
  
  #step1: Propose a new value 
  
  rate_1 <- 0.4
  
  beta1_xi <- rnorm(1, mean = beta_1[i], sd = rate_1)
  
  #step2: calculate transit probability
  
  new_prob_1 <- exp_pi_b1(beta0 = beta_0[,i+1], beta1 = beta1_xi, beta2 = beta_2[i], beta1bar = beta_1_bar, u1 = u_1, sig = sigma[i])
  old_prob_1 <- exp_pi_b1(beta0 = beta_0[,i+1], beta1 = beta_1[i], beta2 = beta_2[i], beta1bar = beta_1_bar, u1 = u_1, sig = sigma[i])
  p_1 <- min(c(1, new_prob_1/old_prob_1 ))
  
  #step3: Decide whether to transit
  
  l1 <- runif(1)
  if(l1 <= p_1){
    beta_1[i+1] <- beta1_xi
  }else{
    beta_1[i+1] <- beta_1[i]
  }
  
  #Generate beta2
  
  #step1: Propose a new value 
  
  rate_2 <- 0.003
  beta2_xi <- rnorm(1, mean = beta_2[i], sd = rate_2)
  
  #step2: calculate transit probability
  
  new_prob_2 <- exp_pi_b2(beta0 = beta_0[,i+1], beta1 = beta_1[i+1], beta2 = beta2_xi, beta2bar = beta_2_bar, u2 = u_2, sig = sigma[i])
  old_prob_2 <- exp_pi_b2(beta0 = beta_0[,i+1], beta1 = beta_1[i+1], beta2 = beta_2[i], beta2bar = beta_2_bar, u2 = u_2, sig = sigma[i])
  p_2 <- min(c(1,  new_prob_2 / old_prob_2 ))
  
  #step3: Decide whether to transit
  
  l2 <- runif(1)
  if(l2<=p_2){
    beta_2[i+1] <- beta2_xi
  }else{
    beta_2[i+1] <- beta_2[i]
  }
  
  rho[1, i] <- p_1
  rho[2, i] <- p_2
  
  #Generate sigma
  
  rate_param <-  1/2 * sum(  ( weight - beta_0[,i+1]/(1+beta_1[i+1]*exp(beta_2[i+1]*time)) )^2  ,na.rm = TRUE) + b_sig
  
  inv_sig <- rgamma(1, shape = N/2 + a_sig, rate = rate_param)
  
  sigma[i+1] <- 1/inv_sig
  
  
  #Generate tau
  
  rate_param <- 1/2 * sum(  (beta_0[,i+1] - beta_0_bar[i+1])^2    ) + b_tau
  inv_tau <- rgamma(1, shape = I/2 + a_tau, rate = rate_param)
  tau[i+1] <- 1/inv_tau
  
}

mcmcse::ess(beta_1[thin])
mcmcse::ess(beta_2[thin])

thin <- seq(from = 12000, to = 100000, by = 88)
plot(beta_1[thin], type = 'l')