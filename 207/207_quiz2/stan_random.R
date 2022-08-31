
order_dat <- dat[order(continent),]
ni <- table(continent)
y_order <- log(order_dat$total_cases_per_million)
x_tpc <-  order_dat$tests_per_case
x_tbp <-  order_dat$total_boosters_per_hundred
x_msp <-  order_dat$male_smokers
x_hbp <-  order_dat$hospital_beds_per_thousand
x_hdi <-  order_dat$human_development_index
x_I_af <-  c(rep(1,ni[1]),rep(0,sum(ni[-1])))
x_I_as <-  c(rep(0,ni[1]), rep(1,ni[2]) ,rep(0,sum(ni[-c(1:2)])))
x_I_eu <-  c(rep(0,ni[1]+ni[2]), rep(1,ni[3]) ,rep(0,sum(ni[-c(1:3)])))
x_I_na <-  c(rep(0,ni[1]+ni[2]+ni[3]), rep(1,ni[4]) ,rep(0,sum(ni[-c(1:4)])))
x_I_sa <-  c(rep(0,ni[1]+ni[2]+ni[3]+ni[4]), rep(1,ni[5]) )



m_basic <-
  
  "
data{

int n;
vector[n] y;
vector[n] test_case;
vector[n] booster_prop;
vector[n] male_smoke;
vector[n] hospital_bed;
vector[n] hum_develop;
vector[n] i_af;
vector[n] i_as;
vector[n] i_eu;
vector[n] i_na;
vector[n] i_sa;

}

parameters{

real<lower=0> sigma;

real beta_int_af;
real beta_int_as;
real beta_int_eu;
real beta_int_sa;
real beta_int_na;

real beta_test_c_af;
real beta_test_c_as;
real beta_test_c_eu;
real beta_test_c_sa;
real beta_test_c_na;

real beta_boost_af;
real beta_boost_as;
real beta_boost_eu;
real beta_boost_sa;
real beta_boost_na;

real beta_m_smoke_af;
real beta_m_smoke_as;
real beta_m_smoke_eu;
real beta_m_smoke_sa;
real beta_m_smoke_na;

real beta_hospital_af;
real beta_hospital_as;
real beta_hospital_eu;
real beta_hospital_sa;
real beta_hospital_na;

real beta_hum_dvl_af;
real beta_hum_dvl_as;
real beta_hum_dvl_eu;
real beta_hum_dvl_sa;
real beta_hum_dvl_na;

real mu_int;
real mu_test_c;
real mu_boost;
real mu_m_smoke;
real mu_hospital;
real mu_hum_dvl;

real<lower = 0> tau_int;
real<lower = 0> tau_test_c;
real<lower = 0> tau_boost;
real<lower = 0> tau_m_smoke;
real<lower = 0> tau_hospital;
real<lower = 0> tau_hum_dvl;
}

model{

for(i in 1:n){
  y[i] ~ normal(
  
  beta_int_af*i_af[i] + beta_hum_dvl_as*i_as[i] + beta_int_eu*i_eu[i]+ beta_int_sa*i_sa[i] + beta_int_na*i_na[i] +
  
  beta_test_c_af*i_af[i]*test_case[i] + beta_test_c_as*i_as[i]*test_case[i] + beta_test_c_eu*i_eu[i]*test_case[i] + beta_test_c_sa*i_sa[i]*test_case[i] + beta_test_c_na*i_na[i]*test_case[i] + 
  
  beta_boost_af*i_af[i]*booster_prop[i] + beta_boost_as*i_as[i]*booster_prop[i] + beta_boost_eu*i_eu[i]*booster_prop[i] + beta_boost_sa*i_sa[i]*booster_prop[i] + beta_boost_na*i_na[i]*booster_prop[i] +
  
  beta_m_smoke_af*i_af[i]*male_smoke[i] + beta_m_smoke_as*i_as[i]*male_smoke[i] + beta_m_smoke_eu*i_eu[i]*male_smoke[i] + beta_m_smoke_sa*i_sa[i]*male_smoke[i] + beta_m_smoke_na*i_na[i]*male_smoke[i] + 
  
  beta_hospital_af*i_af[i]*hospital_bed[i] + beta_hospital_as*i_as[i]*hospital_bed[i] + beta_hospital_eu*i_eu[i]*hospital_bed[i] + beta_hospital_sa*i_sa[i]*hospital_bed[i] + beta_hospital_na*i_na[i]*hospital_bed[i] + 
  
  beta_hum_dvl_af*i_af[i]*hum_develop[i] + beta_hum_dvl_as*i_as[i]*hum_develop[i] + beta_hum_dvl_eu*i_eu[i]*hum_develop[i] + beta_hum_dvl_sa*i_sa[i]*hum_develop[i] + beta_hum_dvl_na*i_na[i]*hum_develop[i]
  
  , sigma) ;
}

beta_int_af ~ normal(mu_int, tau_int);
beta_int_as ~ normal(mu_int, tau_int);
beta_int_eu ~ normal(mu_int, tau_int);
beta_int_sa ~ normal(mu_int, tau_int);
beta_int_na ~ normal(mu_int, tau_int);


beta_test_c_af ~ normal(mu_test_c, tau_test_c);
beta_test_c_as ~ normal(mu_test_c, tau_test_c);
beta_test_c_eu ~ normal(mu_test_c, tau_test_c);
beta_test_c_sa ~ normal(mu_test_c, tau_test_c);
beta_test_c_na ~ normal(mu_test_c, tau_test_c);


beta_boost_af ~ normal(mu_boost, tau_boost);
beta_boost_as ~ normal(mu_boost, tau_boost);
beta_boost_eu ~ normal(mu_boost, tau_boost);
beta_boost_sa ~ normal(mu_boost, tau_boost);
beta_boost_na ~ normal(mu_boost, tau_boost);


beta_m_smoke_af ~ normal(mu_m_smoke, tau_m_smoke);
beta_m_smoke_as ~ normal(mu_m_smoke, tau_m_smoke);
beta_m_smoke_eu ~ normal(mu_m_smoke, tau_m_smoke);
beta_m_smoke_sa ~ normal(mu_m_smoke, tau_m_smoke);
beta_m_smoke_na ~ normal(mu_m_smoke, tau_m_smoke);


beta_hospital_af ~ normal(mu_hospital, tau_hospital);
beta_hospital_as ~ normal(mu_hospital, tau_hospital);
beta_hospital_eu ~ normal(mu_hospital, tau_hospital);
beta_hospital_sa ~ normal(mu_hospital, tau_hospital);
beta_hospital_na ~ normal(mu_hospital, tau_hospital);


beta_hum_dvl_af ~ normal(mu_hum_dvl, tau_hum_dvl);
beta_hum_dvl_as ~ normal(mu_hum_dvl, tau_hum_dvl);
beta_hum_dvl_eu ~ normal(mu_hum_dvl, tau_hum_dvl);
beta_hum_dvl_sa ~ normal(mu_hum_dvl, tau_hum_dvl);
beta_hum_dvl_na ~ normal(mu_hum_dvl, tau_hum_dvl);

mu_int ~ normal(0, 100);
mu_test_c ~ normal(0, 100);
mu_boost ~ normal(0, 100);
mu_m_smoke ~ normal(0, 100);
mu_hospital ~ normal(0, 100);
mu_hum_dvl ~ normal(0, 100);

tau_int ~ inv_gamma(0.0001, 0.0001);
tau_test_c ~ inv_gamma(0.0001, 0.0001);
tau_boost ~ inv_gamma(0.0001, 0.0001);
tau_m_smoke ~ inv_gamma(0.0001, 0.0001);
tau_hospital ~ inv_gamma(0.0001, 0.0001);
tau_hum_dvl ~ inv_gamma(0.0001, 0.0001);

}


"

basic_data <- list(
  
  test_case = x_tpc,
  booster_prop = x_tbp,
  male_smoke = x_msp,
  hospital_bed = x_hbp,
  hum_develop = x_hdi,
  i_af = x_I_af,
  i_as = x_I_as,
  i_eu = x_I_eu,
  i_sa = x_I_sa,
  i_na = x_I_na,
  y = y_order,
  n = nrow(dat)
  
)

fit.stan <- stan(model_code = m_basic, data = basic_data, iter = 10000, warmup = 5000, chains = 1)









#part II

order_dat <- dat[order(continent),]
ni <- table(continent)
y_order <- log(order_dat$total_cases_per_million)
x_tpc <-  order_dat$tests_per_case
x_tbp <-  order_dat$total_boosters_per_hundred
x_msp <-  order_dat$male_smokers
x_hbp <-  order_dat$hospital_beds_per_thousand
x_hdi <-  order_dat$human_development_index

x_I_af <-  c(rep(1,ni[1]),rep(0,sum(ni[-1])))
x_I_as <-  c(rep(0,ni[1]), rep(1,ni[2]) ,rep(0,sum(ni[-c(1:2)])))
x_I_eu <-  c(rep(0,ni[1]+ni[2]), rep(1,ni[3]) ,rep(0,sum(ni[-c(1:3)])))
x_I_na <-  c(rep(0,ni[1]+ni[2]+ni[3]), rep(1,ni[4]) ,rep(0,sum(ni[-c(1:4)])))
x_I_sa <-  c(rep(0,ni[1]+ni[2]+ni[3]+ni[4]), rep(1,ni[5]) )

x_tpc_af <- x_tpc*x_I_af
x_tpc_as <- x_tpc*x_I_as
x_tpc_eu <- x_tpc*x_I_eu
x_tpc_sa <- x_tpc*x_I_sa
x_tpc_na <- x_tpc*x_I_na

x_tbp_af <- x_tbp*x_I_af
x_tbp_as <- x_tbp*x_I_as
x_tbp_eu <- x_tbp*x_I_eu
x_tbp_sa <- x_tbp*x_I_sa
x_tbp_na <- x_tbp*x_I_na

x_msp_af <- x_msp*x_I_af
x_msp_as <- x_msp*x_I_as
x_msp_eu <- x_msp*x_I_eu
x_msp_sa <- x_msp*x_I_sa
x_msp_na <- x_msp*x_I_na

x_hbp_af <- x_hbp*x_I_af
x_hbp_as <- x_hbp*x_I_as
x_hbp_eu <- x_hbp*x_I_eu
x_hbp_sa <- x_hbp*x_I_sa
x_hbp_na <- x_hbp*x_I_na

x_hdi_af <- x_hdi*x_I_af
x_hdi_as <- x_hdi*x_I_as
x_hdi_eu <- x_hdi*x_I_eu
x_hdi_sa <- x_hdi*x_I_sa
x_hdi_na <- x_hdi*x_I_na


x_all <- cbind(x_tpc_af, x_tpc_as, x_tpc_eu, x_tpc_na, x_tpc_sa,
               x_tbp_af, x_tbp_as, x_tbp_eu, x_tbp_na, x_tbp_sa,
               x_msp_af, x_msp_as, x_msp_eu, x_msp_na, x_msp_sa, 
               x_hbp_af, x_hbp_as, x_hbp_eu, x_hbp_na, x_hbp_sa,
               x_hdi_af, x_hdi_as, x_hdi_eu, x_hdi_na, x_hdi_sa
)





LLH_1 <- function(beta_int, sig, beta_test_c, beta_boost, beta_m_smoke, beta_hospital, beta_hum_dvl){
  yhat <- rep(NA, 38)  
  for (i in 1:38) {
    yhat[i] <- beta_int + hospital_bed[i]*beta_hospital + hum_develop[i]*beta_hum_dvl + test_case[i]*beta_test_c + booster_prop[i]*beta_boost + male_smoke[i]*beta_m_smoke
    
  }
  out <- list(
    log_like = sum(log(dnorm(y, mean = yhat, sd = sig))),
    yhat = yhat
  )
  return(out)
}

pos_mean_1 <- summary(fit.stan_1)$summary[,1]
pos_beta_int_1 <- pos_mean_1[1]
pos_sig_1 <- pos_mean_1[2]
pos_beta_test_c <- pos_mean_1[3]
pos_beta_boost <- pos_mean_1[4]
pos_beta_m_smoke <- pos_mean_1[5]
pos_beta_hospital <- pos_mean_1[6]
pos_beta_hum_dvl <- pos_mean_1[7]

pos_like_1 <- LLH_1(beta_int=pos_beta_int_1, sig=pos_sig_1, beta_test_c=pos_beta_test_c, beta_boost=pos_beta_boost, beta_m_smoke=pos_beta_m_smoke, beta_hospital=pos_beta_hospital, beta_hum_dvl=pos_beta_hum_dvl)



LLH_2 <- function(beta_int,beta_others,sig){
  yhat <- rep(NA, 38)
  for (i in 1:38) {
    yhat[i] =  beta_int[continent[i]] + x_tpc[i]*beta_others[1,continent[i]] + x_tbp[i]*beta_others[2,continent[i]] + x_msp[i]*beta_others[3,continent[i]] + x_hbp[i]*beta_others[4,continent[i]] + x_hdi[i]*beta_others[5,continent[i]]
  }

  out <- list(
    log_like = sum(log(dnorm(y_order, mean = yhat, sd = sig))),
    yhat = yhat
  )
  
  return(out)
}

pos_mean_2 <- summary(fit.stan_2)$summary[,1]
pos_beta_int_2 <- pos_mean_2[2:6]
pos_beta_others_2 <- matrix( pos_mean_2[7:31], nrow = 5, ncol = 5 , byrow = TRUE)
pos_sig_2 <- pos_mean_2[1]

pos_like_2 <- LLH_2(beta_int = pos_beta_int_2, beta_others = pos_beta_others_2, sig = pos_sig_2 )$log_like