
##############################################################
#### Function ################################################

quantile.var.func <- function(tau = 0.1) {
  tau * (1 - tau) / dnorm( qnorm(tau) )^2
}

quantile.nu.a1.a2.func <- function(tau, a1, a2) {
  tmp1 <- (1 - tau)^2 * ( tau - pnorm( qnorm(tau) + a2 ) ) + tau^2 * ( pnorm( qnorm(tau) + a1 ) - tau )
  tmp2 <- ( (1 - tau) * dnorm( qnorm(tau) + a2 ) +  tau * dnorm( qnorm(tau) + a1 ) - dnorm( qnorm(tau) ) )^2
  tmp1/tmp2
}

##############################################################


##############################################################
#### Check Median ############################################

( tmp1 <- (pnorm( 2.68 ) - 0.5) / ( 2 * (dnorm( 2.68 ) - dnorm( 0 ))^2 ) )
( tmp2 <- quantile.var.func(tau = 0.5) )
tmp1/tmp2

quantile.nu.a1.a2.func(tau = 0.5, a1 = 2.68, a2 = -2.68)
quantile.var.func(tau = 0.5)

quantile.nu.a1.a2.func(tau = 0.5, a1 = 2.68, a2 = -2.68) / quantile.var.func(tau = 0.5)

quantile.var.func(tau = 0.5) / quantile.nu.a1.a2.func(tau = 0.5, a1 = 2.68, a2 = -2.68)
##############################################################

library(dplyr)

##############################################################
#### ン = 0.1 #################################################

tau <- 0.1

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.1 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
  a2 <- qnorm(epsilon[i]) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.1 <- rbind(res.tau0.1, tmp)
  
}

res.tau0.1 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.1 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.1 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.2 #################################################

tau <- 0.2

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.2 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
  a2 <- qnorm(epsilon[i]) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.2 <- rbind(res.tau0.2, tmp)
  
}

res.tau0.2 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.2 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.2 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.3 #################################################

tau <- 0.3

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.3 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
  a2 <- qnorm(epsilon[i]) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.3 <- rbind(res.tau0.3, tmp)
  
}

res.tau0.3 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.3 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.3 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.4 #################################################

tau <- 0.4

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.4 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
  a2 <- qnorm(epsilon[i]) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.4 <- rbind(res.tau0.4, tmp)
  
}

res.tau0.4 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.4 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.4 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.5 (ン ‖ 1/2) #######################################

tau <- 0.5

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.5 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
  a2 <- qnorm(epsilon[i]) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.5 <- rbind(res.tau0.5, tmp)
  
}

res.tau0.5 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.5 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.5 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.5 (ン ｜ 1/2) #######################################

tau <- 0.5

epsilon <- seq(0.0001, tau, by = 0.0001)

res.tau0.5 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
  a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.5 <- rbind(res.tau0.5, tmp)
  
}

res.tau0.5 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.5 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.5 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.6 #################################################

tau <- 0.6

epsilon <- seq(0.0001, 1 - tau, by = 0.0001)

res.tau0.6 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
  a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.6 <- rbind(res.tau0.6, tmp)
  
}

res.tau0.6 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.6 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.6 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.7 #################################################

tau <- 0.7

epsilon <- seq(0.0001, 1 - tau, by = 0.0001)

res.tau0.7 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
  a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.7 <- rbind(res.tau0.7, tmp)
  
}

res.tau0.7 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.7 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.7 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.8 #################################################

tau <- 0.8

epsilon <- seq(0.0001, 1 - tau, by = 0.0001)

res.tau0.8 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
  a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.8 <- rbind(res.tau0.8, tmp)
  
}

res.tau0.8 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.8 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.8 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### ン = 0.9 #################################################

tau <- 0.9

epsilon <- seq(0.0001, 1 - tau, by = 0.0001)

res.tau0.9 <- NULL

for(i in 1:length(epsilon)){
  
  a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
  a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
  
  eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
  
  tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
  
  res.tau0.9 <- rbind(res.tau0.9, tmp)
  
}

res.tau0.9 %>% mutate(eff.95 = abs(eff - 0.95)) %>% arrange(eff.95) %>% head(5)
res.tau0.9 %>% mutate(eff.90 = abs(eff - 0.90)) %>% arrange(eff.90) %>% head(5)
res.tau0.9 %>% mutate(eff.80 = abs(eff - 0.80)) %>% arrange(eff.80) %>% head(5)

##############################################################

##############################################################
#### function of ン in epsilon ################################

quantile.var.func <- function(tau = 0.1) {
  tau * (1 - tau) / dnorm( qnorm(tau) )^2
}

quantile.nu.a1.a2.func <- function(tau, a1, a2) {
  tmp1 <- (1 - tau)^2 * ( tau - pnorm( qnorm(tau) + a2 ) ) + tau^2 * ( pnorm( qnorm(tau) + a1 ) - tau )
  tmp2 <- ( (1 - tau) * dnorm( qnorm(tau) + a2 ) +  tau * dnorm( qnorm(tau) + a1 ) - dnorm( qnorm(tau) ) )^2
  tmp1/tmp2
}



skipped_estimation_find_a <- function(tau, efficiency){
  
  require(tidyverse)
  
  res.tau <- NULL
  
  if(tau <= 0.5) {
    
    epsilon <- seq(0.0001, tau, by = 0.0001)
    
    for(i in 1:length(epsilon)){
      
      a1 <- qnorm(1 - (1 - tau) * epsilon[i] / tau) - qnorm(tau)
      a2 <- qnorm(epsilon[i]) - qnorm(tau)
      
      eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
      
      tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
      
      res.tau <- rbind(res.tau, tmp)
      
    }
    
  } else {
    
    epsilon <- seq(0.0001, 1 - tau, by = 0.0001)
    
    for(i in 1:length(epsilon)){
      
      a1 <- qnorm(1 - epsilon[i]) - qnorm(tau)
      a2 <- qnorm(tau * epsilon[i]/(1 - tau)) - qnorm(tau)
      
      eff <- quantile.var.func(tau = tau) / quantile.nu.a1.a2.func(tau = tau, a1 = a1, a2 = a2)
      
      tmp <- data.frame(epsilon = epsilon[i], a1 = a1, a2 = a2, eff = eff)
      
      res.tau <- rbind(res.tau, tmp)
      
    }
    
  }
  
  res.tau <- res.tau %>% mutate(eff.diff = abs(eff - efficiency)) %>% arrange(eff.diff) %>% head(1)
  
  alpha_star <- min(tau^2/(1-tau), (1-tau)^2/tau)
  
  return(list(epsilon= res.tau$epsilon, a1 = res.tau$a1, a2 = res.tau$a2, alpha_star = alpha_star))
}

skipped_estimation_find_a(tau = 0.05, efficiency = 0.8)

##############################################################
