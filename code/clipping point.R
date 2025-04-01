## Clipping point(Skipped point)

# asymptotic sample quantile variance
quantile.var.func <- function(tau = 0.1) {
  tau * (1 - tau) / dnorm( qnorm(tau) )^2
}

# asymptotic sample skipped quantile variance
quantile.nu.a1.a2.func <- function(tau, a1, a2) {
  tmp1 <- (1 - tau)^2 * ( tau - pnorm( qnorm(tau) + a2 ) ) + tau^2 * ( pnorm( qnorm(tau) + a1 ) - tau )
  tmp2 <- ( (1 - tau) * dnorm( qnorm(tau) + a2 ) +  tau * dnorm( qnorm(tau) + a1 ) - dnorm( qnorm(tau) ) )^2
  tmp1/tmp2
}

skipped_est_clipping_point <- function(tau, efficiency){
  
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
  
  return(list(epsilon= res.tau$epsilon, a1 = round(res.tau$a1, 2), a2 = round(res.tau$a2, 2)))
}

## Example
# skipped_est_clipping_point(0.9, 0.95)
# epsilon = 0.001 / a1 = 1.81 / a2 = -3.65