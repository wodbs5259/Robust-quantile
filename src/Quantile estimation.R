
source("function.R")

# When simulation is 0, parameter grid is eliminated error rate object.

n.tmp <- c(100, 200, 400)
tau.tmp <- c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05)
param.grid <- expand.grid(n = n.tmp, tau = tau.tmp)

for(k in 1:nrow(param.grid)){
  params <- param.grid[k, ]
  
  cat("n :", params[, "n"], " simul :", 0, " tau :", params[, "tau"], "start\n\n")
  
  clipping_point <- skipped_est_clipping_point(tau = params[, "tau"], efficiency = 0.95)
  
  a1 <- clipping_point$a1
  a2 <- clipping_point$a2
  
  tau <- params[, "tau"]
  
  for (i in 1:100) {
    
    set.seed(13 * i + 120)
    
    data <- rnorm(n = params[, "n"], m = 0, sd = 1)
    
    # S-Quantile
    S.Q.MADN <- fit.rob(data = data, tau = tau, sig_type = "MADN", Type = "Estimation", center = median(data))$m
    
    true.quantile <- as.numeric( qnorm(p = tau, m = 0, s = 1) )
    
    Quantile <- as.numeric( quantile(data, probs = tau) )
    
    write(c(i, true.quantile, Quantile, S.Q.MADN),
          file = paste0("../result/est/Simul0/",
                        "Simul0", "_n_", params[, "n"], "_tau_", params[, "tau"], ".txt"),
          ncolumns = length(c(i, true.quantile, Quantile, S.Q.MADN)), append = T)
    
  }
  
  cat("n :", params[, "n"], " simul :", 0, " tau :", params[, "tau"], "finish\n\n")
  
}



simulation.data.make <- function(n = 100, error.rate = 0.1, simul = 1) {
  
  if(simul == 1){
    
    real.data <- rnorm(n * (1 - error.rate), mean = 0, sd = 1)
    contaminate.data <- rnorm(n * error.rate, mean = 5, sd = 0.1)
    
  } else {
    
    real.data <- rnorm(n * (1 - error.rate), mean = 0, sd = 1)
    contaminate.data <- rnorm(n * error.rate, mean = 0, sd = 5)
    
  }
  
  data <- c(real.data, contaminate.data)
  
  return(list(real.data = real.data, data = data))
  
}

n.tmp <- c(100, 200, 400)
error.rate.tmp <- c(0.05, 0.1, 0.15, 0.2)
simul.tmp <- c(1, 2)
tau.tmp <- c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05)

param.grid <- expand.grid(n = n.tmp, error.rate = error.rate.tmp, simul = simul.tmp, tau = tau.tmp)

for(k in 1:nrow(param.grid)){
  params <- param.grid[k, ]
  
  cat("n :", params[, "n"], " error.rate :", params[, "error.rate"], " simul :", params[, "simul"], 
      " tau :", params[, "tau"], "start\n\n")
  
  clipping_point <- skipped_est_clipping_point(tau = params[, "tau"], efficiency = 0.95)
  
  a1 <- clipping_point$a1
  a2 <- clipping_point$a2
  
  tau <- params[, "tau"]
  
  for (i in 1:100) {
    
    set.seed(13 * i + 120)
    
    tmp <- simulation.data.make(n = params[, "n"], error.rate = params[, "error.rate"], 
                                simul = params[, "simul"])
    
    real.data <- tmp$real.data
    data <- tmp$data
    
    # S-Quantile
    S.Q.MADN <- fit.rob(data = data, tau = tau, sig_type = "MADN", Type = "Estimation", center = median(data))$m
    
    true.quantile <- as.numeric( qnorm(p = tau, m = 0, s = 1) )
    
    Quantile <- as.numeric( quantile(data, probs = tau) )
    
    write(c(i, true.quantile, Quantile, S.Q.MADN),
          file = paste0("../result/est/", "Simul", params[, "simul"], "/",
                        "Simul", params[, "simul"], "_n_", params[, "n"], 
                        "_error.rate_", params[, "error.rate"], "_tau_", params[, "tau"], ".txt"),
          ncolumns = length(c(i, true.quantile, Quantile, S.Q.MADN)), append = T)
    
  }
  
  cat("n :", params[, "n"], " error.rate :", params[, "error.rate"], " simul :", params[, "simul"], 
      " tau :", params[, "tau"], "finish\n\n")
  
}
