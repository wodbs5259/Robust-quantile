source("function.R")

simulation.data.make <- function(n = 100, p = 1, error.rate = 0.1, simul = 1) {
  
  if(simul == 1) {
    
    if(p == 1){
      beta <- c(0, 1)
      
      x <- matrix(runif(n * p, -5, 5), nc = p)
      fx <- beta[1] + drop(x %*% beta[-1])
      e <- c(rnorm(n * (1 - error.rate), 0, 1), rnorm(n * error.rate, 5, 1))
      y <- fx + e
      
    } else {
      
      beta <- c(0, 1, -1, 0, 0, 0)
      
      x <- matrix(runif(n * p, -5, 5), nc = p)
      fx <- beta[1] + drop(x %*% beta[-1])
      e <- c(rnorm(n * (1 - error.rate), 0, 1), rnorm(n * error.rate, 5, 1))
      y <- fx + e
      
    }
    
  } else {
    
    if(p == 1){
      beta <- c(0, 1)
      
      x <- matrix(runif(n * p, -5, 5), nc = p)
      fx <- beta[1] + drop(x %*% beta[-1])
      e <- c(rnorm(n * (1 - error.rate), 0, 1), rnorm(n * error.rate, 0, 5))
      y <- fx + e
      
    } else {
      
      beta <- c(0, 1, -1, 0, 0, 0)
      
      x <- matrix(runif(n * p, -5, 5), nc = p)
      fx <- beta[1] + drop(x %*% beta[-1])
      e <- c(rnorm(n * (1 - error.rate), 0, 1), rnorm(n * error.rate, 0, 5))
      y <- fx + e
      
    }
    
  }
  data <- data.frame(x = x, y = y)
  
  return(list(data = data, beta = beta))
  
}


n.tmp <- c(100, 200, 400)
error.rate.tmp <- c(0.05, 0.1, 0.15, 0.2)
simul.tmp <- c(1, 2)
p.tmp <- c(1, 5)
tau.tmp <- c(0.95, 0.9, 0.75, 0.5, 0.25, 0.1, 0.05)

param.grid <- expand.grid(n = n.tmp, error.rate = error.rate.tmp, simul = simul.tmp, p = p.tmp, tau = tau.tmp)

for(k in 1:nrow(param.grid)){
  params <- param.grid[k, ]
  
  cat("n :", params[, "n"], " error.rate :", params[, "error.rate"], " simul :", params[, "simul"], 
      " p :", params[, "p"], " tau :", params[, "tau"], "start\n\n")
  
  tau <- params[, "tau"]
  
  for (i in 1:100) {
    
    set.seed(13 * i + 120)
    
    tmp <- simulation.data.make(n = params[, "n"], error.rate = params[, "error.rate"], 
                                simul = params[, "simul"], p = params[, "p"])
    
    data <- tmp$data
    beta <- tmp$beta
    
    # S-Quantile
    S.Q.MADN <- fit.rob(data = data, tau = tau, eff = 0.95, sig_type = "MADN", Type = "linear")$b
    
    # true quantile regression
    true.quantile <- c(beta[1] + qnorm(tau), beta[-1])
    
    # quantile regression
    Quantile <- coef(rq(data[,ncol(data)] ~ as.matrix(data[,-ncol(data)]), tau = tau))
    
    write(c(i, true.quantile, Quantile, S.Q.MADN),
          file = paste0("../result/reg/", "Simul", params[, "simul"], "/",
                        "Simul", params[, "simul"], "_n_", params[, "n"], "_p_", params[, "p"], 
                        "_error.rate_", params[, "error.rate"], "_tau_", params[, "tau"], ".txt"),
          ncolumns = length(c(i, true.quantile, Quantile, S.Q.MADN)), append = T)
    
  }
  
  cat("n :", params[, "n"], " error.rate :", params[, "error.rate"], " simul :", params[, "simul"], 
      " p :", params[, "p"], " tau :", params[, "tau"], "finish\n\n")
  
}
