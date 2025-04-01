
require(quantreg)

## Clipped point

quantile.var.func <- function(tau = 0.1) {
  tau * (1 - tau) / dnorm( qnorm(tau) )^2
}

quantile.nu.a1.a2.func <- function(tau, a1, a2) {
  tmp1 <- (1 - tau)^2 * ( tau - pnorm( qnorm(tau) + a2 ) ) + tau^2 * ( pnorm( qnorm(tau) + a1 ) - tau )
  tmp2 <- ( (1 - tau) * dnorm( qnorm(tau) + a2 ) +  tau * dnorm( qnorm(tau) + a1 ) - dnorm( qnorm(tau) ) )^2
  tmp1/tmp2
}

clipped_point <- function(tau, efficiency){
  
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
# clipped_point(0.9, 0.95)
# epsilon = 0.001 / a1 = 1.81 / a2 = -3.65


##rho and psi function

rho_a <- function(u, a1 = 2.68, a2 = -2.68, tau = 0.5){
  ifelse(u <= a2, -(1 - tau) * a2, 
         ifelse(a2 < u & u <= 0, -(1 - tau) * u, 
                ifelse(0 < u & u <= a1, tau * u, tau * a1)
         )
  )
}

psi_a <- function(u, a1 = 2.68, a2 = -2.68, tau = 0.5){
  ifelse(a2 < u & u <= 0, -(1 - tau), 
         ifelse(0 < u & u <= a1, tau, 0)
  )
} 

## Example
# par(mfrow = c(1, 2))
# x <- seq(-5, 5, by = 0.001)
# plot(x, rho_a(x, a1 = 1.81, a2 = -3.65, tau = 0.9), ylab = "", cex = 0.1)
# abline(v = -3.65, col = "red", lty = 2)
# abline(v = 1.81, col = "blue", lty = 2)

# plot(x, psi_a(x, a1 = 1.81, a2 = -3.65, tau = 0.9), ylab = "", cex = 0.1)
# abline(v = -3.65, col = "red", lty = 2)
# abline(v = 1.81, col = "blue", lty = 2)


## robust quantile function

# robust quantile estimation

fit.rob.qt <- function (x, tau = 0.5, w.grid = NULL, sig_type = "MADN", a1 = 2.66, a2 = -2.66, center = NULL, prt = FALSE) {
  
  require(robustbase)
  
  if (is.null(w.grid)) w.grid <- 2^(-4:10)
  
  m <- ifelse(is.null(center), median(x), center)
  
  sig <- ifelse(sig_type == "MADN", mad(x, center = median(x)), mad(x, center = median(x), constant = 1))
  
  z <- (x - m)/sig
  
  obj <- sum(rho_a(z, a1 = a1, a2 = a2, tau = tau))
  
  iter <- 1; dif <- Inf
  
  repeat {
    
    if (prt == TRUE) cat(iter, ",", m, "\n")
    
    r <- z - sapply(w.grid, FUN = function(w) psi_a(z, a1 = a1, a2 = a2, tau = tau)/w)
    
    t <- x - r * sig
    
    t.mean.new <- colMeans(t)
    
    z.new <- sapply(t.mean.new, FUN = function(t) (x - t)/sig)
    
    obj.new <- apply(z.new, 2, FUN = function(z) sum(rho_a(z, a1 = a1, a2 = a2, tau = tau)))
    
    idx.w <- which.min(obj.new)
    
    m.new <- t.mean.new[idx.w]
    
    if (abs(obj - obj.new[idx.w])/obj < 1e-8 || iter == 1000) break
    
    m <- m.new; obj <- obj.new[idx.w]; z <- z.new[,idx.w]; iter <- iter + 1
  }
  
  return(list(m = m, iter = iter))
  
}

# robust quantile regression

fit.rob.lin <- function (x, y, tau = 0.5, w.grid = NULL, sig_type = "MADN", a1 = 2.66, a2 = -2.66, prt = FALSE) {
  
  if (is.null(w.grid)) w.grid <- 2^(-4:10)
  
  if (is.matrix(x) == F) x <- as.matrix(x)
  
  b <- coef(rq(y ~ x, tau = 0.5))
  
  resid <- y - drop(b[1] + x %*% b[-1]) 
  
  sig <- ifelse(sig_type == "MADN", median(abs(resid))/0.675, median(abs(resid)))
  
  z <- resid/sig
  
  obj <- sum(rho_a(z, a1 = a1, a2 = a2, tau = tau))
  
  iter <- 1
  
  repeat {
    
    if (prt == TRUE) cat(iter, ",", b, ",", obj, "\n")
    
    r.mat <- z - sapply(w.grid, FUN = function(w) psi_a(z, a1 = a1, a2 = a2, tau = tau)/w)
    
    b.mat <- apply(r.mat, 2, FUN = function (r) coef(lm(y - r * sig ~ x)))
    
    resid.mat <- apply(b.mat, 2, FUN = function (b) y - drop(b[1] + x %*% b[-1]))
    
    z.mat <- resid.mat / sig
    
    obj.vec <- apply(z.mat, 2, FUN = function (z) sum(rho_a(z, a1 = a1, a2 = a2, tau = tau)))
    
    idx.w <- which.min(obj.vec)
    
    b_new <- b.mat[,idx.w]
    
    obj_new <- obj.vec[idx.w]
    
    if (abs(obj-obj_new) < 1e-4 || iter == 200) break
    
    b <- b_new; obj <- obj_new; z <- z.mat[,idx.w]; iter <- iter + 1
    
  }
  
  return(list(b = b, iter = iter))
  
}

# robust quantile spline regression (Using ns() function)

fit.rob.spline <- function (x, y, tau = 0.5, w.grid = NULL, sig_type = "MADN", a1 = 2.66, a2 = -2.66, df = NULL, prt = FALSE) {
  
  if (is.null(w.grid)) w.grid <- 2^(-4:10)
  
  if (is.null(df)) df <- smooth.spline(x = x, y = y)$df
  
  b <- coef(rq(y ~ ns(x, df = df), tau = 0.5))
  resid <- y - (b[1] + b[-1] %*% t(ns(x, df = df)))
  
  sig <- ifelse(sig_type == "MADN", median(abs(resid))/0.675, median(abs(resid)))
  
  z <- drop(resid/sig)
  
  obj <- sum(rho_a(z, a1 = a1, a2 = a2, tau = tau))
  
  iter <- 1
  
  repeat {
    
    if (prt == TRUE) cat(iter, ",", b, ",", obj, "\n")
    
    r.mat <- z - sapply(w.grid, FUN = function(w) psi_a(z, a1 = a1, a2 = a2, tau = tau)/w)
    
    b.mat <- apply(r.mat, 2, FUN = function (r) coef(lm((y - r * sig) ~ ns(x, df = df))))
    
    resid.mat <- apply(b.mat, 2, FUN = function (b) y - (b[1] + b[-1] %*% t(ns(x, df = df))))
    
    z.mat <- resid.mat / sig
    
    obj.vec <- apply(z.mat, 2, FUN = function (z) sum(rho_a(z, a1 = a1, a2 = a2, tau = tau)))
    
    idx.w <- which.min(obj.vec)
    
    b_new <- b.mat[,idx.w]
    
    obj_new <- obj.vec[idx.w]
    
    if (abs(obj-obj_new) < 1e-4 || iter == 200) break
    
    b <- b_new; obj <- obj_new; z <- z.mat[,idx.w]; iter <- iter + 1
    
  }
  
  return(list(b = b, iter = iter))
  
}

# integration
fit.rob <- function(data, tau = 0.5, center = NULL, df = NULL, eff = 0.95, sig_type = "MADN", Type = "Estimation"){
  
  clipped_point <- clipped_point(tau = tau, efficiency = eff)
  
  a1 <- clipped_point$a1
  a2 <- clipped_point$a2
  
  # Type = "Estimation" / "linear" / "spline"
  # If Type = "EStimation" is true, data is a vector.
  # If Type = "linear" or "spline" is true, data is data.frame or matrix.
  # And the target variable must be placed in the last column of the data.
  if(Type == "Estimation"){
    
    res <- fit.rob.qt(x = data, a1 = a1, a2 = a2, tau = tau, sig_type = sig_type, center = center)
    
  } else if(Type == "linear") {
    
    res <- fit.rob.lin(x = data[,-ncol(data)], y = data[,ncol(data)],
                       a1 = a1, a2 = a2, tau = tau, sig_type = sig_type)
    
  } else {
    
    res <- fit.rob.spline(x = data[,-ncol(data)], y = data[,ncol(data)],
                          a1 = a1, a2 = a2, tau = tau, df = df, sig_type = sig_type)
    
  }
  
  return(res)
}