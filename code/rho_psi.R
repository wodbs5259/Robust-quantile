# clipped check loss function
rho_a <- function(u, a1 = 2.68, a2 = -2.68, tau = 0.5){
  ifelse(u <= a2, -(1 - tau) * a2, 
         ifelse(a2 < u & u <= 0, -(1 - tau) * u, 
                ifelse(0 < u & u <= a1, tau * u, tau * a1)
         )
  )
}

# subgradient of the clipped check loss function
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
