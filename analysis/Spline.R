source("function.R")

library(tidyverse)
library(splines)


data("balloon", package = "ftnonpar")

balloon <- balloon[(1:length(balloon)) %% 10 == 1]

dat <- data.frame(x = seq_along(balloon), y = balloon)

check <- smooth.spline(x = dat$x, y = dat$y)

tau.tmp <- c(0.10, 0.25, 0.50, 0.75, 0.90)


res <- list()

for(i in 1:length(tau.tmp)){
  
  tau <- tau.tmp[i]
  
  
  # B-spline 변환된 독립 변수
  b_spline <- ns(seq_along(balloon), df = check$df)
  
  
  S.Q.MADN <- fit.rob(data = dat, tau = tau, sig_type = "MADN", Type = "spline", df = check$df)
  S.Q.MADN.y.pred <- drop(S.Q.MADN$b[1] + S.Q.MADN$b[-1] %*% t(b_spline))
  
  
  Quantile <- coef(rq(y ~ ns(x, df = check$df), tau = tau, data = dat))
  Quantile.y.pred <- drop(Quantile[1] + Quantile[-1] %*% t(b_spline))
  
  
  res[[i]] <- data.frame(Q = Quantile.y.pred, S.Q = S.Q.MADN.y.pred)
}


## result

# names(res) <- tau.tmp
# 
# x <- seq(0, 1, length.out = length(seq_along(balloon)))
# 
# dat <- data.frame(Time = x, Radiation = balloon)
# 
# p1 <- ggplot(dat) + geom_point(aes(x = Time, y = Radiation), shape = 1, size = 0.5) + # alpha = 0.4
#   theme_bw() + labs(title = "") +
#   geom_line(aes(x = Time, y = res[["0.1"]][,1]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.25"]][,1]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.5"]][,1]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.75"]][,1]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.9"]][,1]), lwd = 0.2, color = "blue") +
#   theme(plot.title = element_text(size = 10, hjust = 0.4, face='bold'),
#         axis.text.x = element_text(size = 6, face='bold'),
#         axis.text.y = element_text(size = 6, hjust=1, face='bold'),
#         axis.title.x = element_text(size = 8, face='bold'),
#         axis.title.y = element_text(size = 8, face='bold'),
#         panel.grid = element_blank(),
#         plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
# 
# p2 <- ggplot(dat) + geom_point(aes(x = Time, y = Radiation), shape = 1, size = 0.5) + # alpha = 0.4
#   theme_bw() + labs(title = "") +
#   geom_line(aes(x = Time, y = res[["0.1"]][,2]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.25"]][,2]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.5"]][,2]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.75"]][,2]), lwd = 0.2, color = "blue") +
#   geom_line(aes(x = Time, y = res[["0.9"]][,2]), lwd = 0.2, color = "blue") +
#   theme(plot.title = element_text(size = 10, hjust = 0.5, face='bold'),
#         axis.text.x = element_text(size = 6, face='bold'),
#         axis.text.y = element_text(size = 6, hjust=1, face='bold'),
#         axis.title.x = element_text(size = 8, face='bold'),
#         axis.title.y = element_text(size = 8, face='bold'),
#         panel.grid = element_blank(),
#         plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
# 
# 
# p12 <- gridExtra::grid.arrange(p1, p2, ncol = 2)
