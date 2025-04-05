source("function.R")

library(tidyverse)


data("engel", package = "quantreg")

engel <- log10(engel)

set.seed(200)

eps <- 0.1

outlier.income <- sample(sort(unique(engel$income)), size = ceiling(nrow(engel) * eps / (1 - eps)), replace = F)

outlier.point <- engel %>% filter(income %in% unique(outlier.income)) %>% group_by(income) %>% 
  summarise(foodexp = mean(foodexp, na.rm = T), .groups = "drop") %>% as.data.frame()

outlier.dat <- left_join(data.frame(income = outlier.income), outlier.point, by = "income")

outlier.dat$foodexp <- outlier.dat$foodexp + 0.5

dat <- rbind(engel, outlier.dat)


tau.tmp <- c(0.10, 0.25, 0.50, 0.75, 0.90)


res <- list()

for(i in 1:length(tau.tmp)){
  
  tau <- tau.tmp[i]
  
  # robust quantile
  S.Q.MADN <- fit.rob(data = dat, tau = tau, eff = 0.95, sig_type = "MADN", Type = "linear")
  
  # standard quantile
  Quantile <- coef(rq(foodexp ~ income, tau = tau, data = dat))
  
  res[[i]] <- rbind("Quantile" = Quantile, "S.Q" = S.Q.MADN$b)
}

## result

# names(res) <- tau.tmp
# 
# p1 <- ggplot(dat) + geom_point(data = outlier.dat, aes(x = income, y = foodexp), col = "black", size = 0.5, shape = 19) +
#   geom_point(data = engel, aes(x = income, y = foodexp), shape = 1, size = 0.5) + # alpha = 0.4
#   theme_bw() + labs(title = "") + # lower? , x = "Income", y = "Foodexp"
#   geom_abline(slope = res[["0.1"]][1, 2], intercept = res[["0.1"]][1, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.25"]][1, 2], intercept = res[["0.25"]][1, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.5"]][1, 2], intercept = res[["0.5"]][1, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.75"]][1, 2], intercept = res[["0.75"]][1, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.9"]][1, 2], intercept = res[["0.9"]][1, 1], lwd = 0.2, col = "blue") +
#   theme(plot.title = element_text(size = 10, hjust = 0.5, face='bold'),
#         axis.text.x = element_text(size = 6, face='bold'),
#         axis.text.y = element_text(size = 6, hjust=1, face='bold'),
#         axis.title.x = element_text(size = 8, face='bold'),
#         axis.title.y = element_text(size = 8, face='bold'),
#         panel.grid = element_blank(),
#         plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
# 
# 
# p2 <- ggplot(dat) + geom_point(data = outlier.dat, aes(x = income, y = foodexp), col = "black", size = 0.5, shape = 19) +
#   geom_point(data = engel, aes(x = income, y = foodexp), shape = 1, size = 0.5) + # alpha = 0.4
#   theme_bw() + labs(title = "") + # lower? , x = "Income", y = "Foodexp"
#   geom_abline(slope = res[["0.1"]][2, 2], intercept = res[["0.1"]][2, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.25"]][2, 2], intercept = res[["0.25"]][2, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.5"]][2, 2], intercept = res[["0.5"]][2, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.75"]][2, 2], intercept = res[["0.75"]][2, 1], lwd = 0.2, col = "blue") +
#   geom_abline(slope = res[["0.9"]][2, 2], intercept = res[["0.9"]][2, 1], lwd = 0.2, col = "blue") +
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

## Residual plot and qqplot
# lad.fit <- rq(foodexp ~ income, tau = 0.5, data = engel)
# 
# lad.fit$residuals
# lad.fit$fitted.values
# 
# p3 <- ggplot(data.frame(pred = lad.fit$x[,2], resid = lad.fit$residuals)) +
#   geom_point(aes(x = pred, y = resid), size = 0.5) +
#   geom_hline(yintercept = 0, lwd = 0.5) +
#   labs(x = "Income", y = "Residuals", title = "Residual plot") + # lower?
#   ylim(c(-0.25, 0.25)) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 10, hjust = 0.5, face='bold'),
#         axis.text.x = element_text(size = 6, face='bold'),
#         axis.text.y = element_text(size = 6, hjust=1, face='bold'),
#         axis.title.x = element_text(size = 8, face='bold'),
#         axis.title.y = element_text(size = 8, face='bold'),
#         panel.grid = element_blank(),
#         plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
# 
# p4 <- ggplot(data.frame(resid = lad.fit$residuals), aes(sample = resid)) +
#   stat_qq(size = 0.5) +  
#   stat_qq_line(color = "black") + 
#   labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "QQ plot") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 10, hjust = 0.5, face='bold'),
#         axis.text.x = element_text(size = 6, face='bold'),
#         axis.text.y = element_text(size = 6, hjust=1, face='bold'),
#         axis.title.x = element_text(size = 8, face='bold'),
#         axis.title.y = element_text(size = 8, face='bold'),
#         panel.grid = element_blank(),
#         plot.margin = margin(t = 5, r = 15, b = 5, l = 5))
# 
# p34 <- gridExtra::grid.arrange(p3, p4, ncol = 2)
