rm(list = ls())
library(MASS)
library(ggplot2)
library(msm)
library(plyr)
library(dplyr)
library(knitr)

############################################### Univariate Distributions #######################################################

################################## Normal
# Normal Distribution
fit_Normal <- function(estimate, dis = "normal", bins = 50, txt = "estimate"){
  model <- MASS::fitdistr(estimate, densfun = paste(dis), lower = -Inf, upper =Inf) 
  hist(estimate, pch=20, breaks = bins, prob=TRUE, main=paste(txt), xlab = "")
  curve(dnorm(x, model$estimate[1], model$estimate[2]), col="red", lwd=2, add=T)
  
  return(model)
}

# truncated Normal Distribution
truncLik <- function(x, mean, sd, lower = 0, upper= Inf, log = FALSE){
  fit <- msm::dtnorm(x, mean, sd, lower = 0, log = FALSE)
  if(log){ 
    log(fit)
  } 
  else{
    fit
  }
}

fit_trunc_Normal <- function(estimate, mean, sd, bins = 50, txt = "estimate",
                             method = "BFGS", lower = 0, upper = Inf){
  model <- fitdistr(estimate, truncLik,
                    start = list(mean = mean, sd = sd),  method = paste(method))
  hist(estimate, pch=20, breaks = bins, prob=TRUE, main=paste(txt), xlab = "")
  curve(dtnorm(x, model$estimate[1], model$estimate[2], lower=0),
        col="red", lwd=2, add=T)
  return(model)
}


################################## Gamma
fit_Gamma <- function(estimate, txt = "estimate", breaks = 100){
  fit_params <- fitdistr(estimate,"gamma")
  hist(estimate, freq = FALSE, main = paste(txt), col = "grey", breaks = breaks)
  lines(density(estimate, from = 0, to = max(estimate)), col = "black", lwd = 1.5)
  curve(dgamma(x, shape = fit_params$estimate["shape"], rate = fit_params$estimate["rate"]),
        from = 0, to = max(estimate), add = TRUE, col="red", lwd = 1.5)
  legend("topright", legend = c("Fit", "Data"),
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  return(fit_params)
}

################################## Lognormal
fit_log <- function(estimate, txt = "estimate", breaks = 100){
  fit_params <- fitdistr(estimate,"lognormal")
  hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
       col = "grey", breaks = breaks)
  legend("topright", legend = c("Fit", "Data"), 
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  curve(dlnorm(x, fit_params$estimate['meanlog'], fit_params$estimate['sdlog']),
        add = TRUE, col = "red", lwd = 1.5)
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  
  return(fit_params)
}

################################## truncated t
dtt <- function(x, location, scale, df, lower, upper, log = FALSE)
  # Used with truncated t prior
{
  logdens <- ifelse(x >= lower & x <= upper,
                    dt((x - location) / scale, df = df, log = TRUE) - log(scale),
                    -Inf)
  logconstant <- log(pt((upper - location) / scale, df = df) -
                       pt((lower - location) / scale, df = df))
  out <- logdens - logconstant
  
  if ( ! log) {
    out <- exp(out)
  }
  
  return(out)
}


rtt <- function(n,location,scale,df,lower,upper,log=FALSE)
  # Used with truncated t prior
{
  u <- runif(n)
  p <- pt((lower - location) / scale, df = df) +
    u * (pt((upper - location) / scale, df = df) -
           pt((lower - location) / scale, df = df))
  out <- qt(p = p, df = df) * scale + location
  return(out)
}

trunctLik <- function(x, location, scale, df, log = FALSE){
  fit <- dtt(x, location = location, scale = scale, df = df, lower = 0, upper = Inf, log = FALSE)
  if(log){ 
    log(fit)
  } 
  else{
    fit
  }
}

fit_trunc_t <- function(estimate, location, scale, df,  bins = 50, txt = "estimate",
                        method = "BFGS", lower = 0, upper = Inf){
  model <- fitdistr(estimate, trunctLik,
                    start = list(location = location, scale = scale, df = df),
                    method = paste(method))
  
  
  hist(estimate, freq = FALSE, main = paste(txt), col = "grey", breaks = 80)
  lines(density(estimate, from = 0, to = max(estimate)), col = "black", lwd = 1.5)
  legend("topright", legend = c("Fit", "Data"),
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  curve(dtt(x, location= model$estimate[1],
            scale=model$estimate[2], df=model$estimate[3],
            lower = 0, upper = Inf), col="red", lwd=2, add=T)
  
  
  return(model)
}

################################## t
fit_t <- function(estimate, txt = "estimate", breaks = 100){
  fit_params <- fitdistr(estimate, "t")
  hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
       col = "grey", breaks = breaks)
  legend("topright", legend = c("Fit", "Data"), 
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  curve(dtt(x, fit_params$estimate['df'], fit_params$estimate['m'], fit_params$estimate['s'], lower = -Inf, upper = Inf),
        add = TRUE, col = "red", lwd = 1.5)
  lines(density(estimate, from = -50), col = "black", lwd = 1.5)
  
  return(fit_params)
}

################################## truncated t for Bias
trunctLik_bias <- function(x, location, scale, df, log = FALSE){
  fit <- dtt(x, location = location, scale = scale, df = df, lower = 0, upper = 1, log = FALSE)
  if(log){ 
    log(fit)
  } 
  else{
    fit
  }
}

fit_trunc_t_bias <- function(estimate, location, scale, df,  bins = 50, txt = "estimate",
                             method = "BFGS", lower = 0, upper = Inf){
  model <- fitdistr(estimate, trunctLik_bias,
                    start = list(location = location, scale = scale, df = df),
                    method = paste(method))
  
  
  hist(estimate, freq = FALSE, main = paste(txt), col = "grey", breaks = 80)
  lines(density(estimate, from = 0, to = max(estimate)), col = "black", lwd = 1.5)
  legend("topright", legend = c("Fit", "Data"),
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  curve(dtt(x, location= model$estimate[1],
            scale=model$estimate[2], df=model$estimate[3],
            lower = 0, upper = 1), col="red", lwd=2, add=T)
  
  
  return(model)
}



################################## Weibull
fit_weibull <- function(estimate, txt = "estimate", breaks = 100){
  fit_params <- fitdistr(estimate,"weibull")
  hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
       col = "grey", breaks = breaks)
  legend("topright", legend = c("Fit", "Data"), 
         col = c("red", "black"), lty=1, cex=0.8, bty = "n")
  curve(dweibull(x, shape = fit_params$estimate['shape'], scale = fit_params$estimate['scale']),
        add = TRUE, col = "red", lwd = 1.5)
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  
  return(fit_params)
}

fitTable <- function(estimate, txt = "Goodness of Fit"){
  n <- broom::glance(MASS::fitdistr(estimate,"normal"))
  g <- broom::glance(MASS::fitdistr(estimate,"gamma"))
  ln <- broom::glance(MASS::fitdistr(estimate,"lognormal"))
  w <- broom::glance(MASS::fitdistr(estimate,"weibull"))
  exp <- broom::glance(MASS::fitdistr(estimate,"exponential"))
  table <- rbind(n, g, ln, w, exp)
  rownames(table) <- c("normal", "gamma", "lognormal", "weibull", "exponential")
  kableExtra::kable(table, caption = paste(txt))
  table
}




############################################################### Mixture Distributions #########################################################################

########################### Normal Mix
dnormnorm <- function(x, mean, sd, mean1, sd1, prob, log=FALSE) {
  r <- prob*dnorm(x, mean, sd) + (1-prob)*dnorm(x, mean1, sd1)
  if(log){ 
    log(r)
  } 
  else{
    r
  }
}


mix_normal <- function(estimate,  mean, sd, mean1, sd1, prob, txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dnormnorm,
                  start = list(mean = mean, sd = sd, mean1 = mean1,
                               sd1 = sd1, prob = prob),
                  lower = c(-Inf, 0, -Inf, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright",
         legend = c("Mixture", "Data", "Normal (1st comp)", "Normal (2nd comp)"), 
         col = c("blue", "black", "green", "red"),
         lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dnormnorm(x, mean = mix$estimate[1], sd = mix$estimate[2],
                 mean1 = mix$estimate[3], sd1 = mix$estimate[4],
                 prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dnorm(x, mean = mix$estimate[1], sd = mix$estimate[2]),
        add = TRUE, col = "green")
  curve(dnorm(x, mean = mix$estimate[3], sd = mix$estimate[4]),
        add = TRUE, col = "red")
  
  stat <- cbind(loglik, AIC, BIC)
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}

########################### functions for truncated Normal Mix
dtruncnorm <- function(x, mean, sd, mean1, sd1, prob, log=FALSE) {
  r <- prob*dtnorm(x, mean, sd, lower = 0) + (1-prob)*dtnorm(x, mean1, sd1, lower = 0)
  if(log){ 
    log(r)
  } 
  else{
    r
  }
}


mix_tnorm <- function(estimate,  mean, sd, mean1, sd1, prob, txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dtruncnorm,
                  start = list(mean = mean, sd = sd,
                               mean1 = mean1, sd1 = sd1, prob = prob),
                  lower = c(-Inf, 0, -Inf, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright",
         legend = c("Mixture", "Data", "Normal (1st comp)", "Normal (2nd comp)"), 
         col = c("blue", "black", "green", "red"),
         lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dtruncnorm(x, mean = mix$estimate[1], sd = mix$estimate[2],
                  mean1 = mix$estimate[3], sd1 = mix$estimate[4],
                  prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dtnorm(x, mean = mix$estimate[1], sd = mix$estimate[2], lower = 0),
        add = TRUE, col = "green")
  curve(dtnorm(x, mean = mix$estimate[3], sd = mix$estimate[4], lower = 0),
        add = TRUE, col = "red")
  
  stat <- cbind(loglik, AIC, BIC)
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}

########################### Gamma Mix
dgammagamma <- function(x, shape, rate, shape1, rate1, prob, log=FALSE) {
  r <- prob*dgamma(x, shape, rate) + (1-prob)*dgamma(x, shape1, rate1)
  if(log){
    log(r)
  }
  else{
    r
  }
}


mix_gamma <- function(estimate, shape, rate, shape1, rate1, prob,
                      txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dgammagamma,
                  start = list(shape = shape,
                               rate = rate, shape1 = shape1, rate1 = rate1,
                               prob = prob),
                  lower = c(0, 0, 0, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright", legend = c("Mixture", "Data", "Gamma1", "Gamma2"), 
         col = c("blue", "black", "green", "red"), lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dgammagamma(x, shape = mix$estimate[1], rate = mix$estimate[2],
                   shape1 = mix$estimate[3], rate1 = mix$estimate[4],
                   prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dgamma(x, shape = mix$estimate[1], rate = mix$estimate[2]),
        add = TRUE, col = "green")
  curve(dgamma(x, shape = mix$estimate[3], rate = mix$estimate[4]),
        add = TRUE, col = "red")
  
  stat <- cbind(loglik, AIC, BIC)
  
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}



########################### Normal and Gamma Mix

dnormgammamix <- function(x,shape,rate,mean,sd,prob,log=FALSE) {
  r <- prob*dgamma(x,shape,rate)+(1-prob)*dnorm(x,mean,sd)
  if(log){
    log(r)
  }
  else{
    r
  }
}


mix_gamma_norm <- function(estimate, shape, rate, mean, sd, prob, txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dnormgammamix,
                  start = list(shape = shape,rate = rate,
                               mean = mean, sd = sd, prob = prob),
                  lower = c(0, 0, -Inf, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright", legend = c("Mixture", "Data", "Gamma (1st comp)",
                                "Normal (2nd comp)"), 
         col = c("blue", "black", "green", "red"), lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dnormgammamix(x, shape = mix$estimate[1], rate = mix$estimate[2],
                     mean = mix$estimate[3], sd = mix$estimate[4],
                     prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dnorm(x, mean = mix$estimate[3], sd = mix$estimate[4]),
        col = "red", add = TRUE)
  curve(dgamma(x, shape = mix$estimate[1], rate = mix$estimate[2]),
        col = "green", add = TRUE)
  
  stat <- cbind(loglik, AIC, BIC)
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}

######################## Truncated Normal and Gamma Mix
dtnormgammamix <- function(x,shape,rate,mean,sd,prob,log=FALSE) {
  r <- prob*dgamma(x,shape,rate)+(1-prob)*dtnorm(x,mean,sd, lower = 0)
  if(log){
    log(r)
  }
  else{
    r
  } 
}


mix_gamma_tnorm <- function(estimate, shape, rate, mean, sd, prob, txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dtnormgammamix,
                  start = list(shape = shape,rate = rate,
                               mean = mean, sd = sd, prob = prob),
                  lower = c(0, 0, -Inf, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright", legend = c("Mixture", "Data", "Gamma (1st comp)",
                                "Normal (2nd comp)"), 
         col = c("blue", "black", "green", "red"), lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dtnormgammamix(x, shape = mix$estimate[1], rate = mix$estimate[2],
                      mean = mix$estimate[3], sd = mix$estimate[4],
                      prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dtnorm(x, mean = mix$estimate[3], sd = mix$estimate[4], lower = 0),
        col = "red", add = TRUE)
  curve(dgamma(x, shape = mix$estimate[1], rate = mix$estimate[2]),
        col = "green", add = TRUE)
  stat <- cbind(loglik, AIC, BIC)
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}

########################### Lognormal and normal Mix
# Functions for Mixture with Normal
dnormlnmix <- function(x, meanlog, sdlog, mean, sd, prob, log=FALSE) {
  r <- prob*dlnorm(x, meanlog, sdlog) + (1-prob)*dnorm(x, mean, sd)
  if(log){
    
    log(r)
  }
  else{ 
    r
  }
}


mix_ln_norm <- function(estimate, meanlog, sdlog, mean, sd, prob, txt,
                        method = "BFGS"){
  
  mix <- fitdistr(estimate, dnormlnmix,
                  start = list(meanlog = meanlog,
                               sdlog = sdlog, mean = mean, sd = sd, prob = prob),
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright",
         legend = c("Mixture", "Data", "lognorm (1st comp)", "normal (2nd comp)"),
         col = c("blue", "black", "green", "red"), lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dnormlnmix(x, meanlog = mix$estimate[1], sdlog = mix$estimate[2],
                  mean = mix$estimate[3], sd = mix$estimate[4],
                  prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dlnorm(x, meanlog = mix$estimate[1], sdlog = mix$estimate[2]),
        add = TRUE, col = "green")
  curve(dnorm(x, mean = mix$estimate[3], sd = mix$estimate[4]),
        add = TRUE, col = "red")
  
  stat <- cbind(loglik, AIC, BIC)
  
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}

########################### Lognormal and truncated normal Mix
dtnormlnmix <- function(x, meanlog, sdlog, mean, sd, prob, log=FALSE) {
  r <- prob*dlnorm(x, meanlog, sdlog) + (1-prob)*dtnorm(x, mean, sd, lower = 0)
  if(log){
    
    log(r)
  }
  else{ 
    r
  }
}


mix_ln_tnorm <- function(estimate, meanlog, sdlog, mean, sd, prob,
                         txt, method = "L-BFGS"){
  
  mix <- fitdistr(estimate, dtnormlnmix,
                  start = list(meanlog = meanlog,sdlog = sdlog, mean = mean,
                               sd = sd, prob = prob),
                  lower = c(-Inf, 0, -Inf, 0, 0),
                  upper = c(Inf, Inf, Inf, Inf, 1), 
                  method = paste(method))
  
  loglik <- mix$loglik
  AIC <- AIC(mix)
  BIC <- BIC(mix)
  
  plot <- hist(estimate, freq = FALSE, main = paste(txt), ylab = " Density",
               col = "grey", breaks = 100)
  legend("topright",
         legend = c("Mixture", "Data", "lognorm (1st comp)", "normal (2nd comp)"), 
         col = c("blue", "black", "green", "red"), lty=1, cex=0.8, bty = "n")
  lines(density(estimate, from = 0), col = "black", lwd = 1.5)
  x <- seq(min(estimate), max(estimate), length.out = 1000)
  y <- dtnormlnmix(x, meanlog = mix$estimate[1], sdlog = mix$estimate[2],
                   mean = mix$estimate[3], sd = mix$estimate[4],
                   prob = mix$estimate[5])
  lines(x, y, type = "l", col = 'blue', lwd = 2)
  curve(dlnorm(x, meanlog = mix$estimate[1], sdlog = mix$estimate[2]),
        add = TRUE, col = "green")
  curve(dtnorm(x, mean = mix$estimate[3], sd = mix$estimate[4], lower = 0),
        add = TRUE, col = "red")
  
  stat <- cbind(loglik, AIC, BIC)
  
  return(list(par = mix, loglik = loglik, AIC = AIC, BIC = BIC,
              stat = stat, plot = plot))
}


wAIC <- function (x) {
  x <- x[!is.na(x)]
  delta.aic <- x - min(x, na.rm = TRUE)
  rel.LL <- exp(-0.5 * delta.aic)
  sum.LL <- sum(rel.LL, na.rm = TRUE)
  weights.aic <- rel.LL/sum.LL
  
  return(list(deltaAIC = delta.aic, rel.LL = rel.LL, weights = weights.aic))
}
