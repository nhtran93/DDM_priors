# DDM
rm(list = ls())
library(msm)
load("data/parameter_distributions.RData")
load("data/DDM_cleaned.RData")

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

postscript(file = "plots/DDM_Priors.eps", onefile = TRUE, horizontal = FALSE,
           width = 11, height = 11)
par(mfrow = c (4, 2))
#layout(matrix(c(1,2,3,4, 5, 6, 7, 7), 4, 2, byrow = TRUE))
# v
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(v$v, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 0.3),
          xlim = c(-10, 20), axes = FALSE, col = "grey", breaks = 80)
axis(1, seq(-10, 20, by = 10))
axis(2, seq(0, 0.3, by = .1))
rug(v$v)
mtext(paste("v (N = ", length(v$v), ")", sep = ""),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtt(x, location = parameter_distributions$v$location,
          scale = parameter_distributions$v$scale,
          df =  parameter_distributions$v$df, lower = -Inf, upper = Inf),
      col = "#E31A1C", lwd = 3, add=T)

# a
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(a$a, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 1),
          xlim = c(0, 6), axes = FALSE, col = "grey", breaks = 60)
axis(1, seq(0, 6, by = 2))
axis(2, seq(0, 1, by = 0.5))
rug(a$a)
mtext(paste("a (N = ", length(a$a), ")", sep = ""),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dgamma(x, parameter_distributions$a$shape, parameter_distributions$a$rate),
      col = "#E31A1C", lwd = 3, add = TRUE)


# Mirrored zr
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(zr_mirrored, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 10),
          xlim = c(0, 1), axes = FALSE, col = "grey", breaks = 20)
axis(1, seq(0, 1, by = .25))
axis(2, seq(0, 10, by = 2))
rug(zr_mirrored)
mtext(paste("Mirrored zr  (N = ", length(zr_mirrored), ")", sep = ""),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtt(x, location =  parameter_distributions$zr_mirrored$location,
          parameter_distributions$zr_mirrored$scale,
          df = parameter_distributions$zr_mirrored$df, lower = 0, upper = 1),
      col = "#E31A1C", lwd = 3, add = TRUE)

# Ter
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(ter$ter, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 5),
          xlim = c(0, 3), axes = FALSE, col = "grey", breaks = 50)
axis(1, seq(0, 3, by = 1))
axis(2, seq(0, 6, by = 1))
rug(ter$ter)
mtext(expression(paste(T[er], " (N = 857)", sep = "")),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtt(x, location =  parameter_distributions$ter$location,
          parameter_distributions$ter$scale,
          df = parameter_distributions$ter$df, lower = 0, upper = Inf),
      col = "#E31A1C", lwd = 3, add = TRUE)

# sv

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(sv$sv, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 1), xlim = c(0, 5), col = "grey", breaks = 20)
axis(1, seq(0, 5, by = 1))
axis(2, seq(0, 1, by = 0.5))
rug(sv$sv)
mtext(expression(paste(s[v], " (N = 317)", sep = "")),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtnorm(x, parameter_distributions$sv$mean, parameter_distributions$sv$sd, lower = 0),
      col = "#E31A1C", lwd = 3, add=T)

# relative sz
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2 ,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(sz_relative$sz_relative, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 3),
          xlim = c(0, 1), axes = FALSE, col = "grey", breaks = 30)
axis(1, seq(0, 2, by = 1))
axis(2, seq(0, 3, by = 1))
rug(sz_relative$sz_relative)
mtext(expression(paste("Relative ", s[z], " (N = 278)", sep = "")),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtnorm(x, parameter_distributions$relative_sz$mean,
             parameter_distributions$relative_sz$sd, lower = 0, upper = 1),
      col = "#E31A1C", lwd = 3, add=T)

# ster
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2,
    font.lab = 1.5, cex.axis = 1.8, bty = "n", las = 1.5)
h <- hist(ster$ster, freq = FALSE, main = "", xlab = "", ylab = " Density",
          ylim = c(0, 8),
          xlim = c(0, 1), axes = FALSE, col = "grey", breaks = 100)
axis(1, seq(0, 1, by = 0.5))
axis(2, seq(0, 8, by = 2))
rug(ster$ster)
mtext(expression(paste(s[T][er], " (N = 352)", sep = "")),
      side = 1, line = 4, cex = 1.7, font = 1)
curve(dtt(x, parameter_distributions$ster$location,
          parameter_distributions$ster$scale,
          parameter_distributions$ster$df, lower = 0, upper = Inf),
      col = "#E31A1C", lwd = 3, add=T)

dev.off()




