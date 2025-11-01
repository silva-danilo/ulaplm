
# packs
library(viridis)

# density 
dul <- function(y, mu){
  d <- (((1-mu)^2)/(mu*(1-y)^3))*exp(-y*(1-mu)/(mu*(1-y)))
  d[y<=0] <- 0
  d[y>=1] <- 0
  d
}

# prep
palet <- viridis(6, direction=-1)[-1]
par(mar=c(4.5,4.5,1,1), mfrow=c(1,1))

# plot
mu <- c(0.1, 0.3, 0.5, 0.7, 0.9)
dul_y <- function(y) dul(y, mu=mu[1])
curve(dul_y, ylab="density", xlab="y", cex.lab=1.5, cex.axis=1.2, 
      lwd=2, ylim=c(0,12.5), xlim=c(0.01,0.99), col=palet[1], n=1e4)
for(i in 2:length(mu)){
  dul_y <- function(y) dul(y, mu=mu[i])
  curve(dul_y, add=TRUE, lwd=2, col=palet[i], n=1e4)
}
legend("top", legend=mu, cex=1.2, bty="n", horiz=T, col=palet, lwd=rep(2,5),
       title=expression("Values of" ~ mu ~ ":"),inset=c(0.05))
