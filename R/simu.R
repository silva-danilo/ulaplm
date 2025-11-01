
# setwd
setwd("/home/posmae/danilo.silva/Rede IME/ulaplm")
#setwd("/mnt/chromeos/removable/danvah/ulaplm")
#setwd("D:/ulaplm")

# load fit_ulaplm
source("R/ulaplm.R")

# simu
R <- 500
n1 <- 50
n2 <- 200
n3 <- 800
dat_n1 <- gen_data(n1, R)
dat_n2 <- gen_data(n2, R)
dat_n3 <- gen_data(n3, R)

# fit for n1
fit_n1 <- list()
for(r in 1:R){
  # prep
  X <- dat_n1$X
  B_list <- list(dat_n1$B1, dat_n1$B2)
  yr <- dat_n1[[paste0("y", r)]]
  M_list <- list(dat_n1$M1, dat_n1$M2)
  
  # model
  fitr <- fit_ulaplm(X, B_list, yr, M_list)
  fit_n1[[paste0("fit", r)]] <- fitr
}

# fit for n2
fit_n2 <- list()
for(r in 1:R){
  # prep
  X <- dat_n2$X
  B_list <- list(dat_n2$B1, dat_n2$B2)
  yr <- dat_n2[[paste0("y", r)]]
  M_list <- list(dat_n2$M1, dat_n2$M2)
  
  # model
  fitr <- fit_ulaplm(X, B_list, yr, M_list)
  fit_n2[[paste0("fit", r)]] <- fitr
}

# fit for n3
fit_n3 <- list()
for(r in 1:R){
  # prep
  X <- dat_n3$X
  B_list <- list(dat_n3$B1, dat_n3$B2)
  yr <- dat_n3[[paste0("y", r)]]
  M_list <- list(dat_n3$M1, dat_n3$M2)
  
  # model
  fitr <- fit_ulaplm(X, B_list, yr, M_list)
  fit_n3[[paste0("fit", r)]] <- fitr
}

# prep plot
name <- paste0("fit", 1:R)
palet <- viridis(3, alpha=0.8, direction=-1)[-1]
par(mar=c(4,5,1,1), mfrow=c(1,3), cex.lab=1.8, cex.axis=1.5, pch=19)

# plot b1
beta_1_n1 <- lapply(1:R, function(j) fit_n1[[name[j]]]$beta[1]) 
beta_1_n1 <- unlist(beta_1_n1)
beta_1_n2 <- lapply(1:R, function(j) fit_n2[[name[j]]]$beta[1]) 
beta_1_n2 <- unlist(beta_1_n2)
beta_1_n3 <- lapply(1:R, function(j) fit_n3[[name[j]]]$beta[1]) 
beta_1_n3 <- unlist(beta_1_n3)
range_min <- min(beta_1_n1, beta_1_n2, beta_1_n3)
range_max <- max(beta_1_n1, beta_1_n2, beta_1_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta_1_n1, beta_1_n2, beta_1_n3, 
        ylab=expression(beta[1] ~ "fitted"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"), ylim=c(range_min, range_max))
abline(h=-0.5, lwd=3, col=palet[2])
legend("top", legend=c("true coef."), cex=1.5,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot b2
beta_2_n1 <- lapply(1:R, function(j) fit_n1[[name[j]]]$beta[2]) 
beta_2_n1 <- unlist(beta_2_n1)
beta_2_n2 <- lapply(1:R, function(j) fit_n2[[name[j]]]$beta[2]) 
beta_2_n2 <- unlist(beta_2_n2)
beta_2_n3 <- lapply(1:R, function(j) fit_n3[[name[j]]]$beta[2]) 
beta_2_n3 <- unlist(beta_2_n3)
range_min <- min(beta_2_n1, beta_2_n2, beta_2_n3)
range_max <- max(beta_2_n1, beta_2_n2, beta_2_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta_2_n1, beta_2_n2, beta_2_n3, 
        ylab=expression(beta[2] ~ "fitted"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"), ylim=c(range_min, range_max))
abline(h=1, lwd=3, col=palet[2])
legend("top", legend=c("true coef."), cex=1.5,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot b3
beta_3_n1 <- lapply(1:R, function(j) fit_n1[[name[j]]]$beta[3]) 
beta_3_n1 <- unlist(beta_3_n1)
beta_3_n2 <- lapply(1:R, function(j) fit_n2[[name[j]]]$beta[3]) 
beta_3_n2 <- unlist(beta_3_n2)
beta_3_n3 <- lapply(1:R, function(j) fit_n3[[name[j]]]$beta[3]) 
beta_3_n3 <- unlist(beta_3_n3)
range_min <- min(beta_3_n1, beta_3_n2, beta_3_n3)
range_max <- max(beta_3_n1, beta_3_n2, beta_3_n3)
range_max <- range_max + 0.05*(range_max-range_min)
boxplot(beta_3_n1, beta_3_n2, beta_3_n3,
        ylab=expression(beta[3] ~ "fitted"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"), ylim=c(range_min, range_max))
abline(h=0.5, lwd=3, col=palet[2])
legend("top", legend=c("true coef."), cex=1.5,
       bty="n", horiz=T, y.intersp=0.4, lwd=3, col=palet[2])

# plot a1
alpha_1_n1 <- lapply(1:R, function(j) fit_n1[[name[j]]]$alpha[1]) 
alpha_1_n1 <- unlist(alpha_1_n1)
alpha_1_n2 <- lapply(1:R, function(j) fit_n2[[name[j]]]$alpha[1]) 
alpha_1_n2 <- unlist(alpha_1_n2)
alpha_1_n3 <- lapply(1:R, function(j) fit_n3[[name[j]]]$alpha[1]) 
alpha_1_n3 <- unlist(alpha_1_n3)
boxplot(alpha_1_n1, alpha_1_n2, alpha_1_n3,
        ylab=expression(alpha[1] ~ "fitted"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"))

# plot a2
alpha_2_n1 <- lapply(1:R, function(j) fit_n1[[name[j]]]$alpha[2]) 
alpha_2_n1 <- unlist(alpha_2_n1)
alpha_2_n2 <- lapply(1:R, function(j) fit_n2[[name[j]]]$alpha[2]) 
alpha_2_n2 <- unlist(alpha_2_n2)
alpha_2_n3 <- lapply(1:R, function(j) fit_n3[[name[j]]]$alpha[2]) 
alpha_2_n3 <- unlist(alpha_1_n3)
boxplot(alpha_2_n1, alpha_2_n2, alpha_2_n3,
        ylab=expression(alpha[2] ~ "fitted"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"))

# plot edf
edf_n1 <- lapply(1:R, function(j) sum(fit_n1[[name[j]]]$edf)) 
edf_n1 <- unlist(edf_n1)
edf_n2 <- lapply(1:R, function(j) sum(fit_n2[[name[j]]]$edf)) 
edf_n2 <- unlist(edf_n2)
edf_n3 <- lapply(1:R, function(j) sum(fit_n3[[name[j]]]$edf)) 
edf_n3 <- unlist(edf_n3)
boxplot(edf_n1, edf_n2, edf_n3,
        ylab=expression("edf"), col=palet[1], 
        names=c("n=50", "n=200", "n=800"))

# prep plot
palet <- viridis(3, direction=-1)[-1]
par(mar=c(4.5,5,1,1), mfrow=c(1,3))

# plot f1 with n=50
ids <- order(dat_n1$t1)
range <- c(-1.7,2)
plot(dat_n1$f1[ids]~dat_n1$t1[ids], ylab="f1 with n=50", xlab="t1",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n1[[name[j]]]$f1[ids]~dat_n1$t1[ids], 
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n1$f1[ids]~dat_n1$t1[ids], lwd=3, col=palet[1])
legend("top", c("f1 true", "f1 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)

# plot f1 with n=200
ids <- order(dat_n2$t1)
plot(dat_n2$f1[ids]~dat_n2$t1[ids], ylab="f1 with n=200", xlab="t1",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n2[[name[j]]]$f1[ids]~dat_n2$t1[ids], 
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n2$f1[ids]~dat_n2$t1[ids], lwd=3, col=palet[1])
legend("top", c("f1 true", "f1 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)

# plot f1 with n=800
ids <- order(dat_n3$t1)
plot(dat_n3$f1[ids]~dat_n3$t1[ids], ylab="f1 with n=800", xlab="t1",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n3[[name[j]]]$f1[ids]~dat_n3$t1[ids], 
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n3$f1[ids]~dat_n3$t1[ids], lwd=3, col=palet[1])
legend("top", c("f1 true", "f1 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)

# plot f2 with n=50
ids <- order(dat_n1$t2)
range <- c(-5,2.2)
plot(dat_n1$f2[ids]~dat_n1$t2[ids], ylab="f2 with n=50", xlab="t2",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n1[[name[j]]]$f2[ids]~dat_n1$t2[ids], 
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n1$f2[ids]~dat_n1$t2[ids], lwd=3, col=palet[1])
legend("top", c("f2 true", "f2 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)

# plot f1 with n=200
ids <- order(dat_n2$t2)
plot(dat_n2$f2[ids]~dat_n2$t2[ids], ylab="f2 with n=200", xlab="t2",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n2[[name[j]]]$f2[ids]~dat_n2$t2[ids], 
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n2$f2[ids]~dat_n2$t2[ids], lwd=3, col=palet[1])
legend("top", c("f2 true", "f2 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)

# plot f1 with n=800
ids <- order(dat_n3$t2)
plot(dat_n3$f2[ids]~dat_n3$t2[ids], ylab="f2 with n=800", xlab="t2",
     cex.lab=1.8, cex.axis=1.5, type="n", ylim=range)
for(j in 1:R) lines(fit_n3[[name[j]]]$f2[ids]~dat_n3$t2[ids],
                    col=adjustcolor(palet[2], alpha=0.2))
lines(dat_n3$f2[ids]~dat_n3$t2[ids], lwd=3, col=palet[1])
legend("top", c("f2 true", "f2 fitted"), lwd=3, col=palet, cex=1.5, 
       title="", horiz="T", bty="n", y.intersp=0.2)
