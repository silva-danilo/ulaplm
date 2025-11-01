
# setwd
setwd("/home/posmae/danilo.silva/Rede IME/ulaplm")
#setwd("/mnt/chromeos/removable/danvah/ulaplm")
#setwd("D:/ulaplm")

# load fit_ulaplm
source("R/ulaplm.R")

# read data
dat1 <- read.table("data/hypo.txt", header=T)
dat1$adreno <- factor(dat1$adreno, labels=c("no", "yes"))
dat1$drug1 <- factor(dat1$drug1, labels=c("no", "yes"))
dat1$drug2 <- factor(dat1$drug2, labels=c("none", "predi", "hidro"))

# prep data
X <- model.matrix(~1+adreno+drug1+drug2, data=dat1)
y <- dat1$resp

# b-spline 
k1 <- 8
knots <- seq(min(dat1$dep), max(dat1$dep), length.out=k1+2)
knots <- knots[-c(1, k1 + 2)]
knots <- c(rep(min(dat1$dep), 4), knots, rep(max(dat1$dep), 4))
B1 <- splineDesign(dat1$dep, knots=knots, ord=4, outer.ok=T)
B1 <- scale(B1, scale=F)
B1 <- B1[,-ncol(B1)]
B_list <- list(B1)

# penalty matrix
M1 <- get_penalty(ncol(B1))
M_list <- list(M1)

# model
b1 <- fit_ulaplm(X, B_list, y, M_list)

# summary
b1$beta
sqrt(diag(b1$vcov)[1:5])
b1$alpha
sum(b1$edf) #gam(y~X-1+s(dat1$dep))$edf

# test
w <- abs(b1$beta/sqrt(diag(b1$vcov)[1:5]))
1 - pnorm(w)

# test
I_mat <- qr.solve(qr(b1$vcov))
w <- t(b1$theta[-(1:5)]) %*% I_mat[-(1:5),-(1:5)] %*% b1$theta[-(1:5)]
1 - pchisq(as.numeric(w), sum(b1$edf[-(1:5)]))

# residual
res_ulaplm(b1)

# local influ
Bi_ulaplm(b1, c(2,2))
#dmax_ulaplm(b1, c(1,3))

# plot prep
palet <- viridis(2, 1, direction=-1)[-1]
par(mar=c(4.5,5,1,1), mfrow=c(1,3))

# f(dep) vs dep
ids <- order(dat1$dep)
plot(b1$f1[ids]~dat1$dep[ids], ylab="f(dep)", xlab="dep", ylim=c(-1.1,1.2),
     cex.lab=1.8, cex.axis=1.5, lwd=2, type="l", col=palet[1])
polygon(c(dat1$dep[ids], rev(dat1$dep[ids])),
        c(b1$f1_upp[ids], rev(b1$f1_low[ids])),
        col=adjustcolor(palet[1], alpha=0.3), border=NA)
legend("top", c("all data"), 
       col=palet, cex=1.5, title="", horiz="T", bty="n", y.intersp=0.2)

# model (drop 11)
dat2 <- read.table("data/hypo.txt", header=T)
dat2$adreno <- factor(dat2$adreno, labels=c("no", "yes"))
dat2$drug1 <- factor(dat2$drug1, labels=c("no", "yes"))
dat2$drug2 <- factor(dat2$drug2, labels=c("none", "predi", "hidro"))
dat2 <- dat2[-c(11),]
X <- model.matrix(~1+adreno+drug1+drug2, data=dat2)
y <- dat2$resp
k1 <- 8
knots <- seq(min(dat2$dep), max(dat2$dep), length.out=k1+2)
knots <- knots[-c(1, k1 + 2)]
knots <- c(rep(min(dat2$dep), 4), knots, rep(max(dat2$dep), 4))
B1 <- splineDesign(dat2$dep, knots=knots, ord=4, outer.ok=T)
B1 <- scale(B1, scale=F)
B1 <- B1[,-ncol(B1)]
B_list <- list(B1)
M1 <- get_penalty(ncol(B1))
M_list <- list(M1)
b2 <- fit_ulaplm(X, B_list, y, M_list)

# f(dep) vs dep
ids <- order(dat2$dep)
plot(b2$f1[ids]~dat2$dep[ids], ylab="f(dep)", xlab="dep", ylim=c(-1.1,1.2),
     cex.lab=1.8, cex.axis=1.5, lwd=2, type="l", col=palet[1])
polygon(c(dat2$dep[ids], rev(dat2$dep[ids])),
        c(b2$f1_upp[ids], rev(b2$f1_low[ids])),
        col=adjustcolor(palet[1], alpha=0.3), border=NA)
legend("top", c("drop 11"), 
       col=palet, cex=1.5, title="", horiz="T", bty="n", y.intersp=0.2)

# model (drop 43 and 47)
dat3 <- read.table("data/hypo.txt", header=T)
dat3$adreno <- factor(dat3$adreno, labels=c("no", "yes"))
dat3$drug1 <- factor(dat3$drug1, labels=c("no", "yes"))
dat3$drug2 <- factor(dat3$drug2, labels=c("none", "predi", "hidro"))
dat3 <- dat3[-c(43,47),]
X <- model.matrix(~1+adreno+drug1+drug2, data=dat3)
y <- dat3$resp
k1 <- 8
knots <- seq(min(dat3$dep), max(dat3$dep), length.out=k1+2)
knots <- knots[-c(1, k1 + 2)]
knots <- c(rep(min(dat3$dep), 4), knots, rep(max(dat3$dep), 4))
B1 <- splineDesign(dat3$dep, knots=knots, ord=4, outer.ok=T)
B1 <- scale(B1, scale=F)
B1 <- B1[,-ncol(B1)]
B_list <- list(B1)
M1 <- get_penalty(ncol(B1))
M_list <- list(M1)
b3 <- fit_ulaplm(X, B_list, y, M_list)

# f(dep) vs dep
ids <- order(dat3$dep)
plot(b3$f1[ids]~dat3$dep[ids], ylab="f(dep)", xlab="dep", ylim=c(-1.1,1.2),
     cex.lab=1.8, cex.axis=1.5, lwd=2, type="l", col=palet[1])
polygon(c(dat3$dep[ids], rev(dat3$dep[ids])),
        c(b3$f1_upp[ids], rev(b3$f1_low[ids])),
        col=adjustcolor(palet[1], alpha=0.3), border=NA)
legend("top", c("drop 43 and 47"), 
       col=palet, cex=1.5, title="", horiz="T", bty="n", y.intersp=0.2)

# residual
res_ulaplm(b1)
res_ulaplm(b2)
res_ulaplm(b3)
