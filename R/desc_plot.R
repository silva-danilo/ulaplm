
# packs
library(readr)

# setwd
setwd("/home/posmae/danilo.silva/Rede IME/ulaplm")
#setwd("/mnt/chromeos/removable/danvah/ulaplm")
#setwd("D:/ulaplm")

# read dataset
dat <- read.table("data/hypo.txt", header=T)
dat$adreno <- factor(dat$adreno, labels=c("no", "yes"))
dat$drug1 <- factor(dat$drug1, labels=c("no", "yes"))
dat$drug2 <- factor(dat$drug2, labels=c("none", "predi", "hidro"))
dat$resp2 <- log(dat$resp/(1-dat$resp))

# plot
par(mar=c(5,5,1,1), mfrow=c(1,4), cex.lab=1.5, cex.axis=1.3, pch=19)
boxplot(resp2~adreno, data=dat, ylab="logit(resp)")
boxplot(resp2~drug1, data=dat, ylab="logit(resp)")
boxplot(resp2~drug2, data=dat, ylab="logit(resp)")
plot(resp2~dep, data=dat, ylab="logit(resp)")
lines(smooth.spline(y=dat$resp2, dat$dep, df=4), lwd=2)
