#Figures for July 2014 version of paper

BetaBinStats=read.table("Northern/BetaBinStats.txt",header=T)



# Figure 1 ------------

tiff(filename="MS/Figure 1.tif", 600,700, )

par(mfrow=c(3,2), mar=c(5.1, 4.5, 4.1, 2.1))
# A
hist(BetaBinStats$Range, main=NA, xlab="Oc- number of sites occupied", ylab="# of species", breaks=20, cex.lab=2.0, cex.axis=2.0)
title("                                        A", cex=5)

# B
hist(BetaBinStats$meanFreqnon0/120, main=NA, xlab="Fr- mean frequency when present", ylab="# of species", breaks=20, cex.lab=2.0, cex.axis=2.0)
title("                                        B", cex=5)

# C

plot(BetaBinStats$Range, BetaBinStats$meanFreqnon0/120,xlab="Oc- number of sites occupied",ylab="Fr",log="xy",asp=1, axes=F, cex.lab=2.0, cex.axis=2.0)
Axis(side=2, cex.lab=2.0, cex.axis=2.0)
Axis(side=1, cex.lab=2.0, cex.axis=2.0)
title("                                        C", cex=5)

RangeMeanFreq.lm=lm(log(meanFreqnon0/120)~log(Range), data=BetaBinStats)
xrange=range(BetaBinStats$Range)
x=data.frame(Range=seq(from=xrange[1], to=xrange[2], by=0.5))
y=predict.lm(RangeMeanFreq.lm, newdata=x)
lines(x$Range,exp(y), lwd=2)
cor(log(BetaBinStats$Range), log(BetaBinStats$meanFreqnon0))

# D
hist(BetaBinStats$HetIndex, main=NA, xlab=expression(paste('I'[het], " - Heterogeneity Index")), ylab="# of species", breaks=20, cex.lab=2.0, cex.axis=2.0)
title("                                        D", cex=5)

# E

plot(BetaBinStats$p, BetaBinStats$theta,xlab="p",ylab=expression(theta),log="xy",asp=1, axes=F, cex.lab=2.0, cex.axis=2.0)
Axis(side=2, cex.lab=2.0, cex.axis=2.0)
Axis(side=1, cex.lab=2.0, cex.axis=2.0)
# plot(BetaBinStats$theta, BetaBinStats$p,ylab="p",xlab=expression(theta),log="xy",asp=1, cex.lab=2.0, cex.axis=2.0)

ptheta.lm=lm(log(theta)~log(p), data=BetaBinStats)
xrange=range(BetaBinStats$p)
x=data.frame(p=seq(from=xrange[1], to=xrange[2], by=0.005))
y=predict.lm(ptheta.lm, newdata=x)
lines(x$p,exp(y), lwd=2)
cor(log(BetaBinStats$p), log(BetaBinStats$theta))
title("                                        E", cex=5)
dev.off()
