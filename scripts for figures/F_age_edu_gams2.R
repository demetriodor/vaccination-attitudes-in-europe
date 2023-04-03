### Effect of age on hesitancy and refusal
set.seed(1234)

### GAM models
gam.e <- mgcv::gam(never ~ s(age), data = d1.e,family = binomial,method = "REML")
gam.w <- mgcv::gam(never ~ s(age), data = d1.w,family = binomial,method = "REML")
gam.s <- mgcv::gam(never ~ s(age), data = d1.s,family = binomial,method = "REML")

plotdata.e = plot(gam.e,  trans = plogis, shift = coef(gam.e)[1], seWithMean = TRUE, rug=F)
plotdata.w = plot(gam.w,  trans = plogis, shift = coef(gam.w)[1], seWithMean = TRUE, rug=F)
plotdata.s = plot(gam.s,  trans = plogis, shift = coef(gam.s)[1], seWithMean = TRUE, rug=F)

y.e<-plogis(plotdata.e[[1]]$fit+coef(gam.e)[1])
y.w<-plogis(plotdata.w[[1]]$fit+coef(gam.w)[1])
y.s<-plogis(plotdata.s[[1]]$fit+coef(gam.s)[1])

y.etop<-plogis(plotdata.e[[1]]$fit+2*plotdata.e[[1]]$se+coef(gam.e)[1])
y.ebottom<-plogis(plotdata.e[[1]]$fit-2*plotdata.e[[1]]$se+coef(gam.e)[1])

y.wtop<-plogis(plotdata.w[[1]]$fit+2*plotdata.w[[1]]$se+coef(gam.w)[1])
y.wbottom<-plogis(plotdata.w[[1]]$fit-2*plotdata.w[[1]]$se+coef(gam.w)[1])

y.stop<-plogis(plotdata.s[[1]]$fit+2*plotdata.s[[1]]$se+coef(gam.s)[1])
y.sbottom<-plogis(plotdata.s[[1]]$fit-2*plotdata.s[[1]]$se+coef(gam.s)[1])

###
gam.e2 <- mgcv::gam(hesitant ~ s(age), data = d1.e,family = binomial,method = "REML")
gam.w2 <- mgcv::gam(hesitant ~ s(age), data = d1.w,family = binomial,method = "REML")
gam.s2 <- mgcv::gam(hesitant ~ s(age), data = d1.s,family = binomial,method = "REML")

plotdata.e2 = plot(gam.e2,  trans = plogis, shift = coef(gam.e2)[1], seWithMean = TRUE, rug=F)
plotdata.w2 = plot(gam.w2,  trans = plogis, shift = coef(gam.w2)[1], seWithMean = TRUE, rug=F)
plotdata.s2 = plot(gam.s2,  trans = plogis, shift = coef(gam.s2)[1], seWithMean = TRUE, rug=F)

y.e2<-plogis(plotdata.e2[[1]]$fit+coef(gam.e2)[1])
y.w2<-plogis(plotdata.w2[[1]]$fit+coef(gam.w2)[1])
y.s2<-plogis(plotdata.s2[[1]]$fit+coef(gam.s2)[1])

y.e2top<-plogis(plotdata.e2[[1]]$fit+2*plotdata.e2[[1]]$se+coef(gam.e2)[1])
y.e2bottom<-plogis(plotdata.e2[[1]]$fit-2*plotdata.e2[[1]]$se+coef(gam.e2)[1])

y.w2top<-plogis(plotdata.w2[[1]]$fit+2*plotdata.w2[[1]]$se+coef(gam.w2)[1])
y.w2bottom<-plogis(plotdata.w2[[1]]$fit-2*plotdata.w2[[1]]$se+coef(gam.w2)[1])

y.s2top<-plogis(plotdata.s2[[1]]$fit+2*plotdata.s2[[1]]$se+coef(gam.s2)[1])
y.s2bottom<-plogis(plotdata.s2[[1]]$fit-2*plotdata.s2[[1]]$se+coef(gam.s2)[1])

###
gam.e3 <- mgcv::gam(never ~ s(edu), data = d1.e, family = binomial,method = "REML")
gam.w3 <- mgcv::gam(never ~ s(edu), data = d1.w, family = binomial,method = "REML")
gam.s3 <- mgcv::gam(never ~ s(edu), data = d1.s, family = binomial,method = "REML")

plotdata.e3 = plot(gam.e3,  trans = plogis, shift = coef(gam.e3)[1], seWithMean = TRUE, rug=F)
plotdata.w3 = plot(gam.w3,  trans = plogis, shift = coef(gam.w3)[1], seWithMean = TRUE, rug=F)
plotdata.s3 = plot(gam.s3,  trans = plogis, shift = coef(gam.s3)[1], seWithMean = TRUE, rug=F)

y.e3<-plogis(plotdata.e3[[1]]$fit+coef(gam.e3)[1])
y.w3<-plogis(plotdata.w3[[1]]$fit+coef(gam.w3)[1])
y.s3<-plogis(plotdata.s3[[1]]$fit+coef(gam.s3)[1])

y.e3top<-plogis(plotdata.e3[[1]]$fit+2*plotdata.e3[[1]]$se+coef(gam.e3)[1])
y.e3bottom<-plogis(plotdata.e3[[1]]$fit-2*plotdata.e3[[1]]$se+coef(gam.e3)[1])

y.w3top<-plogis(plotdata.w3[[1]]$fit+2*plotdata.w3[[1]]$se+coef(gam.w3)[1])
y.w3bottom<-plogis(plotdata.w3[[1]]$fit-2*plotdata.w3[[1]]$se+coef(gam.w3)[1])

y.s3top<-plogis(plotdata.s3[[1]]$fit+2*plotdata.s3[[1]]$se+coef(gam.s3)[1])
y.s3bottom<-plogis(plotdata.s3[[1]]$fit-2*plotdata.s3[[1]]$se+coef(gam.s3)[1])

###
gam.e4 <- mgcv::gam(hesitant ~ s(edu), data = d1.e,family = binomial,method = "REML")
gam.w4 <- mgcv::gam(hesitant ~ s(edu), data = d1.w,family = binomial,method = "REML")
gam.s4 <- mgcv::gam(hesitant ~ s(edu), data = d1.s,family = binomial,method = "REML")

plotdata.e4 = plot(gam.e4,  trans = plogis, shift = coef(gam.e4)[1], seWithMean = TRUE, rug=F)
plotdata.w4 = plot(gam.w4,  trans = plogis, shift = coef(gam.w4)[1], seWithMean = TRUE, rug=F)
plotdata.s4 = plot(gam.s4,  trans = plogis, shift = coef(gam.s4)[1], seWithMean = TRUE, rug=F)

y.e4<-plogis(plotdata.e4[[1]]$fit+coef(gam.e4)[1])
y.w4<-plogis(plotdata.w4[[1]]$fit+coef(gam.w4)[1])
y.s4<-plogis(plotdata.s4[[1]]$fit+coef(gam.s4)[1])

y.e4top<-plogis(plotdata.e4[[1]]$fit+2*plotdata.e4[[1]]$se+coef(gam.e4)[1])
y.e4bottom<-plogis(plotdata.e4[[1]]$fit-2*plotdata.e4[[1]]$se+coef(gam.e4)[1])

y.w4top<-plogis(plotdata.w4[[1]]$fit+2*plotdata.w4[[1]]$se+coef(gam.w4)[1])
y.w4bottom<-plogis(plotdata.w4[[1]]$fit-2*plotdata.w4[[1]]$se+coef(gam.w4)[1])

y.s4top<-plogis(plotdata.s4[[1]]$fit+2*plotdata.s4[[1]]$se+coef(gam.s4)[1])
y.s4bottom<-plogis(plotdata.s4[[1]]$fit-2*plotdata.s4[[1]]$se+coef(gam.s4)[1])

###start the plot
offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default


png ('./figures/F_age_edu_gams2.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(2,2), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(4,4,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family = "Quattrocento"
)


# plot 1
plot(x=plotdata.e[[1]]$x, y=y.e, col='red', type='l', ylim=c(0,0.25),  xlim=c(14,85), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine refusal')
lines(x=plotdata.w[[1]]$x, y=y.w, col='blue', line=2, lwd=3*s)
lines(x=plotdata.s[[1]]$x, y=y.s, col='black', line=2, lwd=3*s)

lines(x=plotdata.e[[1]]$x, y=y.etop, col='red', lty=3, lwd=0.5*s)
lines(x=plotdata.e[[1]]$x, y=y.ebottom, col='red', lty=3, lwd=0.5*s)

lines(x=plotdata.w[[1]]$x, y=y.wtop, col='blue', lty=3, lwd=0.5*s)
lines(x=plotdata.w[[1]]$x, y=y.wbottom, col='blue', lty=3, lwd=0.5*s)

lines(x=plotdata.s[[1]]$x, y=y.stop, col='black', lty=3, lwd=0.5*s)
lines(x=plotdata.s[[1]]$x, y=y.sbottom, col='black', lty=3, lwd=0.5*s)

title(ylab='Probability of vaccine refusal', xlab = "Age (in years)", cex.lab=1, line=2)
text ('Eastern E.', x=40, y=0.22, col='red', cex=0.85)
text ('Western E.', x=40, y=0.12, col='blue', cex=0.85)
text ('Southern E.', x=40, y=0.06, col='black', cex=0.85)

# plot 2
plot(x=plotdata.e2[[1]]$x, y=y.e2, col='red', type='l', ylim=c(0,0.5),  xlim=c(14,85), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine hesitancy')
lines(x=plotdata.w2[[1]]$x, y=y.w2, col='blue', line=2, lwd=3*s)
lines(x=plotdata.s2[[1]]$x, y=y.s2, col='black', line=2, lwd=3*s)

lines(x=plotdata.e2[[1]]$x, y=y.e2top, col='red', lty=3, lwd=0.5*s)
lines(x=plotdata.e2[[1]]$x, y=y.e2bottom, col='red', lty=3, lwd=0.5*s)

lines(x=plotdata.w2[[1]]$x, y=y.w2top, col='blue', lty=3, lwd=0.5*s)
lines(x=plotdata.w2[[1]]$x, y=y.w2bottom, col='blue', lty=3, lwd=0.5*s)

lines(x=plotdata.s2[[1]]$x, y=y.s2top, col='black', lty=3, lwd=0.5*s)
lines(x=plotdata.s2[[1]]$x, y=y.s2bottom, col='black', lty=3, lwd=0.5*s)


title(ylab='Probability of vaccine hesitancy', xlab = "Age (in years)", cex.lab=1, line=2)
text ('Eastern E.', x=40, y=0.47, col='red', cex=0.85)
text ('Western E.', x=40, y=0.28, col='blue', cex=0.85)
text ('Southern E.', x=40, y=0.16, col='black', cex=0.85)


# plot 3
plot(x=plotdata.e3[[1]]$x, y=y.e3, col='red', type='l', ylim=c(0,0.25),  xlim=c(14,35), lwd=3*s, xlab = 'Education (age when complete)', ylab='Probability of vaccine refusal')
lines(x=plotdata.w3[[1]]$x, y=y.w3, col='blue', line=2, lwd=3*s)
lines(x=plotdata.s3[[1]]$x, y=y.s3, col='black', line=2, lwd=3*s)

title(ylab='Probability of vaccine refusal', xlab = "Education (age when complete)", cex.lab=1, line=2)
text ('Eastern E.', x=20, y=0.22, col='red', cex=0.85)
text ('Western E.', x=20, y=0.09, col='blue', cex=0.85)
text ('Southern E.', x=20, y=0.05, col='black', cex=0.85)

lines(x=plotdata.e3[[1]]$x, y=y.e3top, col='red', lty=3, lwd=0.5*s)
lines(x=plotdata.e3[[1]]$x, y=y.e3bottom, col='red', lty=3, lwd=0.5*s)

lines(x=plotdata.w3[[1]]$x, y=y.w3top, col='blue', lty=3, lwd=0.5*s)
lines(x=plotdata.w3[[1]]$x, y=y.w3bottom, col='blue', lty=3, lwd=0.5*s)

lines(x=plotdata.s3[[1]]$x, y=y.s3top, col='black', lty=3, lwd=0.5*s)
lines(x=plotdata.s3[[1]]$x, y=y.s3bottom, col='black', lty=3, lwd=0.5*s)

# plot 4
plot(x=plotdata.e4[[1]]$x, y=y.e4, col='red', type='l', ylim=c(0,0.55),  xlim=c(14,35), lwd=3*s, xlab = 'Education (age when complete)', ylab='Probability of vaccine hesitancy')
lines(x=plotdata.w4[[1]]$x, y=y.w4, col='blue', line=2, lwd=3*s)
lines(x=plotdata.s4[[1]]$x, y=y.s4, col='black', line=2, lwd=3*s)

title(ylab='Probability of vaccine hesitancy', xlab = "Education (age when complete)", cex.lab=1, line=2)
text ('Eastern E.', x=20, y=0.47, col='red', cex=0.85)
text ('Western E.', x=20, y=0.21, col='blue', cex=0.85)
text ('Southern E.', x=20, y=0.14, col='black', cex=0.85)

lines(x=plotdata.e4[[1]]$x, y=y.e4top, col='red', lty=3, lwd=0.5*s)
lines(x=plotdata.e4[[1]]$x, y=y.e4bottom, col='red', lty=3, lwd=0.5*s)

lines(x=plotdata.w4[[1]]$x, y=y.w4top, col='blue', lty=3, lwd=0.5*s)
lines(x=plotdata.w4[[1]]$x, y=y.w4bottom, col='blue', lty=3, lwd=0.5*s)

lines(x=plotdata.s4[[1]]$x, y=y.s4top, col='black', lty=3, lwd=0.5*s)
lines(x=plotdata.s4[[1]]$x, y=y.s4bottom, col='black', lty=3, lwd=0.5*s)

dev.off()

### This code tests the significance of the non-linear terms
summary(gam.test<- mgcv::gam(hesitant ~ age + s(age, m=c(2,0)), data=d1.s, method="REML"))