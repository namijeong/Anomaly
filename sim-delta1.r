#
source("https://raw.githubusercontent.com/namijeong/Anomaly/main/anomaly.R")
set.seed(1)


ITER = 1000
n    = 48
no.of.noise = 2
delta = 10
mu0 =  5; sd0 = 1

##################################################################################
MU.CV <- MU.3sigma <- MU.HL <- MU.median <- MU.box <- numeric(ITER)
SD.CV <- SD.3sigma <- SD.HL <- SD.median <- SD.box <- numeric(ITER)

for ( i in seq_len(ITER) ) { 
    # Data setup
    x0 = rnorm(n-no.of.noise, mean=mu0, sd=sd0) # Pure normal data
    
    noise = rep(delta, no.of.noise)
    x1 = c(x0,noise)    # contaminated data
    
    #-----------------------------------------------------
    # Perform normal.CV function (default pvalue = 0.05)
    tmp = normal.CV(x1, pvalue=0.1)
    MU.CV[i] = mean(tmp$pure)
    SD.CV[i] =   sd(tmp$pure)
    
    #-----------------------------------------------------
    # Perform normal.3sigma.mean function (default factor = 3)
    tmp = normal.3sigma.mean(x1)
    MU.3sigma[i] = mean(tmp$pure)
    SD.3sigma[i] =   sd(tmp$pure)

    #-----------------------------------------------------
    # Perform normal.3sigma.mean function (default factor = 3)
    tmp = normal.HL(x1)
    MU.HL[i] = mean(tmp$pure)
    SD.HL[i] =   sd(tmp$pure)

    #-----------------------------------------------------
    # Perform normal.3sigma.mean function (default factor = 3)
    tmp = normal.median(x1)
    MU.median[i] = mean(tmp$pure)
    SD.median[i] =   sd(tmp$pure)

    #-----------------------------------------------------
    # Perform normal.box function (default factor = 1.5)
    tmp = normal.box(x1)
    MU.box[i] = mean(tmp$pure)
    SD.box[i] =   sd(tmp$pure)
}

# Plots 
par(mfrow=c(2,3))
plot(MU.CV, SD.CV,        xlim=c(0,10), ylim=c(0,5))
     abline(v=mu0, h=sd0, col="gold")
plot(MU.3sigma, SD.3sigma,xlim=c(0,10), ylim=c(0,5))
     abline(v=mu0, h=sd0, col="gold")
plot(MU.HL, SD.HL, xlim=c(0,10), ylim=c(0,3))
     abline(v=mu0, h=sd0, col="gold")
plot(MU.median, SD.median, xlim=c(0,10), ylim=c(0,3))
     abline(v=mu0, h=sd0, col="gold")
plot(MU.box, SD.box,      xlim=c(0,10), ylim=c(0,5))
     abline(v=mu0, h=sd0, col="gold")

# Generalized MSE 
gMSE(MU.CV,     SD.CV,     mu0, sd0)
gMSE(MU.3sigma, SD.3sigma, mu0, sd0)
gMSE(MU.HL,     SD.HL,     mu0, sd0)
gMSE(MU.median, SD.median, mu0, sd0)
gMSE(MU.box,    SD.box,    mu0, sd0)

# Generalized variance
gVAR(MU.CV,     SD.CV)
gVAR(MU.3sigma, SD.3sigma)
gVAR(MU.HL,     SD.HL)
gVAR(MU.median, SD.median)
gVAR(MU.box,    SD.box) 



