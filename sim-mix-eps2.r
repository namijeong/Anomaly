#
source("https://raw.githubusercontent.com/namijeong/Anomaly/main/anomaly.R")
set.seed(1)

ITER = 10000
## ITER = 10

n    = 50
mu0 =  5; sd0 = 1
mu1 = 15; sd1 = 1

EPS = seq(0, 0.25, by=0.01) 


#========================================================================
ff = file("sim-mix-eps2.output.txt","w")
date(); now <- proc.time()  # checkpoint
HEAD = c("   epsilon", "        CV", "    3sigma", "        HL", "    Median", "       box")
cat(HEAD,"\n", file=ff)
#========================================================================


#========================================================================
for ( j in seq_along(EPS) ) {
    eps = EPS[j]
    MU.CV <- MU.3sigma <- MU.HL <- MU.median <- MU.box <- numeric(ITER)
    SD.CV <- SD.3sigma <- SD.HL <- SD.median <- SD.box <- numeric(ITER)
    
    for ( i in seq_len(ITER) ) { 
    
        # Data setup (mixture model)
        x = rnorm.mix (n, means=c(mu0,mu1), sds=c(sd0,sd1), prob=c(1-eps, eps)) 
        
        #-----------------------------------------------------
        # Perform normal.CV function (default pvalue = 0.05)
        tmp = normal.CV(x, pvalue=0.1)
        MU.CV[i] = mean(tmp$pure)
        SD.CV[i] =   sd(tmp$pure)
        
        #-----------------------------------------------------
        # Perform normal.3sigma.mean function (default factor = 3)
        tmp = normal.3sigma.mean(x)
        MU.3sigma[i] = mean(tmp$pure)
        SD.3sigma[i] =   sd(tmp$pure)
    
        #-----------------------------------------------------
        # Perform normal.3sigma.mean function (default factor = 3)
        tmp = normal.HL(x)
        MU.HL[i] = mean(tmp$pure)
        SD.HL[i] =   sd(tmp$pure)
    
        #-----------------------------------------------------
        # Perform normal.3sigma.mean function (default factor = 3)
        tmp = normal.median(x)
        MU.median[i] = mean(tmp$pure)
        SD.median[i] =   sd(tmp$pure)
    
        #-----------------------------------------------------
        # Perform normal.box function (default factor = 1.5)
        tmp = normal.box(x)
        MU.box[i] = mean(tmp$pure)
        SD.box[i] =   sd(tmp$pure)
    }

    # Generalized MSE 
    MSE.CV     = gMSE(MU.CV,     SD.CV,     mu0, sd0)
    MSE.3sigma = gMSE(MU.3sigma, SD.3sigma, mu0, sd0)
    MSE.HL     = gMSE(MU.HL,     SD.HL,     mu0, sd0)
    MSE.median = gMSE(MU.median, SD.median, mu0, sd0)
    MSE.box    = gMSE(MU.box,    SD.box,    mu0, sd0)

    cat(formatC(c(eps,MSE.CV, MSE.3sigma, MSE.HL, MSE.median, MSE.box), wid=10), "\n", 
        file=ff, append=TRUE)
}
#========================================================================



# ===============================================
# CLOSING
# ===============================================
## close(zz);
cat("\n\n")
cat("\n\n")
date() ; (proc.time()-now)/3600;
##===============================================

