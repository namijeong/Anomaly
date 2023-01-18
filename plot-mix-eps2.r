
ff = file("sim-mix-eps2.output.txt","r")
V = read.table(file=ff, header=TRUE)

EPS        = V[,1]
MSE.CV     = V[,2]
MSE.3sigma = V[,3]
MSE.HL     = V[,4]
MSE.median = V[,5]
MSE.box    = V[,6]

 plot(NA, NA, xlim=range(EPS),  ylim=c(0, 0.25), type="l",
      xlab=expression(epsilon), ylab="Generalized MSE" )
## lines(EPS, MSE.CV) # CV method is very bad. 
lines(EPS, MSE.3sigma, lty=1)
lines(EPS, MSE.box,    lty=2)
lines(EPS, MSE.HL    , lty=1, col="red")
lines(EPS, MSE.median, lty=2, col="red")
legend(0,0.25, legend=c("3-sigma", "Box-plot", "HL-Shamos", "Median-MAD"), 
       bty="n", lty=c(1,2,1,2), col=c("black","black","red","red") )

#-----------------------------------------------------------------

 plot(NA, NA, xlim=range(EPS),  ylim=c(0, 0.0009), type="l",
      xlab=expression(epsilon), ylab="Generalized MSE" )
## lines(EPS, MSE.CV) # CV method is very bad. 
lines(EPS, MSE.3sigma, lty=1)
lines(EPS, MSE.box,    lty=2)
lines(EPS, MSE.HL    , lty=1, col="red")
lines(EPS, MSE.median, lty=2, col="red")
legend(0.15, 0.0002, legend=c("3-sigma", "Box-plot", "HL-Shamos", "Median-MAD"),
       bty="n", lty=c(1,2,1,2), col=c("black","black","red","red") )



