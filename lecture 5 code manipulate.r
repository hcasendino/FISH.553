#===============In-class exercise 2======================
#Lecture 5, FISH553 Advanced R
#Trevor A. Branch
#========================================================


#======using sliders to find the best fit=====================
#Now we will play around with different values of 
#Linfinity, K, and sigma to see which combination 
#produces the LOWEST negative log-likelihood
#==================================================================

#====VB.like.sliders
VB.like.sliders <- function(gender, 
                            Linfinity, K, sigma,
                            add.curve, add.lines, 
                            show.NLL=T) {
   filename="Data\\LengthAge.csv"
   t0 <- 0
   maxD.sigma <- 10
   LA <- read.csv(filename)
   ages <- LA[LA$Gender==gender,]$Ages
   lengths <- LA[LA$Gender==gender,]$Lengths
   xlim <- c(0,1.1*max(ages))
   ylim <- c(0,1.1*max(lengths))
   plot(x=ages, y=lengths, 
        ylim=ylim, xaxs="i",yaxs="i", xlim=xlim,
        xlab="Age (yr)", ylab="Length (cm)", pch=19,col="#FF0000", 
        cex=0.8)
   xvals <- seq(xlim[1], xlim[2], length.out=200)
   #all operations below are vector-based, yvals is vector
   yvals <- Linfinity*(1-exp(-K*(xvals-t0)))  
   lines(x=xvals, y=yvals, lty=1, lwd=2, col="black")
   
   
   #loop through all observed ages
   if (add.curve==T) {
      #plot normal density at each model prediction
      obs.ages <- sort(unique(ages))
      for (i in 1:length(obs.ages)) {
         xx <- as.double( rep(obs.ages[i],200))  #ages vector
         yy <- seq(ylim[1], ylim[2], length.out=200)
         #the VB model-predicted mean length at that age
         model.pred <- Linfinity*(1-exp(-K*(obs.ages[i]-t0)))
         #the highest point on the normal curve
         #note: to compare curves with different sigma, I use the 
         #same sigma for all curves here
         maxD <- dnorm(x=model.pred,mean=model.pred,sd=maxD.sigma)
         #likelihood at that age
         dens <- dnorm(x=yy,mean=model.pred,sd=sigma)
         #draw the normal density at each age, truncate at density
         #smaller than 0.01 of maximum density
         plot.dens <- xx+dens/(1.2*maxD)
         lines(x=plot.dens[dens>0.01*maxD], 
               y=yy[dens>0.01*maxD], type="l", col="gray50")
         #draw the zero density line
         lines(x=xx[dens>0.01*maxD], 
               y=yy[dens>0.01*maxD], type="l", col="gray50")
      }
   }
   #add line from each data point to the density curve
   if (add.lines==T) {
      #predicted length for all observed ages
      model.predL <- Linfinity*(1-exp(-K*(ages-t0)))
      #max plotted density
      maxD <- dnorm(x=model.predL,mean=model.predL,sd=maxD.sigma)
      #density there
      line.dens <- dnorm(x=lengths, mean=model.predL, sd=sigma)
      #adjusted density for plotting nicely
      line.dens.adj <- line.dens/(1.2*maxD)
      segments(x0=ages,x1=ages+line.dens.adj, y0=lengths, y1=lengths)
   }
   #calculate and print the negative log-likelihood
   if (show.NLL==T) {
      #predicted length for all observed ages
      model.predL <- Linfinity*(1-exp(-K*(ages-t0)))
      ndata <- length(ages)
      NLL <- 0.5*ndata*log(2*pi) + ndata*log(sigma)
      NLL <- NLL + 1/(2*sigma*sigma) * sum((lengths-model.predL)^2)
      text(x=0.6*diff(xlim), y=0.2*diff(ylim),
           labels=paste("-lnL =",round(NLL,2)))
      text(x=0.6*diff(xlim), y=0.1*diff(ylim),
           labels=paste("Likelihood =",signif(exp(-1*NLL),3)))
   }
}
#using the manipulate package
par(mfrow=c(1,1))
par(oma=c(0,0,0,0), mar=c(4.5,4.5,0.5,0.5))

require(manipulate)  #need this package

manipulate(VB.like.sliders(gender, Linfinity, K, sigma,
                           add.curve, add.lines), 
   gender = picker("Male","Female"),
   Linfinity = slider(min=40,max=120,initial=90,step=0.001), 
   K = slider(min=0.05,max=0.7,initial=0.3, step=0.001),
   sigma = slider(min=2,max=30, initial=7, step=0.001),
   add.curve = checkbox(TRUE, "Add curve"),
   add.lines = checkbox(TRUE, "Add lines"))

#males best solutions
Linf= 
K=
sigma=
-lnL=

#females best solutions
Linf=
K=
sigma=
-lnL=