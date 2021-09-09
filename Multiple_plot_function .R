p <- c(0.05, 0.1, 0.25, 0.5, 1, 1.5, 2, 2.5, 5)

par(mfrow=c(3,3))

sin.period <- function(p.vector){
  for(i in 1:length(p.vector)){
    xvals <- seq(from=0, to=2*pi, length=100)
    yvals <- sin(xvals/p.vector[i])
    plot(x=xvals, y=yvals, type="l", lty=1,
         xaxs="i", lwd=2)
  }
} 

sin.period(p.vector=p)

