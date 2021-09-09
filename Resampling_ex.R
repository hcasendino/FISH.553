#==========FISH 553 Lecture 3 Quiz=======================

#==hindex function
#calculates h index (Hirsch 2005)
#Hirsch JE 2005 An index to quantify an individual's 
#scientific research output. PNAS 102:16569-16572
#"A scientist has index h if h of his or her Np papers 
#have >=h citations each and the other (Np-h) 
#papers have <=h citations"
hindex <- function(citation.vec) {
   sorted <- sort(citation.vec,decreasing=T)
   paper.vec <- 1:length(citation.vec)
   hvalue <- sum(paper.vec <= sorted)
   return(hvalue)
}
hindex(c(5,4,3,2,1,1,1,1))
hindex(c(10,6,6,6,6,6,6,6,6,6,6))
hindex(c(1,4,3,2,5,4,0,0,0,0,0))
hindex(c(1012, 10, 5,4,3,1))

#Use resampling inference to determine if one journal has a higher
#impact than the other. The test-statistics is the h-index

#==Read in the data
ICES <- read.csv("ICES.csv")
CJFAS <- read.csv("CJFAS.csv")

####METHOD 1
niter <- 10000
CJFAS.better <- vector(length=niter)
for (i in 1:niter) {
   X <- hindex(sample(CJFAS$Citations, 100, replace=F))
   Y <- hindex(sample(ICES$Citations, 100, replace=F))
   CJFAS.better[i] <- X>Y
}
#p-value: is CJFAS better than the other?
sum(CJFAS.better)/niter

####METHOD 2
niter <- 10000
num.better <- 0
for (i in 1:niter) {
   X <- hindex(sample(CJFAS$Citations, 100, replace=F))
   Y <- hindex(sample(ICES$Citations, 100, replace=F))
   if (X > Y) {
      num.better <- num.better+1
   }
}
#p-value: is CJFAS better than the other?
num.better/niter


#============h-index next method
hindex2 <- function(citation.vec) {
   sorted <- sort(citation.vec,decreasing=T)
   i <- 1
   while (i <= citation.vec[i]) {
      i <- i+1
   }
   return(i-1)
}
hindex2(c(5,4,3,2,1,1,1,1))
hindex2(c(10,6,6,6,6,6,6,6,6,6,6))
hindex2(c(1,4,3,2,5,4,0,0,0,0,0))
hindex2(c(1012, 10, 5,4,3,1))


#==========extra calculations....  plots etc.
niter <- 10000
sampleh <- matrix(nrow=niter, ncol=2, dimnames=list(NULL,c("ICES","CJFAS")))
for (i in 1:niter) {
    sampleh[i,1] <- 
       hindex(sample(ICES$Citations, 100, replace=F))
    sampleh[i,2] <- 
       hindex(sample(CJFAS$Citations, 100, replace=F))
}

#p-value: is one better than the other?
sum(sampleh[,"CJFAS"] > sampleh[,"ICES"])/niter
#No, only in 65% of occurrences is CJFAS better

head(sampleh)
table(sampleh[,"ICES"])
table(sampleh[,"CJFAS"])
mean(sampleh[,"ICES"])
mean(sampleh[,"CJFAS"])
sd(sampleh[,"ICES"])
sd(sampleh[,"CJFAS"])
par(mfrow=c(2,1), oma=c(5,5,1,1), mar=c(0,0,0,0))
hist(sampleh[,"ICES"], breaks=seq(7.5,19.5,1),
     freq=F, ylim=c(0,0.45), axes=F,
     main="",xlab="", ylab="", col="grey")
axis(2, at=seq(0,0.4,0.2), las=1, pos=7.5)
mtext(side=3,line=-1.5,"ICES J. Mar. Sci.")
hist(sampleh[,"CJFAS"], breaks=seq(7.5,19.5,1),
     freq=F, ylim=c(0,0.45), axes=F,
     main="",xlab="", ylab="", col="grey")
mtext(side=3,line=-1.5,"Can. J. Fish. Aq. Sci.")
axis(1, pos=0)
axis(2, at=seq(0,0.4,0.2), las=1, pos=7.5)
mtext(side=1,line=3,"Sampled h-index (2008-12)", cex=1.3)
mtext(side=2,outer=T, line=3,"Frequency", cex=1.3)
