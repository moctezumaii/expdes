dpois(39,50)

jointL<-dpois(39,36:60) * dpois(58,36:60) * dpois(49,36:60)
plot(36:60,jointL,xlab=paste("probability of", expression(lambda)))



poi<-dpois(39,20:80)
plot(0:80,poi,xlab=paste("probability of", expression(lambda)))
plot(0:80,poi,xlab=paste("probability of", expression(lambda)),type="l", ylab=L)
poi<-dpois(39,20:80)
plot(20:80,poi,xlab=paste("probability of", expression(lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=paste("probability of", expression(lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=paste("probability of", expression(\lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=paste("probability of", expression(/lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=paste("probability of", expression(Lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=expression(),type="l", ylab="L")
plot(0:80,poi,xlab=expression(paste("P of", lambda)),type="l", ylab="L")
plot(0:80,poi,xlab=expression(paste("P of ", lambda)),type="l", ylab="L")
ppois(0:80,40)
qpois(0:80,40)
dpois(0:80,50)
dnbinom(0:80,100,50)
dnbinom(0:80,0.5,50)
?dnbinom
dnbinom(0:80,size=100,mu=50)
dnbinom(0:80,size=1000,mu=50)
poi2<-dnbinom(0:80,size=1000,mu=50)
plot(0:80,poi2,xlab=expression(paste("P of ", lambda, "based on prior")),type="l", ylab="density")
plot(0:80,poi,xlab=expression(paste("P of ", lambda)),type="l", ylab="L")
plot(0:80,poi2,add=T,col="red")
plot(0:80,poi,xlab=expression(paste("P of ", lambda)),type="l", ylab="L")
lines(0:80,poi2,add=T,col="red")
plot(0:80,poi,xlab=expression(paste("P of ", lambda)),type="l", ylab="L")
lines(0:80,poi2,add=T,col="red")
poi3<-poi*poi2
poi3
lines(0:80,poi3,add=T,lwd=2,lty=2)
lines(0:80,poi3/sum(poi3),add=T,lwd=2,lty=2)
plot(0:80,poi3/sum(poi3),xlab=expression(paste("P of ", lambda)),type="n", ylab="L")
lines(0:80,poi)



prob1<-dpois(39,50)

jointL<-dpois(39,30:80) * dpois(58,30:80) * dpois(49,30:80)
plot(30:80,jointL,xlab=paste("probability of", expression(lambda)),type="l")
alpha=(50^2)/(7^2)
beta=(50)/(7^2)
dpois(39,50)


#Likelihood of observing 39 trees (what we observed) given values of Lambda from 20 to 80:
like<-dpois(39,20:80)
plot(20:80,like,xlab=paste("probability of", expression(lambda)),type="l")

#Estimate momentum to obtain the prior:
alpha=(50^2)/(7)
beta=(50)/(7)

#Plot the prior:
prior<-dgamma(20:80,alpha,beta)
plot(20:80,prior,xlab=paste("probability of", expression(lambda)),type="l",col="red")
#add the likelihhod:
lines(20:80,like)

jointL<-like*prior
posterior<-jointL/sum(jointL)


lines(20:80,jointL,col="black", lty=2,lwd=2)

posterior<-jointL/sum(jointL)
plot(20:80,posterior,xlab=paste("probability of", expression(lambda)),type="l",lty=2,lwd=2)
lines(20:80,prior,col="red")
lines(20:80,like)

dpois(0:100,50)



dpois(39,45)*dpois(58,45)*dpois(49,45)
