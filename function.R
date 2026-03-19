library(ggplot2)

lambplot<- function(lambda1, lambda2, lambda3){

  if (requireNamespace('ggplot2', quietly = TRUE)) {
    library(ggplot2)
    # Example with pkg
  } else {
    message("'pkg' not available")
  }


  x2 <- seq(0,qpois(0.99, max(c(lambda1,lambda2,lambda3)))+2, by = 1)

  selectedDatap <-c(dpois(x2, lambda1))

  selectedDatap2 <- c(dpois(x2, lambda2))

  selectedDatap3 <- c(dpois(x2, lambda3))



  df<- data.frame(y = c(selectedDatap,selectedDatap2,selectedDatap3),
                  x2 = x2,
                  lambda = factor(rep(c(paste("lambda = ", lambda1), paste("lambda = ", lambda2), paste("lambda = ", lambda3)), each = length(x2)),
                                  levels = c(paste("lambda = ", lambda1), paste("lambda = ", lambda2), paste("lambda = ", lambda3))))




  aplot<- ggplot(data=df, aes(x = x2+0.5, y = y, fill = lambda)) +
    geom_col(alpha = 0.75, color = "grey", linewidth = 0.1, position = "dodge") +
    scale_y_continuous("Probability density") +
    scale_x_continuous("y") +
    theme_classic()
  return(list(aplot,paste("your selections were",lambda1, lambda2, "and", lambda3)))

}


lambplot(15,20,30)






lm1example<-function(n,int,slope,xdist=1,var,xmean=0,xvar=2, xmin=0, xmax=100 ){
if(xdist==1){
  x<-rnorm(n,xmean,xvar)
}  else {
  x<-runif(n,xmin,xmax)
}

  y<- int + slope*x + rnorm(n,0,sqrt(var))


  df<-data.frame(x=x, y =y)

 lm1<-lm(y~x, data=df)


pred<- predict(lm1,newdata=df,interval="prediction")

df2<-cbind(df,pred)

newplot<-ggplot(data=df2, aes(x=x,y=y,ymin=lwr,ymax=upr))+
  geom_point()+
  geom_line(aes(y=fit),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()
  return(newplot)

}


lm1example(n=5000, int=1, slope = 10, var=50,xdist=1)




n<-10
rnorm(n)


x<-runif(n,xmin,xmax)


y<- int + slope*x + rnorm(n,0,sqrt(var))










## more complex function

lm2example<-function(n,int,slope,xdist=1,var,xmean=0,xvar=2, xmin=0, xmax=100,cat=F,cvalsn,cvals ){
  if(xdist==1){
    x<-rnorm(n,xmean,xvar)
  }  else {
    x<-runif(n,xmin,xmax)
  }

  if(cat==T){
    if(sum(cvalsn)!=n){
      stop("categorical values do not equal N")
    } else{
  treat<-rep(LETTERS[1:length(cvalsn)])
 # treat<-rep(treat,each=cvalsn)
  betavals<-  rep(0,cvalsn[1])

  for(i in 2:length(cvalsn)){
    betavals<-c(betavals,rep(cvals[i-1],cvalsn[i]) )
  }

  treatvals<-  rep("A",cvalsn[1])

  for(i in 2:length(cvalsn)){
    treatvals<-c(treatvals,rep(treat[i],cvalsn[i]) )
  }

  y<- int + slope*x +betavals+ rnorm(n,0,sqrt(var))


  df<-data.frame(x=x, y =y, group=treatvals)

  lm1<-lm(y~x+group, data=df)


  pred<- predict(lm1,newdata=df,interval="prediction")

  df2<-cbind(df,pred)
  #return(df2)
  newplot<-ggplot(data=df2, aes(x=x,y=y,ymin=lwr,ymax=upr,col=group,fill=group))+
    geom_point()+
    geom_line(aes(y=fit)) +
    geom_ribbon(alpha=0.2)+
    theme_classic()
  return(df2)


  } }

  else{
  y<- int + slope*x + rnorm(n,0,sqrt(var))


  df<-data.frame(x=x, y =y)

  lm1<-lm(y~x, data=df)


  pred<- predict(lm1,newdata=df,interval="prediction")

  df2<-cbind(df,pred)

  newplot<-ggplot(data=df2, aes(x=x,y=y,ymin=lwr,ymax=upr))+
    geom_point()+
    geom_line(aes(y=fit),color="blue") +
    geom_ribbon(alpha=0.2)+
    theme_classic()
  return(newplot)
  }
}


lm2example(n=5000, int=1, slope = 10, var=50,xdist=1, cat = T, cvalsn = c(1000,1000,1000,1000,1000),cvals=c(-1000,11,-200,100))






lambplot<- function(lambda1, s, m){



  x2 <- seq(0,qpois(0.99, max(c(lambda1,m)))+2, by = 1)

  selectedDatap <-c(dpois(x2, lambda1))

  selectedDatap2 <- c(dnbinom(x2,size= s, mu=m))





  df<- data.frame(y = c(selectedDatap,selectedDatap2),
                  x2 = x2,
                  dist = factor(rep(c(paste("Poisson"), paste("NegBinom")), each = length(x2))))

#  return(df)


  aplot<- ggplot(data=df, aes(x = x2+0.5, y = y, fill = dist)) +
    geom_col(alpha = 0.75, color = "grey", linewidth = 0.1, position = "dodge") +
    scale_y_continuous("Probability density") +
    scale_x_continuous("y") +
    theme_classic()
  return(aplot)

}


lambplot(100,10,100)
