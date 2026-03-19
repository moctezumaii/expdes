RK<-read.csv("RK.csv")

wrongmodel<-lm(TOT.N~ D.PARK, data=RK)

summary(wrongmodel)

#plot(wrongmodel)


library(ggplot2)
ggplot(RK, aes(x=D.PARK, y=TOT.N)) +
  geom_point() +
  theme_classic()

predictedV<- predict.lm(wrongmodel, RK, interval = "co")
RK_pred<-cbind(RK,predictedV)


ggplot(RK_pred, aes(x=D.PARK, y=TOT.N,ymin=lwr, ymax=upr)) +
  geom_point() +
  geom_line(aes(y=fit),color="salmon",lwd=2)+
  geom_ribbon(alpha=0.2)+
  theme_classic()

wrongmodel<-lm(TOT.N~ D.PARK, data=RK)
summary(wrongmodel)


correctmodel<-glm(TOT.N~L.WAT.C,data=RK,family=poisson)

summary(correctmodel)

predictedglm<-predict.glm(correctmodel,RK,se.fit=T)

RK_pred_glm<-cbind(RK,predictedglm)

ggplot(RK_pred, aes(x=D.PARK, y=TOT.N,ymin=lwr, ymax=upr)) +
  geom_point() +
  geom_line(aes(y=fit),color="salmon",lwd=2)+
  geom_ribbon(alpha=0.2)+
  theme_classic()


ggplot(RK_pred_glm, aes(x=D.PARK,y=log(TOT.N)))+
  geom_point() +
  geom_line(aes(y=fit),color="salmon",lwd=2)+
  theme_classic()


ggplot(RK_pred_glm, aes(x=D.PARK,y=(TOT.N)))+
  geom_point() +
  geom_line(aes(y=exp(fit)),color="salmon",lwd=2)+
  theme_classic()


ggplot(RK_pred_glm, aes(x = D.PARK, y = TOT.N, ymin=exp(fit-2*se.fit),ymax=exp(fit+2*se.fit))) +
  geom_point()+
  geom_line(aes(y=exp(fit)),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()










summary(correctmodel)


q2<--0.4659+0.009654*50-0.51-0.68

