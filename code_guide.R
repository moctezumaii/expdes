## Wrong model

wrongmodel<-lm(TOT.N~ D.PARK, data=RK)


summary(wrongmodel)

plot(wrongmodel)

ggplot(RK, aes(x = D.PARK, y = TOT.N)) +
  geom_point() +
  theme_classic()


predictedV<-predict.lm(wrongmodel,RK,interval = "co" )
RK_pred<-cbind(RK,predictedV)




ggplot(RK_pred, aes(x = D.PARK, y = TOT.N,ymin=lwr,ymax=upr)) +
  geom_point()+
geom_line(aes(y=fit),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()


## CORRECT

correctmodel<-glm(TOT.N~ D.PARK,family = poisson, data=RK)

summary(correctmodel)

ggplot(RK, aes(x = D.PARK, y = log(TOT.N))) +
  geom_point() +
  theme_classic()


predictedglm<-predict.glm(correctmodel,RK,se.fit = T)
RK_pred_glm<-cbind(RK,predictedglm)




ggplot(RK_pred_glm, aes(x = D.PARK, y = log(TOT.N), ymin=fit-2*se.fit,ymax=fit+2*se.fit)) +
  geom_point()+
  geom_line(aes(y=fit),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()



ggplot(RK_pred_glm, aes(x = D.PARK, y = TOT.N, ymin=exp(fit-2*se.fit),ymax=exp(fit+2*se.fit))) +
  geom_point()+
  geom_line(aes(y=exp(fit)),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()






####### Second model

## Wrong model

wrongmodel<-lm(TOT.N~ L.WAT.C, data=RK)

summary(wrongmodel)

plot(wrongmodel)

ggplot(RK, aes(x = L.WAT.C, y = TOT.N)) +
  geom_point() +
  theme_classic()


predictedV<-predict.lm(wrongmodel,RK,interval = "co" )
RK_pred<-cbind(RK,predictedV)




ggplot(RK_pred, aes(x = L.WAT.C, y = TOT.N, ymin=lwr,ymax=upr)) +
  geom_point()+
  geom_line(aes(y=fit),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()


## CORRECT

correctmodel<-glm(TOT.N~ L.WAT.C,family = poisson, data=RK)

summary(correctmodel)

ggplot(RK, aes(x = L.WAT.C, y = log(TOT.N))) +
  geom_point() +
  theme_classic()


predictedglm<-predict.glm(correctmodel,RK,se.fit = T)
RK_pred_glm<-cbind(RK,predictedglm)




ggplot(RK_pred_glm, aes(x = L.WAT.C, y = log(TOT.N), ymin=fit-2*se.fit,ymax=fit+2*se.fit)) +
  geom_point()+
  geom_line(aes(y=fit),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()



ggplot(RK_pred_glm, aes(x = L.WAT.C, y = TOT.N, ymin=exp(fit-2*se.fit),ymax=exp(fit+2*se.fit))) +
  geom_point()+
  geom_line(aes(y=exp(fit)),color="blue") +
  geom_ribbon(alpha=0.2)+
  theme_classic()


