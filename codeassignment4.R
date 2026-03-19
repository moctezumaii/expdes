### Code to run model 1 plot:
ggplot(grasslanddata, aes(x=1,y=KgDMHA))+
  geom_point(alpha=0.01)+
  geom_boxplot(alpha=0.1,color=gray(0.9,0.1),width=0.1) +
  geom_violin(fill=gray(0.9,0.1),color="black")+
  geom_errorbar(aes(ymin=summary(model1)$coefficients[,1]-2*summary(model1)$coefficients[,2],
                    ymax=summary(model1)$coefficients[,1]+2*summary(model1)$coefficients[,2]),
                width=0.4)+
  geom_point(aes(y=summary(model1)$coefficients[,1]),  shape=20, size=2, color="red", fill="red")+
  theme_classic()

###Code to run and plot model 2

model2<-lm(KgDMHA~Water, data=grasslanddata)
summary(model2)
predictedmodel2<-predict.lm(model2,grasslanddata,interval = "co")
grasslanddata2<-cbind(grasslanddata,predictedmodel2)
ggplot(data=grasslanddata2,aes(y=KgDMHA, x=Water, ymin=lwr,ymax=upr ))+
  geom_point()+
  geom_line(aes(y=fit))+
  geom_ribbon(alpha=0.2)+
  theme_classic()




### Code to run and plot model 6
model6<-lm(KgDMHA~Graze+Pests, data=grasslanddata)
summary(model6)
predictedmodel6<-predict.lm(model6,grasslanddata,interval = "co")
grasslanddata6<-cbind(grasslanddata,predictedmodel6)
ggplot(data=grasslanddata6,aes(y=KgDMHA, x=Graze, ymin=lwr,ymax=upr, color=Pests, shape=Pests ))+
  geom_point()+
  geom_line(aes(y=fit))+
  geom_ribbon(alpha=0.2)+
  theme_classic()

grasslanddata[-1,]



nrow(grasslanddata)

randomnumbs<-sample(1:1000,750)
train<-grasslanddata[randomnumbs,]
test<-grasslanddata[-randomnumbs,]
