x <- seq(from = 0, to = 2000, by = 0.1)
x2 <- runif(n = 50, 0, 2000)
line_df <- data.frame(x = x,
                      y = 500 + 2 * x)
samp_df <- data.frame(x = x2, y = rnorm(n = 25, 500 + 2 * x2, 50))
ggplot(line_df, aes(x = x, y = y)) +
  geom_point(data = samp_df, aes(x = x, y = y), color = "#D47500", alpha = 0.75) +
  geom_path(color = "#446E9B")+
  scale_x_continuous("Rainfall (mm)") +
  scale_y_continuous("Number of walnuts")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




x <- rnorm(200)
y <- 20 + 1.57 * x + rnorm(200, 0, 0.75)
mu2<- 22 + 1.75 * x
mu <- 20 + 1.57 * x
resid <- y - mu
df <- data.frame(x = x, y = y, mu = mu, Residuals = resid)


ggplot(df, aes(x, y)) +
  geom_abline(slope = 1.57, intercept = 20, color = "#446E9B", size = 1.5) +
  scale_y_continuous("y") +
  geom_point(color = "#D47500", alpha = 0.75)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(df, aes(x, Residuals)) +
  geom_abline(slope = 0, intercept = 0, color = "grey40", linetype = "dashed", size = 1.5) +
  geom_point(color = "#D47500", alpha = 0.75)


ggplot(df, aes(x, y)) +
#  geom_segment(aes(x = x, xend = x, y = mu, yend = y), color = "#3CB521", size = 1) +
  geom_segment(aes(x = x, xend = x, y = mu2, yend = y), color = "#f307f7", size = 1) +
#    geom_abline(slope = 1.57, intercept = 20, color = "#446E9B", size = 1.5) +
  geom_abline(slope = 1.75, intercept = 22, color = "#4f493e", size = 1.5) +
  scale_y_continuous("y") +
  geom_point(color = "#D47500", alpha = 0.75)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

x <- seq(from = 0, to = 100, 0.1)
lm2 <- data.frame(x = x,
                  y = 20000 + 1.8 * x - 0.4 * x^2)

ggplot(lm2, aes(x = x, y = y)) +
  geom_line() +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

x<-17



ggplot(df, aes(x, Residuals)) +
  geom_abline(slope = 0, intercept = 0, color = "grey40", linetype = "dashed", size = 1.5) +
  geom_point(color = "#D47500", alpha = 0.75)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="#446E9B", alpha = 0.5) +
  geom_smooth(method='lm', se=FALSE, color="#D47500", alpha = 0.5) +
  geom_path(aes(x,y), data = path1, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path2, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path3, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path4, linewidth = 1, color = "#3CB521")




ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="#446E9B", alpha = 0.5) +
  geom_smooth(method='lm', se=FALSE, color="#D47500", alpha = 0.5) +
  geom_path(aes(x,y), data = path1, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path2, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path3, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path4, linewidth = 1, color = "#3CB521") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment2, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment3, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment4, linewidth = 0.6,
               color = "#CD0200")













x <- runif(100, 0, 60)
ey <- 10 + 1*x
eps <- rnorm(100, 0, 3)
y <- ey + eps
df <- data.frame(x, y)
lm_fit <- lm(y ~ x, data = df)

k <- 2.5
sigma <- sigma(lm_fit)
ab <- coef(lm_fit); a <- ab[1]; b <- ab[2]

x <- seq(-k*sigma, k*sigma, length.out = 50)
y <- dnorm(x, 0, sigma)/dnorm(0, 0, sigma) * 3

x0 <- 5
y0 <- a+b*x0
path1 <- data.frame(x = y + x0, y = x + y0)
segment1 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
x0 <- 20
y0 <- a+b*x0
path2 <- data.frame(x = y + x0, y = x + y0)
segment2 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
x0 <- 35
y0 <- a+b*x0
path3 <- data.frame(x = y + x0, y = x + y0)
segment3 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
x0 <- 50
y0 <- a+b*x0
path4 <- data.frame(x = y + x0, y = x + y0)
segment4 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)

ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="#446E9B", alpha = 0.5)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="#446E9B", alpha = 0.5) +
  geom_smooth(method='lm', se=FALSE, color="#D47500", alpha = 0.5)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="#446E9B", alpha = 0.5) +
  geom_smooth(method='lm', se=FALSE, color="#D47500", alpha = 0.5) +
  geom_path(aes(x,y), data = path1, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path2, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path3, linewidth = 1, color = "#3CB521") +
  geom_path(aes(x,y), data = path4, linewidth = 1, color = "#3CB521")




ggplot(df, mapping = aes(x=x, y=y)) +
  geom_point(color="black", alpha = 0.5) +
   geom_smooth(method='lm', se=FALSE, color="#D47500", alpha = 0.5) +
   geom_path(aes(x,y), data = path1, linewidth = 1, color = "#3CB521") +
   geom_path(aes(x,y), data = path2, linewidth = 1, color = "#3CB521") +
   geom_path(aes(x,y), data = path3, linewidth = 1, color = "#3CB521") +
   geom_path(aes(x,y), data = path4, linewidth = 1, color = "#3CB521") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment2, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment3, linewidth = 0.6,
               color = "#CD0200") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment4, linewidth = 0.6,
               color = "#CD0200") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))









#Milk example
y<-c(25.9,25.978,25.9,26.09,26.1,24.9,25.89,25.978,26.12,25.98,27,25,25.6,25.4,25.5)

model1<-lm(y~1)

summary(lm(y~1))

t.test(y,mu=26)



teporingos<-read.csv("teporingos2pops.csv",stringsAsFactors = T)

summary(teporingos)

library(dplyr)

teporingos%>%group_by(site)%>%summarize(min=min(mass),
                                        q1=quantile(mass      , 0.25),
                                        median=median(mass      ),
                                        mean=mean(mass),
                                        q3=quantile(mass, 0.75),
                                        max=max(mass))








samp <- data.frame(Experiment = 1:100,
                   time = 1:100,
                   Estimate = numeric(length = 100),
                   SE = numeric(length = 100))

for(i in 1:100){
  x <- rnorm(40, 75,4)
  samp$Estimate[i] = mean(x)
  samp$SD[i] = sd(x)
  samp$SE = sd(x)/sqrt(25)
}

fish_df <- data.frame(z = seq(from = 65, to = 85, by = 0.1),
                      pr_z = dnorm(x = seq(from = 65, to = 85, by = 0.1), 75, 4))


p <- ggplot(samp[samp$Experiment==1,], aes(x = Experiment, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 75, color = "black", linetype = "longdash") +
  scale_y_continuous(name = "", limits = c(70, 80), breaks = seq(from = 70, to = 80,by=2)) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_classic() +
  geom_segment(aes(y = 79, yend = 79, x = 71, xend = 82),
               color = "black", linetype = "longdash") +
  annotate(x = 82, y = 78, label = "Sample mean", geom = "text", hjust = 0) +
  geom_point(aes(y = 78, x = 77)) +
  annotate(x = 85, y = 79, label = "True parameter mean", geom = "text", hjust = 0)
p


valsn<-rnorm(39,74.62779,4)
valsn<-c(valsn,2985.112-sum(valsn))
samp[samp$Experiment==1,3]
Experiment<-rep(1,40)
valsn_d<-data.frame(valsn, Experiment)


p+  geom_errorbar(data = samp[samp$Experiment==1,], aes(x = Experiment, ymin = Estimate - SD, ymax = Estimate + SD), width = 0)


p+geom_point(data=samp[samp$Experiment==2,], aes(x = Experiment, y = Estimate))

p+geom_point(data=samp[samp$Experiment==2,], aes(x = Experiment, y = Estimate))+
  geom_point(data=samp[samp$Experiment==3,], aes(x = Experiment, y = Estimate))

p+geom_point(data=samp[samp$Experiment==2,], aes(x = Experiment, y = Estimate))+
  geom_point(data=samp[samp$Experiment==3,], aes(x = Experiment, y = Estimate))+
  geom_point(data=samp[samp$Experiment==4,], aes(x = Experiment, y = Estimate))


for(i in 1:4){
print(i)
p2<-  ggplot() +
  labs(subtitle = "Sampling") +
  geom_path(data = fish_df, aes(x = z, y = pr_z), color = "#446E9B")+
  geom_segment(aes(y = 0, yend = 1, x = samp[samp$Experiment==i,3], xend = samp[samp$Experiment==i,3]),
               color = "black", linetype = "longdash")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  ylab("Density")+xlab("size(cm)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
print(p2)
}

ggplot() +
  labs(subtitle = "Sampling") +
  geom_path(data = fish_df, aes(x = z, y = pr_z), color = "#446E9B")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  ylab("Density")+xlab("size(cm)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


samp$type<-"A"

 ggplot(samp, aes(x = Experiment, y = Estimate,group=type)) +
   geom_point(aes(group = seq_along(Experiment))) +
   geom_hline(yintercept = 75, color = "black", linetype = "longdash") +
   scale_y_continuous(name = "", limits = c(70, 80), breaks = seq(from = 70, to = 80,by=2)) +
   scale_x_continuous(limits = c(0, 100)) +
   theme_classic() +
   geom_segment(aes(y = 79, yend = 79, x = 71, xend = 82),
                color = "black", linetype = "longdash") +
   annotate(x = 82, y = 78, label = "Sample mean", geom = "text", hjust = 0) +
   geom_point(aes(y = 78, x = 77)) +
   annotate(x = 85, y = 79, label = "True parameter mean", geom = "text", hjust = 0)+
   transition_reveal(Experiment)+
   view_follow(fixed_x = TRUE,fixed_y = TRUE)

 ?transition_manual


 ggplot(samp, aes(x = Experiment, y = Estimate,group=type)) +
   geom_point(aes(group = seq_along(Experiment))) +
   geom_hline(yintercept = 75, color = "black", linetype = "longdash") +
   scale_y_continuous(name = "", limits = c(70, 80), breaks = seq(from = 70, to = 80,by=2)) +
   scale_x_continuous(limits = c(0, 100)) +
   theme_classic() +
   geom_segment(aes(y = 79, yend = 79, x = 71, xend = 82),
                color = "black", linetype = "longdash") +
   annotate(x = 82, y = 78, label = "Sample mean", geom = "text", hjust = 0) +
   geom_point(aes(y = 78, x = 77)) +
   annotate(x = 85, y = 79, label = "True parameter mean", geom = "text", hjust = 0)




 ggplot() +
   geom_histogram(data = samp, aes(x = Estimate, y = ..density..),
                  fill = "#999999", alpha = 0.75, color = "#999999", bins = 60) +
      labs(subtitle = "Sampling distribution of 100 samples (fish n = 40)") +
   geom_path(data = fish_df, aes(x = z, y = pr_z), color = "#446E9B")+
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





 ggplot(samp, aes(x = Experiment, y = Estimate)) +
   geom_point() +
   geom_hline(yintercept = 70, color = "red", linetype = "longdash") +
   scale_y_continuous(name = "", limits = c(68, 73), breaks = seq(from = 68, to = 73)) +
   scale_x_continuous(limits = c(0, 100)) +
   theme_classic() +
   geom_segment(aes(y = 72.75, yend = 72.75, x = 1, xend = 12),
                color = "red", linetype = "longdash") +
   annotate(x = 15, y = 72.75, label = "True parameter value", geom = "text", hjust = 0) +
   geom_point(aes(y = 72.25, x = 6)) +
   annotate(x = 15, y = 72.25, label = "Sample mean", geom = "text", hjust = 0)






 wing_df <- data.frame(z = seq(from = 63, to = 77, by = 0.1),
                       pr_z = dnorm(x = seq(from = 63, to = 77, by = 0.1), 70, 2))



 samp2 <- data.frame(Experiment = 1:1000,
                     time = 1:1000,
                     Estimate = numeric(length = 1000),
                     SE = numeric(length = 1000))

 for(i in 1:1000){
   x <- rnorm(30, 70, 2)
   samp2$Estimate[i] = mean(x)
   samp2$SD[i] = sd(x)
   samp2$SE = sd(x)/sqrt(30)
 }

 ggplot() +
   geom_histogram(data = samp2, aes(x = Estimate, y = ..density..),
                  fill = "#999999", alpha = 0.75, color = "#999999", bins = 60) +
   geom_rug(data = samp2, aes(x = Estimate), color = "#D47500") +
   geom_path(data = wing_df, aes(x = z, y = pr_z), color = "#446E9B") +
   geom_vline(xintercept = 70, color = "white") +
   geom_vline(xintercept = mean(samp2$Estimate), linetype = "dashed", color = "white") +
   scale_y_continuous("Density") +
   scale_x_continuous("Sample mean", expand = c(0, 0)) +
   labs(subtitle = "Sampling distribution of 1000 samples (n = 30)") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))







set.seed(2500)
 start.time <- Sys.time()



 tails<-0
 for(i in 1:10000000){
   test<-rbinom(1,1,0.5)
   if(test==1){
     tails<-tails+1
     if(tails==20){
       print(paste0("You won after ", i, " tries"))
       break
     }
   } else
     tails<-0
 }

 end.time <- Sys.time()
 time.taken <- round(end.time - start.time,2)
 time.taken



 set.seed(2500)
 start.time <- Sys.time()
 no_tail <- 0
 for(i in 1:10000000){
   H_or_T <- rbinom(1,1,0.5)
   ifelse(H_or_T == 0, no_tail <- no_tail + 1, no_tail <-  0)
   if(no_tail == 20){
     print(i)
     break
   }
 }
 end.time <- Sys.time()
 time.taken <- round(end.time - start.time,2)
 time.taken


teporingos<-read.csv("teporingos2pops.csv")

site<-c(rep("Tlaloc",24),rep("Popocatepetl",30))

mass<-rnorm(54,450,10)

teporingosnew<-data.frame(site,mass)
write.csv(teporingosnew,"teporingosnew.csv",row.names = F)

r=0
for(i in 1:10^7){
  `if`(rbinom(1,1,.5),r<-r+1,r<-0);if(r==20){cat(i);break}}


 site<-1:63
 Foodavailability<-runif(63,0.2,7)
ReproductiveEffort<- 0.5+1.2*Foodavailability+runif(63,0,0.85)
max(ReproductiveEffort)
foodav<-data.frame(site,Foodavailability,ReproductiveEffort)
write.csv(foodav,"foodav.csv",row.names = F)

pig<-1:150
IC<-rnorm(150,6,1.5)
dose1<-c(rep(0,50),rep(1,50),rep(0,50))
dose2<-c(rep(0,50),rep(0,50),rep(1,50))
Dose<-c(rep("control",50),rep("dose1",50),rep("dose2",50))

FC<-IC+rnorm(150,0,1)-dose1*0.5-dose2*2
min(FC)

drugZ<-data.frame(pig,IC,FC,Dose)
write.csv(drugZ,"drugZ.csv",row.names = F)







teporingos2pops<-read.csv('teporingos2pops.csv')




teporingos2pops







ggplot(data = teporingos2pops, aes(x="pops", y = mass)) +
  geom_boxplot(fill=gray(0.7,0.25),color="black",width=0.1) +
  geom_violin(fill=gray(0.9,0.1),color="black")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(, size = 5, alpha = 0.5)


b0<-3
b1<--0.25
b2<-25
x<-runif(220,0,575)

y<-b0+b1*x+b2*x^2+rnorm(220,0,392000)

alldata<-data.frame(x,y)

quadratic_model <- lm(y ~ x + I(x^2))




predictedq<-predict.lm(quadratic_model,alldata,interval="co")

alldata<-cbind(alldata,predictedq)


ggplot(alldata, aes(x = x, y = y,ymin=lwr,ymax=upr)) +
  geom_point() +
  geom_line(aes(y=fit)) +
  geom_ribbon(alpha=0.2)+
  theme_classic()

plot(quadratic_model,which=1)




alldata<-data.frame(x,y)

quadratic_model <- lm(y ~ x)

predictedq<-predict.lm(quadratic_model,alldata,interval="co")

alldata<-cbind(alldata,predictedq)


ggplot(alldata, aes(x = x, y = y,ymin=lwr,ymax=upr)) +
  geom_point() +
  geom_line(aes(y=fit)) +
  geom_ribbon(alpha=0.2)+
  theme_classic()

plot(quadratic_model,which=1)
    plot(quadratic_model)





welldata<-data.frame(wellID=paste0("well_",1:28),fluoride=rnorm(28,3.1,1.6))

write.csv(welldata,"welldata.csv",row.names = F)



