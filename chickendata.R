#Code for repreated measuresd
library(FANR6750)
data("chickdata")

chickdata$weight<-round(chickdata$weight+rnorm(nrow(chickdata),0,1),0)


chickdata$Chick <- factor(chickdata$Chick)
chickdata$Diet <- factor(chickdata$Diet)
chickdata_wide <- tidyr::pivot_wider(chickdata, names_from = Time,
                                     names_prefix = "Time", values_from = weight)

chickdata_wide[c(1,2,10, 11 ,19, 20, 28, 29),] %>%
  kable(format = "html", col.names = c("Chick", "Diet", "Day 0", "Day 2", "Day 4", "Day 6", "Day 8", "Day 10", "Day 12", "Day 14", "Day 16", "Day 18")) %>%
  add_header_above(c(" " = 1, " " = 1, "Weight" = 10))



library(ggplot2)
ggplot(chickdata[chickdata$Chick %in% c(1,2,10, 11 ,19, 20, 28, 29),], aes(x = Time, y = weight, color = as.factor(Diet), linetype = as.factor(Chick))) +
  geom_path(lwd=1.3) +
  geom_point(size=3) +
  scale_x_continuous("Day") +
  scale_y_continuous("Weight") +
  guides(linetype = "none")+
  theme_classic()





ggplot(chickdata, aes(x = Time, y = weight, color = Diet, linetype = Diet)) +
  geom_path(lwd=1) +
  geom_point(size=1) +
  scale_x_continuous("Day") +
  scale_y_continuous("Weight") +
  theme_classic()



chickdata_wide
chickdata_wide2 <- dplyr::mutate(chickdata_wide, int1 = Time2 - Time0,
                                 int2 = Time4 - Time2,
                                 int3 = Time6 - Time4,
                                 int4 = Time8 - Time6,
                                 int5 = Time10 - Time8,
                                 int6 = Time12 - Time10,
                                 int7 = Time14 - Time12,
                                 int8 = Time16 - Time14,
                                 int9 = Time18 - Time16)

chickdata2 <- tidyr::pivot_longer(chickdata_wide2,
                                  cols = c(int1, int2, int3, int4, int5, int6, int7, int8, int9),
                                  names_to = "Interval",
                                  values_to = "delta")
chickdata2$Interval <- rep(1:9, 36)
ggplot(chickdata2[chickdata2$Chick %in% c(1,2,10, 11 ,19, 20, 28, 29),],
       aes(x = Interval, y = delta, color = as.factor(Diet), linetype = as.factor(Chick))) +
  geom_path(lwd=1.7) +
  geom_point(size=3) +
  scale_x_continuous("Interval", breaks = c(1, 3, 5, 7, 9)) +
  scale_y_continuous("Weight change") +
  guides(linetype = "none")+
  theme_classic()


kableExtra::kable_classic(chickdata_wide2)



manova2 <- manova(cbind(Time2 - Time0,
                        Time4 - Time2,
                        Time6 - Time4,
                        Time8 - Time6,
                        Time10 - Time8,
                        Time12 - Time10,
                        Time14 - Time12,
                        Time16 - Time14,
                        Time18 - Time16) ~ Diet, data = chickdata2)
summary.aov(manova2)


chickdata_wide2[c(1,2,10, 11 ,19, 20, 28, 29),] %>%
  kable(format = "html")




library(nlme)
lmm2 <- lme(fixed = weight ~ Diet *Time,
            random =  ~ (Time-1)|Chick,
            data = chickdata,
            correlation = corAR1())
summary(lmm2)





## COFFEE


plots<-rep(1:4,each=16)
Cover<-rep(rep(c("Control", "Cajanus", "Crotalaria", "Raphanus"),each=4),4)
Fert<-rep(c("Control","Coconut","Coffee", "Fungi"),16)

data.frame(plots=plots,Cover=Cover,Fert=Fert)


plots <- 4                   # number of random plots
cover_crops <- 4            # 4 whole-plot treatments
fertilizers <- 4            # 4 subplot treatments
mu <- 13                  # grand mean

# Generate all combinations
df <- expand.grid(
  plot = factor(1:plots),
  cover_crop = factor(c("Control", "Cajanus", "Crotalaria", "Raphanus")),
  fertilizer = factor(c("Control","Coconut","Coffee", "Fungi"))
)


plot_effects <- rnorm(plots, mean = 0, sd = 1.2)  # γ_k
whole_plot_error <- rnorm(nrow(df), mean = 0, sd = 1.5)  # δ_ik
residual_error <- rnorm(nrow(df), mean = 0, sd = 0.8)  # ε_ijk

alpha <- c(-3, 1, 2, 2.1)  # cover crop effects
beta <- c(-2, -2, 1, 5)  # fertilizer effects

alpha_beta <- matrix(c(
  0,  0,  0,  0,
  0,  -1, -1,  2,
  0, -1,  3, -2,
  0,  2, -2,  3
), nrow = 4, byrow = TRUE)


df$mu <- mu
df$alpha <- alpha[as.numeric(df$cover_crop)]
df$beta <- beta[as.numeric(df$fertilizer)]
df$interaction <- mapply(function(i, j) alpha_beta[i, j],
                         as.numeric(df$cover_crop),
                         as.numeric(df$fertilizer))
df$plot_eff <- plot_effects[as.numeric(df$plot)]

# Combine to create response variable
df$y <- df$mu + df$alpha + df$beta + df$interaction +
  df$plot_eff + whole_plot_error + residual_error

df$WU<-paste0(df$plot,df$cover_crop)
df$WU<-as.numeric(factor(df$WU))

aov(y~cover_crop+fertilizer+cover_crop:fertilizer+plot+Error, data=coffee)

coffee<-df[,c(1:3,9,10)]
