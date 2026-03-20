treatment <- rep(LETTERS[1:4], each = 3)

values <- c(4.2, 4, 4.5, 5, 5.5, 6, 6, 6.5, 7, 7, 8, 9)

dataanova <- data.frame(treatment, values)

dataanova$x <- c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2)

library(ggplot2)

library(dplyr)
ybari <- dataanova %>%
  group_by(treatment) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:4, label = c("bar(y)[A]", "bar(y)[B]", "bar(y)[C]", "bar(y)[D]"))


dataanova$ybar <- rep(ybari$y, each = 3)


ggplot(dataanova, aes(x = x, y = values)) +
  geom_point(aes(color = treatment), size = 5) +
  scale_x_continuous(
    "Fertilizer treatment",
    labels = c("A", "B", "C", "D"),
    breaks = 1:4
  ) +
  scale_y_continuous("Height") +
  geom_hline(yintercept = mean(dataanova$values)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 4.5,
    y = mean(dataanova$values) + 0.2,
    parse = TRUE,
    size = 8
  ) +
  theme_classic() +
  geom_segment(
    data = ybari,
    aes(x = x - 0.25, xend = x + 0.25, y = y, yend = y, color = treatment)
  ) +
  geom_segment(
    data = ybari,
    aes(x = x, xend = x, y = mean(dataanova$values), yend = y),
    color = "#D47500"
  ) +
  geom_segment(
    data = dataanova,
    aes(x = x, xend = x, y = ybar, yend = values),
    color = "#3CB521"
  )

fit1 <- lm(values ~ treatment, data = dataanova)

model.matrix(fit1)

summary(fit1)


dataanova$x <- c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2)
ggplot(dataanova, aes(x = x, y = Height)) +
  geom_point(aes(color = Treatment), size = 5) +
  scale_x_continuous(
    "Fertilizer treatment",
    labels = c("A", "B", "C", "D"),
    breaks = 1:4
  ) +
  scale_y_continuous("Height") +
  guides(color = FALSE) +
  geom_hline(yintercept = mean(dataanova$Height)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 4.25,
    y = mean(dataanova$Height) + 0.6,
    parse = TRUE,
    size = 8
  )


ggplot(dataanova, aes(x = x, y = values)) +
  geom_point(aes(color = treatment), size = 5) +
  scale_x_continuous(
    "Fertilizer treatment",
    labels = c("A", "B", "C", "D"),
    breaks = 1:4
  ) +
  scale_y_continuous("Height") +
  geom_hline(yintercept = mean(dataanova$values)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 4.5,
    y = mean(dataanova$values) + 0.2,
    parse = TRUE,
    size = 8
  ) +
  theme_classic() +
  geom_segment(
    data = ybari,
    aes(x = x - 0.25, xend = x + 0.25, y = y, yend = y, color = treatment)
  ) +
  geom_segment(
    data = dataanova,
    aes(x = x, xend = x, y = ybar, yend = values),
    color = "#3CB521"
  )


dataanova

bartlett.test(values ~ treatment, data = dataanova)

results.aov <- aov(values ~ treatment, data = dataanova)
TukeyHSD(results.aov)
write_xlsx(tidy(TukeyHSD(results.aov)), "Anova_tukey.xlsx")

results.lm <- lm(values ~ treatment, data = dataanova)
emm.s <- emmeans(results.lm, "treatment")
pairs(emm.s)

summary(results.aov)
write_xlsx(tidy(results.aov), "Anova_ob.xlsx")
library(broom)
install.packages("writexl")
library(writexl)

results.lm <- lm(values ~ treatment, data = dataanova)
summary(results.lm)


pairwise.t.test(dataanova$values, dataanova$treatment)

write_xlsx(
  tidy(pairwise.t.test(
    dataanova$values,
    dataanova$treatment,
    p.adjust.method = "bonferroni"
  )),
  "bonf.xlsx"
)


pigs.emm.s <- emmeans(pigs.lm, "source")
pairs(pigs.emm.s)


write_xlsx(dataanova[, -3], "newdata.xlsx")

dataanova$source <- rep(c("O", "I"), each = 6)


mod <- lm(values ~ treatment, data = dataanova)
library(emmeans)
emm <- emmeans(mod, ~treatment)
custom <- list(
  groups = c(1, 1, -1, -1),
  org = c(1, -1, 0, 0),
  inorg = c(0, 0, 1, -1)
)
table <- contrast(emm, custom)

table


library(writexl)
library(broom)
tidytable <- tidy(table)
write_xlsx(tidytable, "contrastt.xlsx")


### New example
treatment <- rep(LETTERS[1:3], each = 15)

values <- c(rnorm(15, 25), rnorm(15, 38), rnorm(15, 41))

dataanova <- data.frame(treatment, values)

dataanova$x <- c(
  seq(from = 0.6, to = 1.4, length = 15),
  seq(from = 1.6, to = 2.4, length = 15),
  seq(from = 2.6, to = 3.4, length = 15)
)

library(ggplot2)

library(dplyr)
ybari <- dataanova %>%
  group_by(treatment) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:3, label = c("bar(y)[A]", "bar(y)[B]", "bar(y)[C]"))


dataanova$ybar <- rep(ybari$y, each = 15)
library(ggplot2)
ggplot(dataanova, aes(x = x, y = values)) +
  geom_point(aes(color = treatment), size = 3) +
  scale_x_continuous(
    "Diet treatment",
    labels = c("High-E", "Forage", "Mixed"),
    breaks = 1:3,
    guide = "none"
  ) +
  scale_y_continuous("Weight gain") +
  geom_hline(yintercept = mean(dataanova$values)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 3.5,
    y = mean(dataanova$values) + 1.2,
    parse = TRUE,
    size = 5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_segment(
    data = ybari,
    aes(x = x - 0.45, xend = x + 0.45, y = y, yend = y, color = treatment)
  ) +
  geom_segment(
    data = ybari,
    aes(x = x, xend = x, y = mean(dataanova$values), yend = y),
    color = "#D47500"
  ) +
  geom_segment(
    data = dataanova,
    aes(x = x, xend = x, y = ybar, yend = values),
    color = "#3CB521"
  )

fit1 <- lm(values ~ treatment, data = dataanova)

model.matrix(fit1)

summary(fit1)


dataanova$x <- c(0.8, 1, 1.2, 1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2)
ggplot(dataanova, aes(x = x, y = Height)) +
  geom_point(aes(color = Treatment), size = 5) +
  scale_x_continuous(
    "Fertilizer treatment",
    labels = c("A", "B", "C", "D"),
    breaks = 1:4
  ) +
  scale_y_continuous("Height") +
  guides(color = FALSE) +
  geom_hline(yintercept = mean(dataanova$Height)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 4.25,
    y = mean(dataanova$Height) + 0.6,
    parse = TRUE,
    size = 8
  )


results.aov <- aov(values ~ treatment, data = dataanova)

summary(results.aov)
write_xlsx(tidy(results.aov), "Anova_ob2.xlsx")

TukeyHSD(results.aov)
write_xlsx(tidy(TukeyHSD(results.aov)), "Anova_tukey2.xlsx")

results.lm <- lm(values ~ treatment, data = dataanova)
emm.s <- emmeans(results.lm, "treatment")
pairs(emm.s)


treatment <- rep(LETTERS[1:3], each = 15)
farm <- rep(rep(1:5, each = 3), 3)
farm <- as.factor()
beta <- rnorm(5, 0, 2)
values <- c(rnorm(15, 30), rnorm(15, 38), rnorm(15, 40))
values <- values + rep(rep(beta, each = 3), 3)

dataanova <- data.frame(treatment, values, farm)
write.csv(dataanova, "cowfarms.csv", row.names = FALSE)

dataanova$x <- c(
  seq(from = 0.6, to = 1.4, length = 15),
  seq(from = 1.6, to = 2.4, length = 15),
  seq(from = 2.6, to = 3.4, length = 15)
)

library(ggplot2)

library(dplyr)
ybari <- dataanova %>%
  group_by(treatment) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:3, label = c("bar(y)[A]", "bar(y)[B]", "bar(y)[C]"))

ybari2 <- dataanova %>%
  group_by(treatment, farm) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:5)


dataanova$ybar <- rep(ybari$y, each = 15)

ggplot(dataanova, aes(x = x, y = values)) +
  geom_point(aes(color = as.factor(farm)), size = 3) +
  scale_x_continuous(
    "Diet treatment",
    labels = c("High-E", "Forage", "Mixed"),
    breaks = 1:3,
    guide = "none"
  ) +
  scale_y_continuous("Weight gain") +
  geom_hline(yintercept = mean(dataanova$values)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 3.5,
    y = mean(dataanova$values) + 1.2,
    parse = TRUE,
    size = 5
  ) +
  theme_classic() +
  geom_segment(
    data = ybari,
    aes(x = x - 0.45, xend = x + 0.45, y = y, yend = y, color = treatment)
  ) +
  #geom_segment(data = ybari2, aes(x = x - 0.45, xend = x + 0.45, y = y, yend = y, color = farm)) +

  geom_segment(
    data = ybari,
    aes(x = x, xend = x, y = mean(dataanova$values), yend = y),
    color = "#D47500"
  ) +
  geom_segment(
    data = dataanova,
    aes(x = x, xend = x, y = ybar, yend = values),
    color = "#3CB521"
  )


results.aov <- aov(values ~ treatment + Error(farm), data = dataanova)

results.aov

results.aov <- aov(values ~ treatment * farm + Error(farm), data = dataanova)


summary(results.aov)
write_xlsx(tidy(results.aov), "Anova_ob3.xlsx")

TukeyHSD(results.aov)


library(multcomp)
glht(results.aov, linfct = mcp(check = "Tukey"))
pairs(emmeans(results.aov, "treatment"))


treatment <- rep(LETTERS[1:3], each = 15)
breed <- rep(rep(c("Holstein", "Angus", "Brehman"), each = 3), 5)
#beta<-rnorm(5,0,2)
values <- c(rnorm(15, 30), rnorm(15, 38), rnorm(15, 40))
values <- values + c(rnorm(15, 30), rnorm(15, 38), rnorm(15, 40))

dataanova <- data.frame(treatment, values, breed)
write.csv(dataanova, "cowbreeds.csv", row.names = FALSE)

dataanova <- dataanova %>% arrange(desc(breed))
dataanova$x <- c(
  seq(from = 0.6, to = 1.4, length = 15),
  seq(from = 1.6, to = 2.4, length = 15),
  seq(from = 2.6, to = 3.4, length = 15)
)

values <- values + c(rnorm(15, 3), rnorm(15, 4), rnorm(15, 6))

dataanova <- dataanova %>% arrange(treatment)

dataanova$x <- c(
  seq(from = 0.6, to = 1.4, length = 15),
  seq(from = 1.6, to = 2.4, length = 15),
  seq(from = 2.6, to = 3.4, length = 15)
)


library(ggplot2)

library(dplyr)
ybari <- dataanova %>%
  group_by(treatment) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:3, label = c("bar(y)[A]", "bar(y)[B]", "bar(y)[C]"))

ybari2 <- dataanova %>%
  group_by(treatment, farm) %>%
  summarise(y = mean(values)) %>%
  mutate(x = 1:5)


dataanova$ybar <- rep(ybari$y, each = 15)

ggplot(dataanova, aes(x = x, y = values)) +
  geom_point(aes(color = as.factor(breed)), size = 3) +
  scale_x_continuous(
    "Diet treatment",
    labels = c("High-E", "Forage", "Mixed"),
    breaks = 1:3,
    guide = "none"
  ) +
  scale_y_continuous("Weight gain") +
  geom_hline(yintercept = mean(dataanova$values)) +
  annotate(
    "text",
    label = "bar(y)[.]",
    x = 3.5,
    y = mean(dataanova$values) + 1.2,
    parse = TRUE,
    size = 5
  ) +
  theme_classic() +
  geom_segment(
    data = ybari,
    aes(x = x - 0.45, xend = x + 0.45, y = y, yend = y, color = treatment)
  ) +
  #geom_segment(data = ybari2, aes(x = x - 0.45, xend = x + 0.45, y = y, yend = y, color = farm)) +

  geom_segment(
    data = ybari,
    aes(x = x, xend = x, y = mean(dataanova$values), yend = y),
    color = "#D47500"
  ) +
  geom_segment(
    data = dataanova,
    aes(x = x, xend = x, y = ybar, yend = values),
    color = "#3CB521"
  )


results.aov <- aov(values ~ treatment * breed, data = dataanova)

summary(results.aov)
write_xlsx(tidy(results.aov), "Anova_ob4.xlsx")

TukeyHSD(results.aov)


### Nested

exp <- data.frame(
  obs = rep(c(1, 2, 3), 2),
  expUnit = rep(c(1, 2), each = 3),
  tA = c(15.9, 16.3, 15.9, 14.2, 13.4, 14.0),
  tB = c(12.3, 12.7, 13.0, 13.1, 13.0, 13.4),
  tC = c(18.5, 18.0, 18.7, 19.0, 19.5, 19)
)


library(tidyr)

anovadata2 <- pivot_longer(
  exp,
  cols = starts_with("t"),
  names_to = "Treatment",
  values_to = "vals"
)


anovadata2$expUnit <- factor(anovadata2$expUnit)


plotData <- aggregate(vals ~ Treatment + expUnit, data = anovadata2, FUN = mean)

plotData <- plotData %>% arrange(Treatment)
plotData$treatment = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)


library(tidyverse)
y.bar <- dplyr::group_by(anovadata2, Treatment) %>%
  summarise(mu = mean(vals)) %>%
  dplyr::mutate(treatment = c(1, 2, 3))

y.bar$exp <- mean(anovadata2$vals)

plotData$exp <- rep(c(15, 12.9, 18.8), each = 2)


anovadata2$Treatment <- as.factor(anovadata2$Treatment)
anovadata2 <- anovadata2 %>% arrange(Treatment)
anovadata2$treatment <- rep(c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2), each = 3) +
  rnorm(18, 0, 0.07)
anovadata2$exp <- rep(plotData$vals, each = 3)

ggplot() +
  geom_hline(yintercept = mean(anovadata2$vals)) +
  geom_segment(
    data = y.bar,
    aes(x = treatment, xend = treatment, y = exp, yend = mu)
  ) +
  geom_segment(
    data = y.bar,
    aes(
      x = treatment - 0.3,
      xend = treatment + 0.3,
      y = mu,
      yend = mu,
      color = Treatment
    ),
    linewidth = 0.75
  ) +
  geom_segment(
    data = plotData,
    aes(
      x = treatment,
      xend = treatment,
      y = exp,
      yend = vals,
      color = Treatment
    )
  ) +
  geom_segment(
    data = plotData,
    aes(
      x = treatment - 0.1,
      xend = treatment + 0.1,
      y = vals,
      yend = vals,
      color = Treatment
    )
  ) +
  geom_blank(
    data = anovadata2,
    aes(x = treatment, y = vals, color = Treatment),
    size = 1
  ) +
  geom_segment(
    data = anovadata2,
    aes(x = treatment, xend = treatment, y = exp, yend = vals),
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  geom_point(
    data = anovadata2,
    aes(x = treatment, y = vals, color = Treatment),
    size = 1
  ) +
  scale_y_continuous("values") +
  scale_x_continuous(
    "Treatment",
    breaks = c(1, 2, 3),
    labels = c("A", "B", "C")
  ) +

  theme_classic() +
  theme(legend.position = "none")


nested_aov <- aov(vals ~ Treatment + Error(expUnit), data = anovadata2)
summary(nested_aov)


pairs(emmeans(nested_aov, "Treatment"))


# Simulate a factorial design with three factors: fertilizer, irrigation and light.
# Fertilizer: 2 levels (organic, inorganic)
# Irrigation: 3 levels (low, medium, high)
# Light: 2 levels (low, high)
# 12 total combinations × 5 replicates = 60 observations.
# No interactions; strong effects of fertilizer and irrigation; medium effect of light.

set.seed(123)
# build all combinations and replicate each combination 5 times
factors <- expand.grid(
  fertilizer = c("organic", "inorganic"),
  irrigation = c("low", "medium", "high"),
  light = c("low", "high"),
  stringsAsFactors = FALSE
)
data <- factors[rep(seq_len(nrow(factors)), each = 5), ]

# Define main effects (no interactions)
effects_fert <- c(organic = 0, inorganic = 5)
effects_irrig <- c(low = 0, medium = 5, high = 10)
effects_light <- c(low = 0, high = 2)
baseline <- 20

mu <- baseline +
  effects_fert[data$fertilizer] +
  effects_irrig[data$irrigation] +
  effects_light[data$light]

growth <- rnorm(nrow(data), mean = mu, sd = 2)
data$growth <- growth

write.csv(growth, "growthdata.csv", row.names = FALSE)
