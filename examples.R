fert_data <- data.frame(
  fertilizer = c(0, 50, 100, 150, 200),
  height = c(32, 45, 55, 68, 80)
)
library(ggplot2)

ggplot(fert_data, aes(x = fertilizer, y = height)) +
  geom_point(size = 5) +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (cm)",
    title = "Fertilizer and Plant Height"
  ) +
  theme_classic()

#Recreate scatterplot but add vertical and horizontal lines at means
mean_fert <- mean(fert_data$fertilizer)
mean_height <- mean(fert_data$height)

ggplot(fert_data, aes(x = fertilizer, y = height)) +
  geom_point(size = 5) +
  geom_vline(xintercept = mean_fert, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean_height, linetype = "dashed", color = "red") +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (cm)",
    title = "Fertilizer and Plant Height with Mean Lines"
  ) +
  theme_classic()

#Transform height from cm to inches (1 inch = 2.54 cm) and recreate scatterplot

fert_data$height_inch <- fert_data$height / 2.54
ggplot(fert_data, aes(x = fertilizer, y = height_inch)) +
  geom_point(size = 5) +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (inches)",
    title = "Fertilizer and Plant Height in Inches"
  ) +
  theme_classic()


r_val <- round(cor(fert_data$fertilizer, fert_data$height), 5)


# Add correlation coefficient to original scatterplot
ggplot(fert_data, aes(x = fertilizer, y = height)) +
  geom_point(size = 5) +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (cm)",
    title = "Fertilizer and Plant Height",
    subtitle = paste("Correlation coefficient (r) =", r_val)
  ) +
  theme_classic()

## Regression line
model <- lm(height ~ fertilizer, data = fert_data)
ggplot(fert_data, aes(x = fertilizer, y = height)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (cm)",
    title = "Fertilizer and Plant Height with Regression Line"
  ) +
  theme_classic()

#Repeat this plot, with a new dataset of n 20 with higher variance. Simulate the data
set.seed(13)
n <- 20
fertilizer_new <- seq(0, 200, length.out = n)
height_new <- 32.2 + 0.238 * fertilizer_new + rnorm(n, mean = 0, sd = 10)

fert_data_new <- data.frame(
  fertilizer = fertilizer_new,
  height = height_new
)

ggplot(fert_data_new, aes(x = fertilizer, y = height)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Fertilizer (kg/ha)",
    y = "Plant height (cm)",
    title = "Fertilizer and Plant Height with Regression Line (New Data)"
  ) +
  theme_classic()


model <- lm(height ~ fertilizer, data = fert_data_new)
summary(model)


### New example
# Simulate data for a new example
set.seed(42)
# response variable is babygrowing pig average daily growth rate (g/day)
# predictor variable is protein intake (g/day)
n <- 30
protein_intake <- runif(n, 50, 150) # protein intake between 50 and 150 g/day
growth_rate <- 0.5 * protein_intake + rnorm(n, mean = 0, sd = 10) # growth rate with some random noise

pig_data <- data.frame(
  protein_intake = protein_intake,
  growth_rate = growth_rate
)

# Run the linear regression model
model_pig <- lm(growth_rate ~ protein_intake, data = pig_data)
summary(model_pig)
## Plotting the data and regression line

ggplot(pig_data, aes(x = protein_intake, y = growth_rate)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Protein Intake (g/day)",
    y = "Average Daily Growth Rate (g/day)",
    title = "Protein Intake and Pig Growth Rate"
  ) +
  theme_classic()
install.packages("qqplotr")

#plot qq plot to check normality of residuals

library(qqplotr)
ggplot(model_pig, aes(sample = resid(model_pig))) +
  stat_qq_point(size = 3) +
  stat_qq_line(color = "red") +
  #add confidence envelope
  stat_qq_band(level = 0.95, fill = "lightblue", alpha = 0.5) +
  labs(
    title = "QQ Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic()
#How to interpret qq plots with confidence envelopes:
# If the points in the QQ plot fall approximately along the red line and within the light blue confidence envelope, it suggests that the residuals are approximately normally distributed.
# If the points deviate significantly from the red line or fall outside the confidence envelope, it may indicate that the residuals are not normally distributed, which could violate the assumptions of linear regression.

# Plot residuals vs fitted values to check for homoscedasticity
ggplot(model_pig, aes(x = fitted(model_pig), y = resid(model_pig))) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_classic()
# If this doesn't look great, what other test can I do?
# You can perform the Breusch-Pagan test for heteroscedasticity using the 'lmtest' package in R. This test checks whether the variance of the residuals is constant (homoscedasticity) or if it varies with the fitted values (heteroscedasticity).
install.packages("lmtest")
library(lmtest)
#Do Breusch-Pagan test
bptest(model_pig)

#Test for linearity by plotting residuals vs fitted values. If there is a clear pattern (e.g., a curve), it may indicate non-linearity. You can also add a loess smoother to the plot to help visualize any potential non-linear patterns.
ggplot(model_pig, aes(x = fitted(model_pig), y = resid(model_pig))) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values with Loess Smoother",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_classic()


install.packages("patchwork")

library(ggplot2)
library(patchwork)

set.seed(42)

# --- Simulate some regression residuals (slightly non-normal for realism) ---
n <- 80
residuals_obs <- rnorm(n, mean = 0, sd = 2.5) + rnorm(n, 0, 0.3)
df <- data.frame(
  index = 1:n,
  residuals = residuals_obs
)

theme_slide <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  )


ggplot(df, aes(x = "", y = residuals)) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "steelblue") +
  labs(
    title = "Step 1: Our Observed Residuals",
    subtitle = "Each dot is one residual from the model",
    x = NULL,
    y = "Residual value"
  ) +
  theme_slide


ggplot(df, aes(x = "", y = residuals)) +
  geom_violin(
    fill = "lightblue",
    alpha = 0.4,
    color = "steelblue",
    width = 0.6,
    trim = FALSE
  ) +
  geom_jitter(width = 0.12, alpha = 0.6, size = 2, color = "steelblue") +
  labs(
    title = "Step 2: Violin Plot of Residuals",
    subtitle = "The shape shows how densely residuals cluster",
    x = NULL,
    y = "Residual value"
  ) +
  theme_slide


obs_mean <- mean(residuals_obs)
obs_sd <- sd(residuals_obs)

ggplot(df, aes(x = residuals)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "steelblue",
    alpha = 0.4,
    color = "white"
  ) +
  geom_density(color = "firebrick", linewidth = 1, linetype = "solid") +
  stat_function(
    fun = dnorm,
    args = list(mean = obs_mean, sd = obs_sd),
    color = "firebrick",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = max(residuals_obs) * 0.65,
    y = 0.18,
    label = "— Observed density",
    color = "firebrick",
    size = 4.5,
    hjust = 0
  ) +
  annotate(
    "text",
    x = max(residuals_obs) * 0.65,
    y = 0.16,
    label = "--- Theoretical Normal",
    color = "firebrick",
    size = 4.5,
    hjust = 0
  ) +
  labs(
    title = "Step 3: Compare to a Normal Distribution",
    subtitle = paste0(
      "Normal( μ = ",
      round(obs_mean, 2),
      ",  σ = ",
      round(obs_sd, 2),
      " )"
    ),
    x = "Residual value",
    y = "Density"
  ) +
  theme_slide


# Generate "expected" residuals from the theoretical normal, show side by side.

set.seed(99)
expected_residuals <- sort(qnorm(ppoints(n), mean = obs_mean, sd = obs_sd))
observed_sorted <- sort(residuals_obs)

df_compare <- data.frame(
  value = c(observed_sorted, expected_residuals),
  source = rep(c("Observed", "Expected\n(if perfectly Normal)"), each = n),
  rank = rep(1:n, 2)
)

ggplot(df_compare, aes(x = source, y = value, fill = source)) +
  geom_violin(alpha = 0.35, width = 0.7, trim = FALSE, color = NA) +
  geom_jitter(aes(color = source), width = 0.1, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  labs(
    title = "Step 4: Observed vs. Expected Residuals",
    subtitle = "If our data were truly Normal, both sides would look alike",
    x = NULL,
    y = "Residual value"
  ) +
  theme_slide +
  theme(legend.position = "none")


# Show the ranked pairs with connecting lines, transitioning to QQ space.

df_pairs <- data.frame(
  observed = observed_sorted,
  expected = expected_residuals,
  rank = 1:n
)

# 5a: Side-by-side with lines connecting matched quantiles
ggplot(df_pairs) +
  geom_segment(
    aes(x = 1, xend = 2, y = expected, yend = observed),
    alpha = 0.25,
    color = "grey50"
  ) +
  geom_point(
    aes(x = 1, y = expected),
    color = "firebrick",
    size = 2,
    alpha = 0.7
  ) +
  geom_point(
    aes(x = 2, y = observed),
    color = "steelblue",
    size = 2,
    alpha = 0.7
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Expected\n(Theoretical)", "Observed")
  ) +
  labs(
    title = "Step 5a: Matching Quantiles by Rank",
    subtitle = "Each line pairs an expected quantile with an observed one",
    x = NULL,
    y = "Residual value"
  ) +
  theme_slide


ggplot(df_pairs, aes(x = expected, y = observed)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "firebrick",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.7) +
  labs(
    title = "Step 5b: The QQ Plot!",
    subtitle = "Each point: x = theoretical quantile, y = observed quantile \n On the line -> data matches Normal",
    x = "Theoretical Quantiles (Normal)",
    y = "Observed Quantiles (Sample)"
  ) +
  coord_equal() +
  theme_slide


ggplot(df_pairs, aes(x = expected, y = observed)) +
  # Shaded regions for interpretation

  annotate(
    "rect",
    xmin = min(df_pairs$expected),
    xmax = max(df_pairs$expected),
    ymin = min(df_pairs$expected),
    ymax = max(df_pairs$expected),
    fill = "grey90",
    alpha = 0.3
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "firebrick",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.7) +
  # Annotations for deviations

  annotate(
    "text",
    x = min(df_pairs$expected) * 0.9,
    y = max(df_pairs$observed) * 0.95,
    label = "Points ABOVE line:\nobserved > expected\n(heavier left tail)",
    color = "grey30",
    size = 3.5,
    hjust = 0
  ) +
  annotate(
    "text",
    x = max(df_pairs$expected) * 0.5,
    y = min(df_pairs$observed) * 0.95,
    label = "Points BELOW line:\nobserved < expected\n(lighter right tail)",
    color = "grey30",
    size = 3.5,
    hjust = 0
  ) +
  labs(
    title = "Reading a QQ Plot",
    subtitle = "Points on the line = Normal  |  Deviations = non-Normality",
    x = "Theoretical Quantiles",
    y = "Observed Quantiles"
  ) +
  coord_equal() +
  theme_slide
