# ==============================================================================
# Simulation script for two teaching datasets
# Course: Experimental Design
# Purpose: Generate (1) a yield dataset with heteroscedasticity + interaction,
#          and (2) a seed-sprouting binary dataset for GLM intro.
# ==============================================================================

# ---- Setup -------------------------------------------------------------------
set.seed(2026)

library(dplyr)
library(tibble)

# ==============================================================================
# DATASET 1: Crop yield — continuous response with interaction + heteroscedasticity
# ==============================================================================
# Design:
#   - Response: yield_kgha (kg/ha)
#   - Continuous predictor: nitrogen_kg_ha (nitrogen application rate)
#   - Categorical predictor: variety (4 cultivars, unbalanced)
#   - True DGP: log(yield) = intercept + b_nitrogen * nitrogen + variety_effect
#                           + interaction + Normal error
#   - This creates multiplicative (log-normal) errors on the raw scale, producing
#     heteroscedasticity and nonlinearity that a log transform will fix.
# ==============================================================================

# Variety levels and sample sizes (unbalanced)
varieties <- c("Alpha", "Beta", "Gamma", "Delta")
n_per_variety <- c(30, 22, 18, 15)  # unbalanced

# Build design data frame
d1 <- tibble()
for (i in seq_along(varieties)) {
  d1 <- bind_rows(d1, tibble(
    variety = rep(varieties[i], n_per_variety[i]),
    nitrogen_kg_ha = round(runif(n_per_variety[i], min = 20, max = 200), 1)
  ))
}

# Parameters on the log scale
intercept     <- 6.0        # baseline log(yield) ~ exp(6) ≈ 403 kg/ha
b_nitrogen    <- 0.004      # main effect of nitrogen (on log scale)

# Variety intercept offsets (deviations from baseline)
variety_intercepts <- c(Alpha = 0, Beta = 0.15, Gamma = -0.10, Delta = 0.25)

# Interaction slopes: how nitrogen effect differs by variety
# This makes the interaction clearly significant
variety_slopes <- c(Alpha = 0, Beta = 0.003, Gamma = -0.002, Delta = 0.005)

# Residual SD on the log scale
sigma_log <- 0.18

# Simulate log(yield)
d1 <- d1 %>%
  mutate(
    log_yield = intercept +
      b_nitrogen * nitrogen_kg_ha +
      variety_intercepts[variety] +
      variety_slopes[variety] * nitrogen_kg_ha +
      rnorm(n(), 0, sigma_log),
    yield_kgha = round(exp(log_yield), 1)
  ) %>%
  select(variety, nitrogen_kg_ha, yield_kgha)

# Quick check: interaction should be significant
cat("--- Dataset 1 diagnostic check ---\n")
m_check <- lm(log(yield_kgha) ~ nitrogen_kg_ha * variety, data = d1)
cat("ANOVA (Type I) on log-transformed model:\n")
print(anova(m_check))
cat("\n")

# ==============================================================================
# DATASET 2: Seed sprouting — binary outcome for logistic/GLM discussion
# ==============================================================================
# Design:
#   - Response: sprout (0 or 1)
#   - Predictor: seed_size_mm (continuous)
#   - True DGP: logit(P(sprout)) = a + b * seed_size_mm
#   - Parameters chosen so lm() predictions clearly go outside [0, 1]
# ==============================================================================

n_seeds <- 120
a <- -6          # intercept: low probability at small sizes
b <-  1.2        # slope: steep increase

seed_size_mm <- round(runif(n_seeds, min = 1, max = 12), 2)
p_sprout     <- plogis(a + b * seed_size_mm)   # true probability
sprout       <- rbinom(n_seeds, size = 1, prob = p_sprout)

d2 <- tibble(
  seed_size_mm = seed_size_mm,
  sprout       = sprout
)

cat("--- Dataset 2 diagnostic check ---\n")
m2_check <- lm(sprout ~ seed_size_mm, data = d2)
preds <- predict(m2_check, newdata = tibble(seed_size_mm = seq(1, 12, 0.5)))
cat("Min predicted value:", round(min(preds), 3), "\n")
cat("Max predicted value:", round(max(preds), 3), "\n")

# ==============================================================================
# Write CSVs
# ==============================================================================

write.csv(d1, "data/yield_transform_dataset.csv", row.names = FALSE)
write.csv(d2, "data/seed_sprouting.csv",          row.names = FALSE)

cat("\nDatasets written to data/ directory.\n")
