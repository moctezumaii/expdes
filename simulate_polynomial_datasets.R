# Fixed seed for reproducibility.
set.seed(571)

# This script writes two teaching datasets to the repository root.
# Dataset 1 is plant science adjacent with a quadratic mean structure.
# Dataset 2 is ecology adjacent with a cubic mean structure on the log link scale.
#
# I use deterministic quantiles with qnorm() and qpois() so the same CSV files are
# written every time the script runs. This keeps the assignment reproducible while
# still making the data look like realistic simulated observations.

# -----------------------------------------------------------------------------
# Dataset 1. Fertilizer rate and biomass
# -----------------------------------------------------------------------------
# Story:
# Greenhouse plants were grown under different fertilizer rates.
# Biomass increases at low to moderate fertilizer rates, then levels off and
# declines at the high end. That creates a true quadratic relationship.
#
# Simulation choices:
# - predictor: fertilizer_g_m2 is continuous
# - response: biomass_g is continuous
# - true mean: beta0 + beta1*x + beta2*x^2 with beta2 < 0
# - noise: qnorm() values add realistic scatter around the quadratic mean

n_biomass <- 60
fertilizer_g_m2 <- round(seq(10, 170, length.out = n_biomass), 1)
prob_biomass <- ((((seq_len(n_biomass) * 17) %% 59) + 0.5) / 60)

true_biomass <- 210 + 5.2 * fertilizer_g_m2 - 0.021 * fertilizer_g_m2^2
biomass_noise <- qnorm(prob_biomass, mean = 0, sd = 14)
biomass_g <- round(true_biomass + biomass_noise, 1)

plant_biomass <- data.frame(
  fertilizer_g_m2 = fertilizer_g_m2,
  biomass_g = biomass_g
)

write.csv(plant_biomass, "fertilizer_biomass_quadratic.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# Dataset 2. Seedling counts and canopy cover
# -----------------------------------------------------------------------------
# Story:
# Field crews counted oak seedlings across sites with different canopy cover.
# The count changes with canopy cover in a cubic pattern on the log scale.
#
# Simulation choices:
# - predictor: canopy_cover_pct is continuous
# - response: oak_seedlings is a count
# - true model: log(lambda) = beta0 + beta1*z + beta2*z^2 + beta3*z^3
#   where z is centered canopy cover
# - counts: qpois() maps fixed probabilities to Poisson counts so the output is
#   reproducible and still tied to a Poisson mean structure

n_seedlings <- 72
canopy_cover_pct <- round(seq(5, 95, length.out = n_seedlings), 1)
z <- canopy_cover_pct - 50
prob_seedlings <- ((((seq_len(n_seedlings) * 19) %% 71) + 0.5) / 72)

eta <- 1.7 + 0.028 * z - 0.0009 * z^2 - 0.000018 * z^3
lambda <- exp(eta)
oak_seedlings <- qpois(prob_seedlings, lambda = lambda)

seedling_counts <- data.frame(
  canopy_cover_pct = canopy_cover_pct,
  oak_seedlings = oak_seedlings
)

write.csv(seedling_counts, "seedling_counts_cubic.csv", row.names = FALSE)
