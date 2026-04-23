set.seed(571)

n <- 180

sucrose_pct <- round(runif(n, min = 5, max = 25), 1)
feeder_height_cm <- round(runif(n, min = 80, max = 220), 1)
shade_pct <- round(runif(n, min = 0, max = 90), 1)

feeder_color <- factor(
  sample(
    c("red", "yellow", "blue"),
    size = n,
    replace = TRUE,
    prob = c(0.45, 0.30, 0.25)
  ),
  levels = c("red", "yellow", "blue")
)

site <- factor(
  sample(
    c("meadow", "edge", "orchard", "forest"),
    size = n,
    replace = TRUE,
    prob = c(0.30, 0.25, 0.25, 0.20)
  ),
  levels = c("meadow", "edge", "orchard", "forest")
)

color_effect <- c(red = 0.25, yellow = 0.05, blue = -0.18)
site_effect <- c(meadow = 0.28, edge = 0.10, orchard = -0.04, forest = -0.22)

linear_predictor <- -0.2 +
  0.11 * sucrose_pct +
  0.0025 * (feeder_height_cm - 150) -
  0.006 * (shade_pct - 45) +
  color_effect[feeder_color] +
  site_effect[site]

lambda <- exp(linear_predictor)
visits <- rpois(n, lambda = lambda)

poisson_data <- data.frame(
  visits = visits,
  sucrose_pct = sucrose_pct,
  feeder_height_cm = feeder_height_cm,
  shade_pct = shade_pct,
  feeder_color = feeder_color,
  site = site
)

write.csv(poisson_data, "poisson_dataset.csv", row.names = FALSE)
