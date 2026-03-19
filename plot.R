library(ggplot2)
library(gridExtra)

# ── colour palette ──────────────────────────────────────────────────────────
col_farm <- c("Farm 1" = "#2166ac", "Farm 2" = "#d6604d")

# ── shared theme (no units / axis text) ─────────────────────────────────────
base_theme <- function(title) {
  list(
    theme_classic(base_size = 11),
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      plot.title       = element_text(face = "bold", size = 10, hjust = 0.5),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.size  = unit(0.4, "cm"),
      panel.grid       = element_blank()
    ),
    labs(title = title, x = "Diet", y = "Weight Gained")
  )
}

# helper: interaction (both-farms) plot
int_plot <- function(df, title) {
  ggplot(df, aes(x = Diet, y = Weight, colour = Farm, group = Farm)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    scale_colour_manual(values = col_farm) +
    base_theme(title)
}

# helper: single-factor means bar chart
means_plot <- function(df, xvar, title) {
  agg <- aggregate(Weight ~ get(xvar), data = df, FUN = mean)
  names(agg) <- c("Level", "Weight")
  ggplot(agg, aes(x = Level, y = Weight)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    base_theme(title) +
    labs(x = xvar) +
    theme(legend.position = "none")
}

# ═══════════════════════════════════════════════════════════════════════════
# SET 1 — ADDITIVE  (parallel lines, clear main effects, no interaction)
# ═══════════════════════════════════════════════════════════════════════════
dat1 <- data.frame(
  Diet   = rep(c("D1","D2","D3"), 2),
  Farm   = rep(c("Farm 1","Farm 2"), each = 3),
  Weight = c(10, 15, 20,   # Farm 1 — increases by 5
             16, 21, 26)   # Farm 2 — same slope, shifted up
)

p1a <- int_plot(dat1,   "Set 1 — Additive\n(Both Factors)")
p1b <- means_plot(dat1, "Diet",  "Set 1 — Diet Means")
p1c <- means_plot(dat1, "Farm",  "Set 1 — Farm Means")

# ═══════════════════════════════════════════════════════════════════════════
# SET 2 — INTERACTION + MAIN EFFECTS  (non-parallel, both factors differ)
# ═══════════════════════════════════════════════════════════════════════════
dat2 <- data.frame(
  Diet   = rep(c("D1","D2","D3"), 2),
  Farm   = rep(c("Farm 1","Farm 2"), each = 3),
  Weight = c(10, 20, 14,   # Farm 1 — peaks at D2
             14, 22, 34)   # Farm 2 — keeps rising steeply
)

p2a <- int_plot(dat2,   "Set 2 — Interaction + Main Effects\n(Both Factors)")
p2b <- means_plot(dat2, "Diet",  "Set 2 — Diet Means")
p2c <- means_plot(dat2, "Farm",  "Set 2 — Farm Means")

# ═══════════════════════════════════════════════════════════════════════════
# SET 3 — INTERACTION ONLY  (lines cross, but all marginal means are equal)
# ═══════════════════════════════════════════════════════════════════════════
dat3 <- data.frame(
  Diet   = rep(c("D1","D2","D3"), 2),
  Farm   = rep(c("Farm 1","Farm 2"), each = 3),
  Weight = c(22, 10, 18,   # Farm 1 — high-low-mid
             10, 22, 18)   # Farm 2 — mirrors Farm 1 at D1 & D2, same at D3
)
# Diet means:  D1=(22+10)/2=16, D2=(10+22)/2=16, D3=(18+18)/2=18  ≈ flat
# Farm means:  F1=(22+10+18)/3=16.67, F2=(10+22+18)/3=16.67       equal

p3a <- int_plot(dat3,   "Set 3 — Interaction Only\n(Both Factors)")
p3b <- means_plot(dat3, "Diet",  "Set 3 — Diet Means")
p3c <- means_plot(dat3, "Farm",  "Set 3 — Farm Means")

# ═══════════════════════════════════════════════════════════════════════════
# ARRANGE  3 × 3 GRID
# ═══════════════════════════════════════════════════════════════════════════
grid.arrange(
  p1a, p1b, p1c,
  p2a, p2b, p2c,
  p3a, p3b, p3c,
  ncol = 3,
  top  = grid::textGrob(
    "Additive vs Interaction Effects in a Two-Factor Design",
    gp = grid::gpar(fontsize = 13, fontface = "bold")
  )
)
