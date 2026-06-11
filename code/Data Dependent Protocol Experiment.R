#  Simulation Study: Data-Dependent Protocol
#  Concept and idea: Lars Nørtoft Reiter
#  Code: Lars Nørtoft Reiter & Claude (Anthropic)

library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(2024)

# ---- Parameters --------------------------------------------
n_sim     <- 10^6
n         <- 100
df        <- n - 1
alpha     <- 0.05
crit      <- qt(1 - alpha / 2, df)   # ~1.984
threshold <- -2

# ---- Helpers -----------------------------------------------
t_stat <- function(x) {
  if (length(x) < 2) return(NA_real_)
  mean(x) / (sd(x) / sqrt(length(x)))
}

protocol_A <- function(x) x
protocol_B <- function(x) x[x >= threshold]

# ---- Simulation --------------------------------------------
sim <- replicate(n_sim, {
  x <- rnorm(n)
  c(A = t_stat(protocol_A(x)),
    B = t_stat(protocol_B(x)))
})

results <- as.data.frame(t(sim)) |>
  pivot_longer(everything(),
               names_to  = "protocol",
               values_to = "t") |>
  mutate(protocol = factor(
    protocol,
    levels = c("A", "B"),
    labels = c("(A)", "(B)")
  ))

# ---- Rejection rates for annotation ------------------------
reject <- results |>
  group_by(protocol) |>
  summarise(
    rate  = mean(abs(t) > crit, na.rm = TRUE),
    .groups = "drop"
  )|>
  mutate(label = sprintf("Rejection rate = %.2f%%", round(rate * 100,1)))

# ---- Reference t_9 density ---------------------------------
t_grid    <- seq(-6, 6, length.out = 1000)
ref       <- data.frame(t = t_grid, density = dt(t_grid, df = df))
ref_left  <- subset(ref, t <= -crit)
ref_right <- subset(ref, t >=  crit)

# ---- Palette -----------------------------------------------
pal <- c("(A)" = "#4393c3", "(B)" = "#c0392b")

# ---- Plot --------------------------------------------------
p <- ggplot(results, aes(x = t)) +
  # empirical density (smooth, filled)
  geom_density(aes(fill = protocol),
               alpha     = 0.85,
               colour    = NA,
               bw        = 0.10) +
  # rejection-region shading, layered ON TOP of the density
  geom_area(data = ref_left,  aes(x = t, y = density),
            fill = "grey35", alpha = 0.55, inherit.aes = FALSE) +
  geom_area(data = ref_right, aes(x = t, y = density),
            fill = "grey35", alpha = 0.55, inherit.aes = FALSE) +
  # theoretical t_99 reference curve
  geom_line(data = ref, aes(x = t, y = density),
            colour = "black", linewidth = 0.85, inherit.aes = FALSE) +
  # critical-value lines
  geom_vline(xintercept = c(-crit, crit),
             linetype = "longdash", linewidth = 0.45, colour = "grey30") +
  # rejection-rate label — upper right of each panel
  geom_text(data = reject,
            aes(label = label),
            x = 5.8, y = 0.415,
            hjust  = 1,
            size   = 5.5,
            family = "serif",
            inherit.aes = FALSE) +
  facet_wrap(~ protocol, ncol = 1) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = seq(-6, 6, 1),
                     limits = c(-6, 6),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.44),
                     breaks = seq(0, 0.4, 0.1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "t-statistic", y = "Density") +
  theme_classic(base_size = 14, base_family = "serif") +
  theme(
    # facet label
    strip.text       = element_text(size = 17, face = "bold", hjust = 0),
    strip.background = element_blank(),
    strip.placement  = "outside",
    # axes
    axis.line        = element_line(colour = "grey20", linewidth = 0.4),
    axis.ticks       = element_line(colour = "grey20", linewidth = 0.4),
    axis.text        = element_text(size = 12),
    axis.title.x     = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y     = element_text(size = 15, margin = margin(r = 10)),
    # panels
    panel.spacing    = unit(1.5, "lines"),
    plot.margin      = margin(14, 18, 14, 14)
  )

# ---- Save --------------------------------------------------
ggsave("protocol_A_and_B.png", p,
       width = 10, height = 7, dpi = 300)
