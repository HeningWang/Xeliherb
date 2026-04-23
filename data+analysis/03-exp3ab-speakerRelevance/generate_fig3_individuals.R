## generate_fig3_individuals.R
## ---------------------------------------------------------------------------
## Regenerates Figure 3 for the CogSci 2026 camera-ready revision with
## individual (posterior - prior) values overlaid on condition means + 95% CIs.
## Also computes per-cell directionality statistics (Reviewer 1).
##
## Inputs:
##   - data_preprocessed_allFactors.csv (raw slider data, 0-100 scale)
##   - fits/long_mixed_decision_point.rds (existing brms ZOIB fit; reference model)
##
## Outputs:
##   - ../../writing/Xeliherb_CogSci_final_submission/figures-exp3/results_exp_3_individuals.png
##   - cell_stats.csv (per-cell summary, written alongside this script)
## ---------------------------------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(ggplot2)
  library(aida)
  library(brms)
  library(tidybayes)
  library(ggdist)
  library(patchwork)
})

set.seed(1234)

setwd("/Users/heningwang/Documents/GitHub/Xeliherb/data+analysis/03-exp3ab-speakerRelevance")

theme_set(theme_aida())

CSP_colors <- c(
  "#7581B3", "#99C2C2", "#C65353", "#E2BA78", "#5C7457", "#575463",
  "#B0B7D4", "#66A3A3", "#DB9494", "#D49735", "#9BB096", "#D4D3D9",
  "#414C76", "#993333"
)

## --- Load raw data -----------------------------------------------------------
data <- read.csv("data_preprocessed_allFactors.csv")

data <- data %>%
  mutate(
    submission_id = as.factor(submission_id),
    informationSource = factor(informationSource, levels = c("indirect", "direct")),
    scienceTeam = factor(scienceTeam, levels = c("Cultivation", "Localization"))
  )

## --- Individual update on the same rescaled (0,1) axis used by CogSci2026.Rmd
## CogSci2026.Rmd line 485: update = (update + 0.05)/(100 + 0.05)
data_plot <- data %>%
  mutate(
    scienceTeam = factor(
      scienceTeam,
      levels = c("Cultivation", "Localization"),
      labels = c("intervention-oriented", "diagnostic-oriented")
    ),
    update_rescaled = (update + 0.05) / (100 + 0.05)
  )

## --- Build figure ------------------------------------------------------------
## Two stacked panels:
##   (A) Distribution panel: half-eye (density + median + IQR) per cell,
##       showing individual-level shape. Y-axis clipped to the bulk so the
##       reviewer's "are individuals all moving the same way" question is
##       visually answerable without forcing the mean panel to disappear.
##   (B) Zoomed mean panel: same mean + 95% bootstrap CI as the original
##       Fig. 3, y-axis tight so the interaction is visible.
## Panels share the x-axis; legend at top; CSP-colors preserved.

pd <- position_dodge(width = 0.5)

## Convenience: clip y-axis of panel A to the central bulk, but track how many
## points fall outside so we can annotate them rather than silently drop them.
y_lo <- -0.10   # covers ~99% of participants on the rescaled scale
y_hi <-  0.95

clipping_counts <- data_plot %>%
  group_by(informationSource, scienceTeam) %>%
  summarise(
    n_below = sum(update_rescaled < y_lo),
    n_above = sum(update_rescaled > y_hi),
    n_total = n(),
    .groups = "drop"
  )

## --- Panel A: distribution shape with individual points overlaid -----------
panel_A <- ggplot(
  data_plot,
  aes(x = informationSource, y = update_rescaled,
      fill = scienceTeam, colour = scienceTeam, group = scienceTeam)
) +
  geom_hline(yintercept = (0 + 0.05) / (100 + 0.05),
             linetype = "dashed", colour = "grey55", linewidth = 0.4) +
  ## Half-violin density (bulk of each cell)
  ggdist::stat_slab(
    aes(fill = scienceTeam),
    position = position_dodge(width = 0.5),
    side = "right",
    scale = 0.55,
    alpha = 0.35,
    colour = NA,
    normalize = "groups"
  ) +
  ## Raw individual points, jittered on the left of each density
  geom_point(
    position = position_jitterdodge(jitter.width = 0.10,
                                    dodge.width  = 0.5,
                                    seed = 42),
    alpha = 0.35,
    size = 0.9,
    stroke = 0
  ) +
  ## Median line + IQR (the "bulk" summary — robust to tails)
  stat_summary(
    fun.min = function(x) quantile(x, 0.25),
    fun.max = function(x) quantile(x, 0.75),
    fun     = median,
    geom = "pointrange",
    position = pd,
    size = 0.6,
    linewidth = 1.1,
    shape = 21,
    fill = "white",
    stroke = 1.1
  ) +
  scale_colour_manual(values = CSP_colors) +
  scale_fill_manual(values = CSP_colors) +
  coord_cartesian(ylim = c(y_lo, y_hi), clip = "off") +
  labs(
    x = NULL,
    y = "Change (individuals)",
    colour = "Speaker Goal Orientation",
    fill   = "Speaker Goal Orientation"
  ) +
  theme_aida() +
  theme(
    legend.position = "top",
    text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    plot.margin = margin(8, 8, 2, 14)
  )

## --- Panel B: zoomed means + 95% bootstrap CI (same geometry as original) --
## Y-axis tight to the mean range so the interaction is the visual signal.
mean_range <- data_plot %>%
  group_by(informationSource, scienceTeam) %>%
  summarise(m = mean(update_rescaled), .groups = "drop")
mean_lo <- min(mean_range$m) - 0.06
mean_hi <- max(mean_range$m) + 0.06

panel_B <- ggplot(
  data_plot,
  aes(x = informationSource, y = update_rescaled,
      colour = scienceTeam, group = scienceTeam)
) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    width = 0.15,
    position = pd,
    linewidth = 0.9
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "point",
    position = pd,
    size = 3.2
  ) +
  scale_colour_manual(values = CSP_colors) +
  coord_cartesian(ylim = c(mean_lo, mean_hi)) +
  labs(
    x = "Directness of Information",
    y = "Mean change\n(95% CI)",
    colour = "Speaker Goal Orientation"
  ) +
  theme_aida() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    plot.margin = margin(2, 8, 8, 14)
  )

## --- Compose with patchwork --------------------------------------------------
fig <- (panel_A / panel_B) +
  plot_layout(heights = c(2.2, 1)) +
  plot_annotation(tag_levels = "A")

out_path <- "../../writing/Xeliherb_CogSci_final_submission/figures-exp3/results_exp_3_individuals.png"
ggsave(out_path, plot = fig, width = 7.6, height = 6.2, units = "in", dpi = 400)
cat("Saved figure to:", normalizePath(out_path), "\n")
cat("Clipped points (|y| outside panel-A range):\n")
print(as.data.frame(clipping_counts))

## ---------------------------------------------------------------------------
## Per-cell directionality statistics
## ---------------------------------------------------------------------------

## Raw cell descriptives on the 0-100 scale of 'update'
raw_cell_stats <- data %>%
  mutate(
    sign_cat = case_when(
      update >  0 ~ "positive",
      update == 0 ~ "zero",
      update <  0 ~ "negative"
    )
  ) %>%
  group_by(informationSource, scienceTeam) %>%
  summarise(
    N            = n(),
    mean_update  = mean(update),
    sd_update    = sd(update),
    median_update= median(update),
    pct_pos      = mean(update  > 0) * 100,
    pct_zero     = mean(update == 0) * 100,
    pct_neg      = mean(update  < 0) * 100,
    n_pos        = sum(update  > 0),
    n_zero       = sum(update == 0),
    n_neg        = sum(update  < 0),
    min_update   = min(update),
    max_update   = max(update),
    q25          = quantile(update, 0.25),
    q75          = quantile(update, 0.75),
    .groups = "drop"
  )

## ---------------------------------------------------------------------------
## Model-based per-cell mean change (posterior - prior) and 95% CrI
## from the already-fitted long_mixed_decision_point ZOIB model.
## ---------------------------------------------------------------------------

fit <- readRDS("fits/long_mixed_decision_point.rds")

## Reconstruct data_long exactly as the model was fit.
data_long <- data %>%
  select(decision1, decision2, informationSource, scienceTeam, submission_id) %>%
  pivot_longer(
    cols = c(decision1, decision2),
    names_to = "decision_point",
    values_to = "decision"
  ) %>%
  mutate(
    decision_point = if_else(decision_point == "decision1", 0L, 1L),
    informationSource = relevel(informationSource, ref = "indirect"),
    scienceTeam = relevel(scienceTeam, ref = "Cultivation"),
    decision = decision / 100
  )

nd <- tidyr::expand_grid(
  informationSource = factor(c("indirect", "direct"),
                             levels = levels(data_long$informationSource)),
  scienceTeam = factor(c("Cultivation", "Localization"),
                       levels = levels(data_long$scienceTeam)),
  decision_point = c(0L, 1L)
)

cell_epred <- nd %>%
  add_epred_draws(
    fit,
    ndraws = 4000,
    re_formula = NA,
    allow_new_levels = TRUE
  ) %>%
  ungroup() %>%
  select(.draw, informationSource, scienceTeam, decision_point, .epred)

change_draws <- cell_epred %>%
  pivot_wider(names_from = decision_point, values_from = .epred,
              names_prefix = "dp") %>%
  mutate(change = dp1 - dp0)   # posterior - prior, on (0,1) scale

## Summaries on the original 0-100 scale (multiply by 100)
model_cell_stats <- change_draws %>%
  group_by(informationSource, scienceTeam) %>%
  summarise(
    post_mean_change = mean(change) * 100,
    crI_low          = quantile(change, 0.025) * 100,
    crI_high         = quantile(change, 0.975) * 100,
    p_gt0            = mean(change > 0),
    .groups = "drop"
  )

full_cell_stats <- raw_cell_stats %>%
  left_join(model_cell_stats, by = c("informationSource", "scienceTeam")) %>%
  mutate(
    cell_label = paste0(
      as.character(informationSource), " x ",
      ifelse(as.character(scienceTeam) == "Cultivation",
             "intervention", "diagnostic")
    )
  ) %>%
  relocate(cell_label)

write.csv(full_cell_stats, "cell_stats.csv", row.names = FALSE)
cat("Saved per-cell stats to:", normalizePath("cell_stats.csv"), "\n\n")

cat("=== Per-cell summary ===\n")
print(as.data.frame(full_cell_stats))
cat("\n")

## ---------------------------------------------------------------------------
## Extra individual-level diagnostics per cell
## ---------------------------------------------------------------------------

extra_cell_stats <- data %>%
  group_by(informationSource, scienceTeam) %>%
  summarise(
    N               = n(),
    ## Boundary-like responses on the raw change scale
    pct_at_max_ceiling = mean(update >= 80) * 100,   # near-ceiling change
    pct_large_neg      = mean(update <= -10) * 100,  # notable negative
    ## Dip/bimodality proxies
    dip_pct_small_abs  = mean(abs(update) < 5) * 100,
    ## Spread
    IQR_update = IQR(update),
    ## Count of near-boundary priors or posteriors
    n_prior_0_or_100      = sum(decision1 == 0 | decision1 == 100),
    n_posterior_0_or_100  = sum(decision2 == 0 | decision2 == 100),
    .groups = "drop"
  )

cat("\n=== Extra per-cell diagnostics ===\n")
print(as.data.frame(extra_cell_stats))

## Hartigan's dip test per cell for bimodality — optional, only if diptest is available
if (requireNamespace("diptest", quietly = TRUE)) {
  dip_stats <- data %>%
    group_by(informationSource, scienceTeam) %>%
    summarise(
      dip_p = diptest::dip.test(update)$p.value,
      .groups = "drop"
    )
  cat("\n=== Hartigan's dip test (p > .05 = unimodal) ===\n")
  print(as.data.frame(dip_stats))
} else {
  cat("\n(diptest package not available; skipping Hartigan's dip test)\n")
}
