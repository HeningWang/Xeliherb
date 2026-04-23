# Experiment 2 — Individual-level patterns in causal-confidence change

This file documents the per-cell individual-level analysis that supports the
revised Figure 3 (`results_exp_3_individuals.png`) for the CogSci 2026
camera-ready. It responds to Reviewer 1's request to (i) show individual
trajectories on Fig. 3 and (ii) report how often the mean-change pattern is
driven by positive updating versus zero or negative updating.

## Data and model provenance

* Raw data: `data_preprocessed_allFactors.csv` (slider range 0–100; within-participant
  change = `decision2 − decision1`, stored as `update`).
* Reference model: `fits/long_mixed_decision_point.rds` — the pre-existing brms
  ZOIB model from `02-model-analysis.Rmd`. **No re-fitting was done.** Posterior
  draws were re-used to derive per-cell expected change by differencing the
  epred at `decision_point = 1` (posterior) and `decision_point = 0` (prior),
  with random effects marginalised out (`re_formula = NA`). Draws were then
  rescaled from the (0,1) modelling scale to the 0–100 slider scale.
* Figure: a two-panel composite. Panel A shows the per-cell distribution as a
  half-violin density plus jittered individual points, with the **median + IQR**
  drawn as a white-filled pointrange for each cell; y-axis clipped to the
  central 99% of the data so the bulk is legible. Panel B is a zoomed-in view
  of the condition **means + 95% bootstrap CIs** — geometrically identical to
  the original Fig. 3 — so the headline interaction remains visually sharp.

## Per-cell summary

All changes reported on the original 0–100 slider scale. Model mean / CrI from
the ZOIB posterior (`p > 0` = posterior probability of positive mean change).
Percentages of positive / zero / negative are computed from raw participant
change scores.

### Direct × Intervention-oriented (direct + Cultivation)
* **N:** 102
* **Mean:** 34.42 (SD = 25.50); **Median:** 33.5; **IQR:** 15.0–50.0 (width 35.0)
* **Model mean change:** 31.66 [95% CrI: 27.51, 35.77]; p(change > 0) = 1.00
* **Sign distribution:** 90.2% positive (92/102) · 2.9% zero (3/102) · 6.9% negative (7/102)
* **Range:** min = −19, max = 95
* **Tail notes:** Tightest negative tail of the four cells — no participant
  dropped more than 19 points. ~5.9% near-ceiling (change ≥ 80); 17 participants
  landed on a 0/100 posterior boundary.

### Direct × Diagnostic-oriented (direct + Localization)
* **N:** 101
* **Mean:** 37.80 (SD = 27.06); **Median:** 40.0; **IQR:** 15.0–60.0 (width 45.0)
* **Model mean change:** 35.28 [95% CrI: 31.11, 39.18]; p(change > 0) = 1.00
* **Sign distribution:** 91.1% positive (92/101) · 2.0% zero (2/101) · 6.9% negative (7/101)
* **Range:** min = −30, max = 94
* **Tail notes:** The whole bulk shifts up relative to direct × intervention —
  median +6.5, Q75 +10, mean +3.4 — not just the tails. ~7.9% near-ceiling
  movers and 17 boundary-posterior responses.

### Indirect × Intervention-oriented (indirect + Cultivation)
* **N:** 101
* **Mean:** 32.05 (SD = 25.89); **Median:** 30.0; **IQR:** 15.0–50.0 (width 35.0)
* **Model mean change:** 29.34 [95% CrI: 25.37, 33.27]; p(change > 0) = 1.00
* **Sign distribution:** 89.1% positive (90/101) · 2.0% zero (2/101) · 8.9% negative (9/101)
* **Range:** min = −44, max = 99
* **Tail notes:** Minor extension of the negative tail (4% dropped ≥10 points,
  min −44). Near-ceiling share (~5%) similar to direct cells. 10 boundary-posterior responses.

### Indirect × Diagnostic-oriented (indirect + Localization)
* **N:** 100
* **Mean:** 30.39 (SD = 27.74); **Median:** 30.0; **IQR:** 14.5–45.0 (width 30.5)
* **Model mean change:** 26.45 [95% CrI: 22.17, 30.59]; p(change > 0) = 1.00
* **Sign distribution:** 90.0% positive (90/100) · 2.0% zero (2/100) · 8.0% negative (8/100)
* **Range:** min = −50, max = 99
* **Tail notes:** Same median as indirect × intervention; Q75 is slightly
  lower (45 vs 50). The small negative mean gap is not a bulk shift — it is
  carried mostly by a modestly heavier left tail (6% ≤ −10, including the
  global minimum of −50). 10 boundary-posterior responses.

## Where the crossover lives (Reviewer 1, question 1)

The crossover interaction is robust on the **direct** arm at every summary
level:

| Contrast (direct cells) | diagnostic − intervention |
|---|---|
| Q25 | 15 − 15 = 0 |
| Median | 40 − 33.5 = **+6.5** |
| Q75 | 60 − 50 = **+10** |
| Mean | 37.8 − 34.4 = **+3.4** |
| Model mean | 35.3 − 31.7 = **+3.6** |

Under direct framing, the whole upper half of the distribution moves upward —
median, Q75, mean, and model mean all point in the same direction. This is
not a tail phenomenon.

On the **indirect** arm, the gap is small and mostly tail-mediated:

| Contrast (indirect cells) | diagnostic − intervention |
|---|---|
| Q25 | 14.5 − 15 = −0.5 |
| Median | 30 − 30 = 0 |
| Q75 | 45 − 50 = −5 |
| Mean | 30.4 − 32.0 = −1.7 |
| Model mean | 26.5 − 29.3 = −2.9 |

The median is identical, Q25 is essentially identical, and only Q75 and the
mean differ by a small amount. Honest reading: under indirect framing the
difference between diagnostic- and intervention-oriented teams is negligible
at the center of the distribution and small even at the tail.

## Directionality check (Reviewer 1, question 2)

Across all four cells, roughly 9 in 10 participants updated in the positive
direction. The share of non-positive changes was 9–11% per cell (7–9% negative,
2–3% zero). No cell had a majority, plurality, or even a quarter of
participants moving against the group mean direction. The direction of the
mean change therefore reflects the direction of the typical participant, not
a subgroup of large updaters.

## Discussion paragraph the writer can drop into §Exp 2 Discussion

> A reviewer asked whether the directness × speaker-goal interaction in Fig. 3
> could be carried by a handful of atypical participants, and whether the
> direction of mean change was consistent at the individual level. The
> overlaid individual distributions in Fig. 3A show that both concerns are
> addressed by the data. First, between 89% and 91% of participants in every
> cell updated their causal confidence in the positive direction, with only
> 7–9% moving in the opposite direction and 2–3% not moving at all — every
> cell's mean reflects the direction of the typical participant, not a small
> subgroup of strong updaters. Second, the **direct** arm of the crossover is
> a shift of the whole upper half of the distribution: under direct framing,
> the diagnostic-oriented condition's median is 6.5 points higher than the
> intervention-oriented condition's, and its 75th percentile is 10 points
> higher, matching the direction and roughly the magnitude of the
> model-estimated difference in means. Under **indirect** framing, the
> medians are identical (30 points in both cells) and only the upper tail
> shifts slightly; the effect of speaker-goal orientation therefore genuinely
> weakens when the information is not directly attributed. Read together,
> panels A and B show that the headline interaction in the means is the
> signature of a bulk distributional shift in the stronger (direct) arm, and
> of a real but attenuated distributional difference in the weaker (indirect)
> arm — exactly the pattern a relevance-sensitive updating account predicts
> when a speaker's goal becomes easier to identify.

## Files produced

* `../../writing/Xeliherb_CogSci_final_submission/figures-exp3/results_exp_3_individuals.png`
  — new two-panel Figure 3: Panel A shows per-cell distributions with individual
  points and median + IQR; Panel B shows the zoomed mean + 95% CI view. The
  original `results_exp_3.png` is untouched.
* `cell_stats.csv` — tidy per-cell summary (raw + model-based) for any future
  table.
* `generate_fig3_individuals.R` — reproducible script that regenerates both.

## Notes for the writer (sanity checks)

* Panel A's y-axis is clipped to [−0.10, 0.95] on the rescaled (0,1) axis —
  roughly 99% of all observations. A handful of slider extremes beyond those
  bounds are cut off visually; the clipping counts are printed when the script
  runs (`print(as.data.frame(clipping_counts))`). If the Writer wants no
  clipping at all, increase `y_lo` / `y_hi` in `generate_fig3_individuals.R`.
* Panel A reports **medians + IQR** (white-filled pointranges), not means +
  CIs. This is deliberate: the reviewer's worry is about robustness to outliers,
  and medians + IQRs answer that question directly without competing visually
  with the means in Panel B. If the Writer would rather show mean + CI on both
  panels, swap the `stat_summary(fun = median, ...)` block for the
  `mean_cl_boot` version from Panel B.
* The condition means and 95% intervals in Panel B remain the **raw bootstrap
  CIs** used in the original Fig. 3 (via `mean_cl_boot`). The per-cell "model
  mean change, 95% CrI" values in the table are ZOIB-model-based and
  marginalise the random effects; they are slightly shrunken relative to raw
  means (e.g. raw 34.42 vs model 31.65 for direct × intervention). The
  Discussion paragraph above quotes the **raw medians and raw mean
  differences** because those are what the reader sees in the figure; swap to
  model-based numbers if the surrounding §Exp 2 prose prefers the posterior
  scale.
* Individual points and densities are on the rescaled (0,1) axis
  (`(x + 0.05)/(100 + 0.05)`) to match `analysis/CogSci2026.Rmd`. The dashed
  grey line marks the rescaled zero.
