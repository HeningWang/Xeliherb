# Camera-Ready Changes

Tracks substantive edits between `main_submitted.tex` (frozen 2026-04-23) and `main.tex`.

To see a visual diff:

```bash
cd writing/Xeliherb_CogSci_camera_ready
latexdiff main_submitted.tex main.tex > main_diff.tex
latexmk -pdf main_diff.tex
```

---

## Reviewer comments (verbatim)

### R1 (reviewer 1) — score 4/5 — Linguistics, Psychology

> This is a very interesting study, well-designed, that revealed pragmatic factors (i.e., directness of communication and context of advising others) influence causal interpretation of correlational language. The findings are conveyed clearly by the good quality of writing and will be of interest to a broad audience in cognitive science.
>
> Though the effect was small, it is still puzzling why opposite patterns appeared between Speaker goal orientation and directness of communication. Intuitively, in particular, no difference would be expected for the impersonal context regardless of speakers's goals if listeners become less sensitive to speakers' goals. Could it be due to large individual differences? Does the mean change pattern always reflect positive changes only (prior to posterior)? Added individual data points to Fig. 3. may be helpful to compare individual patterns with the group pattern.

### R2 (reviewer 2) — score 3/5 — Cognitive Science, Linguistics

> The Introduction is clear enough but I missed a crucial motivation for the study — what is the difference between a Preference in Assignments of Cause and Effect and a pragmatic inference? Without a better understanding of this difference, it didn't jump out to me what the broader theoretical relevance of this work is. The authors will likely want to lengthen the introduction (and cut elsewhere) to make these points clearer. In the Conclusion, the authors write "These findings are difficult to capture with accounts that relate causal enrichments to a simple bias or heuristic, but follow naturally from a pragmatic account in which causal implicatures arise through reasoning about the communicative context." In the revision, the authors will want to explain this distinction in detail.
>
> The experiments seem to be well-designed and provide valuable insight beyond Lassiter and Franke's original forced-choice paradigm: the authors verify that some participants are in fact making a causal inference and demonstrate how (communicating about) this causal inference can differ across tasks contexts. These strike me as incremental findings, but still worth a place in the conference proceedings. The authors should do more to explain the theoretical impact of their claim that causal interpretations of correlational statements arise through pragmatic inference. In other words, why does this matter?

### R3 (reviewer 3) — score 3/5 — Cognitive Science, Philosophy, Psychology

> The paper aims to experimentally investigate how information about statistical correlations provided to participants in a fictional scenario is interpreted and further communicated by them in causal terms and how it shapes their decision-making. The topic is interesting and the paper is well written (there are a couple of typos like "procudeure" on p. 3).
>
> I'm not sure what the general motivation and goal of the investigation are. The authors say that it shows that it shows "that correlational language can strengthen causal beliefs in decision-making contexts". Given that it is well-known that people conflate correlation with causation (hence the common disclaimers in stats classes), this conclusion risks sounding trivial.
>
> Also, I'm not sure that the experimental design is fully sound. For instance, why not letting participants choose between three options (only heliherb, only ralocrop, or both)? If they believe that ralocrop causes heliherb, the second choice may be also sensible and provide stronger evidence for the authors' claim.

### R4 (metareviewer) — score 4/5 — Cognitive Science, Psychology

> The reviewers are in good agreement. The paper is well written and the results provide interesting new data on how people move to attribute cause to correlational evidence. The topic is of broad interest for Decision making and general cognition.
>
> Two recommend full paper plus poster, one plus talk.

---

## 2026-04-23 — HW

Rewrote Introduction paragraphs 1–3. The submitted version introduced the pragmatic view only via Reichenbach and went straight from Gershman/PACE into Lassiter's positive evidence. The revision contrasts two accounts of causal enrichment — a structural-bias account (PACE; Gershman re-framed as automatic/linguistic) and a pragmatic-inference account (Reichenbach + Sperber & Wilson + Goodman/Degen) — and adds a closing paragraph that spells out their diverging predictions (stable vs. context-sensitive enrichment).

Addresses R2 (PACE-vs-pragmatic distinction missing in Introduction)

## 2026-04-27 — HW (Introduction tightening)

Tightened the rest of the Introduction so it stops repeating the new paragraph 1–3 framing.

- "However, \citeauthor{lassiter2024rationality} also provide positive evidence in favor of a *pragmatic inference view*…" → "Consistent with this, \citet{lassiter2024rationality} provide positive evidence for the pragmatic account." The old "However" pivoted against the wrong thing once the pragmatic account was already on the table.
- Cut the "Moreover, the pragmatic account predicts… speaker's goal… overheard" sentence; the new paragraph 4 already says this. Downstream reference now reads "the pragmatic factors identified above."
- Cut the "While this study provides some evidence for the pragmatic inference view," preamble in the contributions paragraph.
- Verbosity trims throughout: "for participants who then had to make an important (binary) decision" → "to participants facing a binary decision"; "Based on the received information, participants then had to choose from two options, namely to cultivate" → "Based on this information, participants chose to cultivate"; "Evidence for a certain degree of *causal belief*… proportion of choices of option *both*" → "Evidence for *causal belief*… rate of *both*-choices"; "indistinguishable from a statement that used interventionist language to communicate a strong sense of causal relation" → "indistinguishable from the interventionist condition".
- Typos: `xerliherb` → `xeliherb`; `decision-maker beliefs` → `believes`; `subsequent decision` → `subsequent decisions`.

## 2026-04-27 — HW (Experiment 2 individual-differences analysis)

Replaced Figure 3 with a two-panel composite (`results_exp_3_individuals.png`). Panel A shows per-cell distributions with individual change-score points and median + IQR overlays; Panel B is the zoomed condition means with bootstrap 95% CIs, geometrically the same plot as the original Fig. 3. Caption rewritten to match.

Added a closing paragraph at the end of §Exp 2 Results (originally drafted in §Exp 2 Discussion, then moved and condensed from four sentences to three). It reports the per-cell positive/zero/negative split — 89–91% positive in every cell, 7–9% negative, 2–3% zero — and the descriptive structure of the crossover: under direct communication the diagnostic-oriented cell's median sits 6.5 points above the intervention-oriented cell's, with the 75th percentile 10 points above; under indirect framing the medians are identical (30 points in both cells) and only the upper tail differs slightly. The earlier interpretive closer ("exactly the pattern a relevance-sensitive updating account predicts…") was dropped, since the surrounding Discussion already does that work.

Numbers from `data+analysis/03-exp3ab-speakerRelevance/findings-individuals-for-cogsci2026-r1.md`. No models re-fit; numbers re-summarised from the existing ZOIB posterior.

Addresses R1's two questions about Fig. 3: individual trajectories are now visible, and the per-cell directionality answers whether the mean change reflects positive changes only (yes, in 89–91% of participants per cell).

## 2026-04-27 — HW (Conclusion: pragmatic-vs-bias defense)

Replaced the two-paragraph opener of the Conclusion — a generic recap sentence followed by the one-sentence "difficult to capture… simple bias or heuristic" assertion — with a single denser paragraph. The new paragraph keeps the tension framing of the recap, names the three signatures the paper offers (directness modulating belief updating with the input statement held constant, Exp. 2; advice-giving producing more causal reformulation than self-recall, Exp. 1; the speaker-goal contrast reversing sign as directness is reduced, Exp. 2), and closes with the bias-vs-pragmatic contrast: a context-insensitive surface-form bias would have to postulate distinct biases for distinct framings; a pragmatic account predicts the constellation directly. The original framing ("difficult to capture with… simple bias or heuristic… follows naturally from a pragmatic account") is preserved inside the new paragraph.

Addresses R2 (Conclusion needs to explain the bias-vs-pragmatic distinction in detail; "why does this matter?").

## 2026-04-27 — HW (Experiment 1 cuts)

Two compression cuts in Experiment 1, no substantive content lost.

- §Experiment 1 opening paragraph: cut the two-sentence motivation ("However, such choice behavior provides only indirect evidence... In particular, it remains unclear...") since the same point already lives in the Introduction's contributions paragraph.
- §Experiment 1 Materials: cut the three opening sentences that re-described the *xeliherb*-paradigm and the binary decision (already covered in §Procedure). Also collapsed the three-sentence interpretation rationale before the Results section ("Importantly, the recall task was not framed as reproducing the exact wording... Under this interpretation... We therefore expect...") into a single sentence stating the expectation.

## 2026-04-27 — HW (Experiment 2 Results revision and cuts)

Restructured §Experiment 2 Results.

- Tightened the figure-preview opening: dropped the hedged "the plot suggests… possibly a credible interaction" preamble; the model paragraphs below state the same thing with numbers.
- Compressed the model-description paragraph: reordered so the unusual modelling choice (ZOIB likelihood) leads, removed the "Because participants showed substantial heterogeneity…" preface and the standalone restatement of the prior-as-baseline design.
- Tightened the decision-order paragraph: reduced the "Fixed effects were estimated on the logit scale, but…" methodological aside to a single trailing sentence ("Effects below are reported as posterior expected predictions on the 0–100 slider scale."); finding now leads.
- Moved the standalone "individual differences" paragraph from between the model description and the fixed-effects results to immediately before the per-cell distributions paragraph, so the two form one block on individual-level patterns. The combined block now directly answers R1's question about whether the asymmetry could be due to large individual differences.

## 2026-04-27 — HW (Editor pass)

One copy-edit pass across the body. Substantive fixes:

- `procudeure` → `procedure` on p. 3 (R3-flagged).
- Subject-verb agreement in the abstract: `affect` → `affects`, plus a missing conjunction.
- §Exp 2 Results: factor name standardised from `\textsc{information source}` to `\textsc{directness of information}` to match Methods (2 occurrences); awkward "made participants endorse an action which is rational for a belief in causation more" → "made participants more likely to endorse an action that is rational under a belief in causation".
- §Exp 2 Discussion: misplaced parenthesis and missing article in "influences yield of *xeliherb)*." → "influences the yield of *xeliherb*)."; possessive agreement in the same sentence (`participant's causal belief` → `participants' causal beliefs`); `attenuates` → `attenuate`; `maybe` → `possibly`.
- Conclusion: removed a contradictory phrase introduced in an earlier merge ("under direct communication the speaker-goal contrast reversed sign as directness was reduced" — the two clauses said opposite things); `enrichments` → `enrichment` in the closing reference clause.
- Hyphen → en-dash in numeric ranges (`0–1`, `0–100`, `A--C`); a stray space before `\footnote`; a doubled space; a missing space after a colon; a trailing space in the title; a comma before "and they should be clearly separated" in the Introduction; `paradism` → `paradigm`.

Addresses R3 (typo "procudeure").
