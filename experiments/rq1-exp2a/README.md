# Experiment 2a (RQ1b): truth-value judgement

Journal Experiment 2a, as specified in the Dissertation repository at
`subprojects/sub1-xeliherb/drafts/journal-background/sub1_journal2026_rq1_rq2_experiment_implementation_plan_v1.md`
and preregistered in `sub1_journal2026_rq1_preregistration_v1.tex` next to it.
Participants read the colonist's sentence (1a) *associated* or (1b) *causes* of Experiment 1, learn what the
researchers found (ground truth H, a common cause, or G, ralocrop raises yield), answer a comprehension
question about the finding until they are correct, and judge the report *wrong*, *partly right* or *right*.

## Conditions and Prolific

The four cells (`trials/conditions.csv`) run as four Prolific studies, and each study link names its cell:

```
https://heningwang.github.io/Xeliherb/experiments/rq1-exp2a/?condition=1a-H&PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}
```

with `condition=1a-H` (critical), `1b-H` (false control), `1b-G` (true control) or `1a-G` (underinformative).
A link without a valid condition shows a "cannot start" page.

## Before launch

In `src/magpie.config.js`: enter the magpie `experimentId`, the Prolific completion code, and set `mode: 'prolific'`.
Pushing to `main` deploys to GitHub Pages.

## Local test

`npm ci`, then `npm run serve -- --port 8097` and open http://localhost:8097/?condition=1a-H.

## Data

Two rows per participant, each with `condition`, `cell`, `truth` and `sentence`.
`screen = comprehension`: `comprehension_attempts` (every answer chosen, as G/R/H/N, e.g. `G|H`),
`comprehension_first_correct` and `comprehension_order` (display order of the answers).
`screen = judgement`: `judgement` (Wrong, Partly right or Right).
