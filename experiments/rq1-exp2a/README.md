# Experiment 2a (RQ1b): truth-value judgement

Journal Experiment 2a, as specified in the Dissertation repository at
`subprojects/sub1-xeliherb/drafts/rq1-experiments/plan/sub1_journal2026_rq1_rq2_experiment_implementation_plan_v1.md`
and preregistered in `drafts/rq1-experiments/preregistration/` there.
Participants read the colonist's sentence (1a) *associated* or (1b) *causes* of Experiment 1, learn what the
researchers found (ground truth H, a common cause, or G, ralocrop raises yield), answer a comprehension
question about the finding until they are correct, and judge the report *wrong*, *partly right* or *right*.

## Conditions and Prolific

The five cells (`trials/conditions.csv`) run as five Prolific studies, and each study link names its cell:

```
https://heningwang.github.io/Xeliherb/experiments/rq1-exp2a/?condition=1a-H&PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}
```

with `condition=1a-H` (critical), `1b-H` (false control), `1c-H` (neutral control), `1b-G` (true control) or `1a-G`
(underinformative). `1a`, `1b` and `1c` are the association, causal and neutral sentences; `H` is the common cause and
`G` the forward link as ground truth.
A link without a valid condition shows a "cannot start" page.

## Before launch

In `src/magpie.config.js`: enter the magpie `experimentId`, the Prolific completion code, and set `mode: 'prolific'`.
Pushing to `main` deploys to GitHub Pages.

## Local test

`npm ci`, then `npm run serve -- --port 8097` and open http://localhost:8097/?condition=1a-H.

## Data

Four rows per participant (`screen` = welcome, cover, comprehension, judgement), each with `condition`, `cell`,
`truth`, `sentence`, `design_version` and magpie's `responseTime`.
`screen = comprehension`: `comprehension_attempts` (every answer chosen, as G/R/H/N, e.g. `G|H`),
`comprehension_first_correct` and `comprehension_order` (display order of the answers).
`screen = judgement`: `judgement` (Wrong, Partly right or Right).
