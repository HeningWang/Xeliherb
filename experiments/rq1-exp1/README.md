# Experiment 1 (RQ1a): information type and vehicle

Journal Experiment 1, as specified in the Dissertation repository at
`subprojects/sub1-xeliherb/drafts/rq1-experiments/plan/sub1_journal2026_rq1_rq2_experiment_implementation_plan_v1.md`
and preregistered in `drafts/rq1-experiments/preregistration/` there.
A 2 x 3 between-participants design: information type (observational: wild patches; interventional: a test field
in which the colonists plant ralocrop) crossed with the vehicle (own observation: participants gather the ten patches;
data reported: a colonist sends the patch table and summary; verbal report: a colonist sends one sentence).
The verbal-report groups see no kind of patch; their sentence is "associated with" (observational) or "causes"
(interventional). After the cover story, participants answer a comprehension question and can only continue once
they answer it correctly. They then see the evidence and predict how many of 100 low-yield patches without
ralocrop will give a high yield once ralocrop is planted, and finally choose a cultivation policy.

## Conditions and Prolific

Each group runs as its own Prolific study, and the study link names the group:

```
https://heningwang.github.io/Xeliherb/experiments/rq1-exp1/?condition=obs-own&batch=1&PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}
```

with `condition=obs-own`, `obs-data`, `obs-verbal`, `int-own`, `int-data` or `int-verbal`, and `batch` the batch number.
Each batch publishes six new studies, one per group with equal places, at the same time, so the groups stay equal and
every row records its batch; exclude participants of all earlier studies in each new one. Give the six studies the same
title, description, payment and time estimate, and enter the same completion code in each.
A link without a valid condition shows a "cannot start" page.

The group texts are in `trials/conditions.csv` and the patches in `trials/patches.csv`;
the cross-tables are counted from the patches.

## Before launch

In `src/magpie.config.js`: enter the magpie `experimentId`, the Prolific completion code,
and set `mode: 'prolific'`. Pushing to `main` builds and deploys the experiment to GitHub Pages
(`.github/workflows/deploy-to-gh-pages.yml`).

## Local test

```sh
npm ci
npm run build
python3 -m http.server 8096 --directory dist
```

and open http://localhost:8096/?condition=fields. In debug mode the data appear as a table at the end.

## Data

One row per screen (`screen` = welcome, cover, critical, policy), each with `condition`, `info`, `vehicle`,
`batch`, `design_version` (v8: as v7, with a verbal reminder of the kind of patches instead of the picture on the critical screen; v7: the 2 x 3 design with the blocking comprehension check and a grey wild-patch box; v6 had the same materials with a cream wild-patch box; v5 had an attention check on the critical screen) and magpie's `responseTime` (the time spent on the screen).
Cover: `comprehension_first` (the first answer), `comprehension_errors` (wrong answers before the correct one) and
`comprehension_order` (display order). Critical: `prediction` (0-100), `patch_order` (patch numbers in `trials/patches.csv`, in the order of the patch table; not in the verbal-report
groups), and in the own-observation groups `reveal_ms` (time of each click since the screen opened) and `gather_ms`
(time from the first to the last click). Policy: `policy` and `policy_order`.
