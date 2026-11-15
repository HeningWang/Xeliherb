# Experiment 1 (RQ1a): the effect of association wording on causal belief

Journal Experiment 1, as specified in the Dissertation repository at
`subprojects/sub1-xeliherb/drafts/journal-background/sub1_journal2026_rq1_rq2_experiment_implementation_plan_v1.md`
and preregistered in `sub1_journal2026_rq1_exp1_preregistration_v1.md` next to it.
Participants learn the same patches in one of four ways (fields, note, (1a) associated, (1b) causes)
and rate belief in G, the alternatives R, H and N, the perceived strength C and the cultivation decision D.

## Conditions and Prolific

Each group runs as its own Prolific study, and the study link names the group:

```
https://heningwang.github.io/Xeliherb/experiments/rq1-exp1/?condition=fields&PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}
```

with `condition=fields`, `note`, `1a` or `1b`. Each batch adds 20 places to all four studies at the same time,
so the groups stay equal. Give the four studies the same title, description, payment and time estimate,
enter the same completion code in each, and exclude participants of the other three studies in each.
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

One row per response screen (`screen` = evidence, alternatives, strength, decision), each with `condition`
and magpie's `responseTime`. Evidence: `G`, and in the fields group `harvest_order` (patch numbers in
`trials/patches.csv`, in the order of gathering) and `reveal_ms` (time of each click since the screen opened).
Alternatives: `R`, `H`, `N` and `alternatives_order`. Strength: `C`. Decision: `D`. All ratings 0–100.
