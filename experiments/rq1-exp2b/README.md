# Experiment 2b (RQ1b): cancellation

Journal Experiment 2b, as specified in the Dissertation repository at
`subprojects/sub1-xeliherb/drafts/rq1-experiments/plan/sub1_journal2026_rq1_rq2_experiment_implementation_plan_v1.md`
and preregistered in `drafts/rq1-experiments/preregistration/` there.
Participants rate twelve sentences ("Does the colonist contradict themselves?", 0–100): four critical items,
two in the association version and two in the causal version, and eight control sentences (four contradictions and four
cancellations of *some*, *and*, *a* and *or*).

## Lists and Prolific

`trials/lists.csv` assigns the versions to the items in a Latin square (List 1: A and C association, B and D
causal; List 2 the reverse). The two lists run as two Prolific studies, and each study link names its list:

```
https://heningwang.github.io/Xeliherb/experiments/rq1-exp2b/?condition=list1&PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}
```

with `condition=list1` or `list2`. The items are in `trials/critical.csv` and `trials/controls.csv`.
The order is drawn for each participant: the first two sentences are controls of different types, two critical
items are never adjacent, and the last sentence is a control.

## Before launch

In `src/magpie.config.js`: enter the magpie `experimentId`, the Prolific completion code, and set `mode: 'prolific'`.
Pushing to `main` deploys to GitHub Pages.

## Local test

`npm ci`, then `npm run serve -- --port 8098` and open http://localhost:8098/?condition=list1.

## Data

One row each for the welcome and cover screens, then twelve rows (`screen = sentence`): `list`, `design_version`, `position` (1–12), `item` (A–D or the control id, c-…),
`type` (association, causal, contradiction or cancellation), `text`, `previous_type` (type of the sentence
before it, empty at position 1), `first_critical` (true for the first
critical item the participant saw) and `rating` (0–100).
