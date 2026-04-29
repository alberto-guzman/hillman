# Claude Code instructions — Hillman Summer Program Impact Study

This file gives Claude project-specific guidance for working in this repo.
The README.md has the full methodological description; this file focuses on
conventions and gotchas that have surfaced during code review.

## Core methodological conventions (do not break these)

### `year` vs `hs_grad_year` — they are NOT interchangeable

- **`year`** = year of Hillman summer-program attendance (the **application
  year**). This is the **cohort identifier**. Used for:
  - Matching strata (`exact = ~year` in script 5)
  - Regression year FE (`factor(year)` in script 7)
  - Cohort ATT analysis (script 7)
  - Descriptive/cohort breakdowns in scripts 1, 2, 3a, 4, 8

- **`hs_grad_year`** = year student finished high school. This is the
  **outcome time anchor**. Used for:
  - NSC join keys in script 3b (Danielle's `.dta` is keyed on hs_grad_year)
  - Analytic-sample cap (`hs_grad_year >= 2018, hs_grad_year <= 2021`)
  - CHECK 4 in script 3b (NA pattern by HS grad cohort)

If you find code labeling cohort by `hs_grad_year`, **that's a bug** unless
it's specifically about outcome follow-up windows. The cohort ATT in script
7 was originally written using `hs_grad_year` and has been corrected to
use `year`.

### NSC merge uses `.dta`'s authoritative `hs_grad_year`

Script 3b joins on `(first_name, last_name)` only and **prefers Danielle's
hs_grad_year over our derived `year + 12 - grade`** when they differ
(coalesce). This is because some students were held back / skipped grades,
and Danielle's value comes from registrar records. Don't revert this to a
3-key join.

### Cohort cap (2018–2021) is outcome-driven

The analytic sample is restricted to `hs_grad_year ∈ [2018, 2021]` because:
- 2017 HS grads: only 18 students; predates program scope
- 2022 HS grads: NSC query has only ~57% seamless rate (vs ~80% for prior
  cohorts) — partial NSC coverage
- 2023+ HS grads: NSC query returned 0% enrollment data

This means the 2022 and 2023 **application-year cohorts** are also fully
excluded (they all have hs_grad_year ≥ 2022). The exclusion is in script 3b.

### All panels condition on `has_nsc_record == 1`

Per PI input: students not appearing in NSC went to college per Hillman's
own records but their institution did not report to NSC. Including them as
zeros would bias outcomes downward. Script 7 filters all panels to
`has_nsc_record == 1`; Panel C additionally filters on `enroll_ever == 1`.

### Caliper is 0.25 SD (not 0.5)

The caliper was tightened from 0.5 to 0.25 SD during code review (per
Austin 2011 / Stuart 2010 standard). Don't loosen it without a substantive
reason — PA balance materially improved at 0.25.

### G-computation via `marginaleffects::avg_comparisons()` with HC3

ATT extraction uses `avg_comparisons(fit, variables = "treated_in_year",
vcov = "HC3", newdata = subset(data, treated_in_year == 1))`. **Don't
revert to reading the LPM coefficient directly** — that breaks if anyone
adds treatment-covariate interactions later.

HC3 (not HC2 or cluster-on-subclass) is correct for matching with
replacement per Hill & Reiter (2006) and the MatchIt vignette. HC1 fallback
is preserved for leverage = 1 cases.

## Pipeline structure

`code/run_all.R` is the canonical entry point. Scripts pass objects via the
shared R session. Scripts 5–8 also save/load RDS files so they can run
standalone after a fresh matching run.

```
helpers.R (sourced by 1, 2)
  → 1 → 2 → 3a → 3b → 4 → 5 → 7 → 7_subgroup → 8
```

Scripts 1–4 expect predecessor objects in the global environment.
Scripts 5+ load from RDS files.

## Outcome variable naming gotchas

- **`deg_bach_6y` is actually a 7-year window** (Danielle's loop range is
  `0/6` inclusive of year 0). The variable is kept for historical reasons;
  display labels in script 8 read "within 7 years."
- **`deg_any_stem_6y` is a 6-year window.** Janitor cleans the source
  column name `STEMdegree_in6_grad` to `ste_mdegree_in6_grad` (awkward
  but stable).
- **`deg_bach_4y` was dropped** — identical to `deg_bach_6y` in the matched
  sample for current cohorts (no bachelor's earned in years 5–7).

## Things that have caused silent bugs in the past — don't repeat

1. **Joining on `(first_name, last_name, hs_grad_year)`** when our derived
   hs_grad differs from Danielle's. Caught only after surfacing 3 students
   silently dropped.
2. **All four grade dummies + intercept** in PS or outcome regression —
   produces rank-deficient design matrix. Drop `grade_11` (modal reference).
3. **All three `urban_miss`/`suburban_miss`/`rural_miss`** in the outcome
   regression — they're identical columns (all derived from
   `geographic_location`). Use only `urban_miss`.
4. **Treating "Do not wish to answer"/"N/A" as `disability = 1`** — was
   miscoding ~95 students. The current regex matches `^\\s*(no|none|do not
   wish|n/?a|not applicable)\\b` to NA those out.
5. **Hardcoded duplicate-row removal** based on row order — was fragile to
   source-file reorderings. Now uses a deterministic completeness sort
   (`desc(!is.na(stipend))`, then `desc(!is.na(gpa))`).

## Reproducibility

- `set.seed(20260428)` is set in script 5 before each `matchit()` call.
  Don't change unless you want to invalidate the published matched sample.
- The `marginaleffects` package is required for script 7. Install with
  `install.packages("marginaleffects")`.

## Useful commands

Run the full pipeline:

```r
source("code/run_all.R")
```

Re-run only the impact analysis (script 5 outputs already exist on disk):

```r
source("code/7_1d_impact.R")
source("code/7_1d_impact_subgroup.R")
source("code/8_1d_tables_figures.R")
```

Inspect a results object:

```r
readRDS("output/att_results_all_states.rds") |>
  dplyr::select(panel, label, ctrl_mean, trt_mean, att, se, pval)
```

## When in doubt

- Don't change `year` to `hs_grad_year` in regression FE or matching strata
- Don't add `cluster = subclass` to outcome SEs (HC3 is correct for
  with-replacement)
- Don't restore `deg_bach_4y` to the published outcome set
- Don't drop the `set.seed()` calls
- Verify with `git log` what the most recent commit's intent was before
  reverting anything
