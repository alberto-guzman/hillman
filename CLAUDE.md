# Claude Code instructions — Hillman Summer Program Impact Study

This file gives Claude project-specific guidance for working in this repo.
The README.md has the full methodological description; this file focuses on
conventions and gotchas that have surfaced during code review.

## Active framing (as of 2026-04-29)

**PA public schools is the PRIMARY analytic sample.** Hillman is a Pittsburgh-
based program; the matched all-states sample is ~80% PA-resident anyway, so
PA-primary is the natural target population and avoids cross-state geographic
confounding. The all-states sample is reported as a robustness/generalizability
check.

Strongest defensible result: PA Persisted-in-STEM-Y2 ATT = +16.0 pp (HC3 SE
8.4, p = .058). All other STEM-related point estimates are positive but do
not reach conventional significance.

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
it's specifically about outcome follow-up windows or right-censoring (e.g.,
script 3b's `NSC_DATA_THROUGH_YEAR` check on year-window degree outcomes).

### NSC merge uses `.dta`'s authoritative `hs_grad_year`

Script 3b joins on `(first_name, last_name)` only and **prefers Danielle's
hs_grad_year over our derived `year + 12 - grade`** when they differ
(coalesce). This is because some students were held back / skipped grades,
and Danielle's value comes from registrar records. Don't revert this to a
3-key join.

### Cohort cap (2018–2021) is outcome-driven

The analytic sample is restricted to `hs_grad_year ∈ [2018, 2021]` because:
- 2017 HS grads: n=18 students; seamless-STEM rate is 22% vs 41–52% for
  2018–2021 (likely small-N noise or older-vintage NSC measurement
  degradation; excluded out of conservatism on a headline outcome)
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

### 1:3 nearest-neighbor matching with replacement

Match ratio is 1:3 (each treated paired with up to 3 controls). The pre-
match control:treated ratios (~3.5:1 all-states; ~3:1 PA) comfortably support
this. Each kth match must still pass the caliper, so degenerate candidates
are still rejected. Don't drop to 1:1 unless balance breaks.

### Pre-match NSC filter

`prepare_matching_data` in script 5 filters to `has_nsc_record == 1` before
matching. This keeps matching weights coherent through to outcome estimation
(no broken 1:k pairs from post-match outcome filtering). The estimand is
explicit: ATT among Hillman applicants whose post-secondary trajectory is
observable in NSC. Don't move this filter back to the outcome stage.

### Exact-match specifications differ by sample

- **PA sample:** exact on `year`. PA students are all in PA by construction,
  so adding `pa_state` would be a constant.
- **All-states sample:** exact on `(year, pa_state)`. Forces within-state-of-
  residence comparisons. PA-treated cannot be paired with non-PA controls
  (and vice versa) on PS distance alone — geographic confounding is too
  large for the PS to fully adjust for.

Don't unify the exact specs — the asymmetry is intentional.

### G-computation via `marginaleffects::avg_comparisons()` with HC3

ATT extraction uses `avg_comparisons(fit, variables = "treated_in_year",
vcov = "HC3", newdata = subset(data, treated_in_year == 1))`. **Don't
revert to reading the LPM coefficient directly** — that breaks if anyone
adds treatment-covariate interactions later.

HC3 (not HC2 or cluster-on-subclass) is correct for matching with
replacement per Hill & Reiter (2006) and the MatchIt vignette. There is
**no HC1 fallback** — if HC3 fails (e.g., leverage = 1 in a tiny cell
or rank-deficient model), `marginaleffects` will silently substitute
classical OLS SEs and emit a warning. Two known causes of this
fallback have been resolved upstream: `first_gen` (collinear with year
FE) was removed from the covariate set, and the n=2 2021 cell in
all-states was excluded from matching. For heterogeneity subgroups,
the warnings persist for tiny cells and should be flagged in the
writeup as right-censored / underpowered subgroup estimates.

## Pipeline structure

`code/run_all.R` is the canonical entry point. Scripts pass objects via the
shared R session. Scripts 5, 7, and 8 also save/load RDS files so they can
run standalone after a fresh matching/impact run.

```
helpers.R (sourced by 1, 2)
  → 1 → 2 → 3a → 3b → 4 → 5 → 7 → 8
```

Scripts 1–4 expect predecessor objects in the global environment.
Scripts 5+ load from RDS files. The previous `7_1d_impact_subgroup.R`
and `8_1d_tables_figures.R` were deleted (commit 61f0637); subgroup
ATTs are now produced inline by script 7's heterogeneity section.

`8_1d_tables.R` (added 2026-04-29) generates three booktabs-style LaTeX
tables (descriptives, balance, impact) via `kableExtra`, saving standalone
`.tex` files to `output/tables/`. Each file wraps a tabular in a
`\begin{table}...\end{table}` environment with caption and label and is
intended to be `\input{}` from a manuscript file. No source notes are
emitted (notes belong in the manuscript prose, not the table). Required
preamble in the host document: `\usepackage{booktabs}` (kableExtra also
recommends `makecell` and `multirow` for richer table features).

## Outcome variable naming gotchas

- **`deg_bach_6y` is actually a 7-year window** (Danielle's loop range is
  `0/6` inclusive of year 0). Year-window degree outcomes are right-censored
  in the current NSC pull (Feb 2023, data through 2023-06-30): no 2018–2021
  cohort has 6 or 7 years of follow-up. Script 3b uses
  `NSC_DATA_THROUGH_YEAR = 2023` to set these to NA when the window has not
  elapsed; the analytic sample is therefore all-NA on `deg_bach_6y`,
  `deg_any_stem_6y`, and `deg_any_6y`. They are dropped from script 7's
  Panel C and will move back in once a future NSC refresh provides
  sufficient follow-up.
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
6. **Including `first_gen` as a covariate.** The question wasn't on the
   2017 or 2018 application forms, so `first_gen` is 100% missing for
   those cohorts. The missing-indicator method then produces
   `first_gen_miss == 1[year %in% c(2017, 2018)]` — a perfect linear
   combination of the year FE / intercept. Aliasing in the PS logit and
   the outcome LPM (contributes to HC3 SE failure on aliased cells).
   Dropped from the covariate set entirely. Only 1 of 237 matched students
   is coded first-generation, so within-sample variation is also negligible.
7. **Silent OLS-classical SE fallback in `marginaleffects::avg_comparisons`.**
   When the requested `vcov = "HC3"` cannot be computed (rank-deficient
   model matrix or leverage = 1), it silently substitutes `vcov(fit)`
   (classical OLS) and emits a warning. Earlier versions had p<.05 STEM
   findings that were partly artifacts of this fallback — once `first_gen`
   collinearity and the n=2 2021 cell were removed, the SEs are honest HC3
   throughout the pooled fits. Heterogeneity subgroups can still trigger
   the fallback in tiny cells; treat those subgroup p-values cautiously.
8. **NA → 0 imputation for right-censored degree outcomes.** Year-window
   degree variables (`deg_bach_6y` etc.) had NA → 0 applied uniformly to
   all enrollees, treating "window not yet elapsed" the same as "did not
   attain." Script 3b now gates the imputation on whether
   `hs_grad_year + window <= NSC_DATA_THROUGH_YEAR`. With the current
   Feb 2023 NSC pull, no 2018–2021 cohort has the full 6/7-year window,
   so these outcomes are dropped from the headline panel.
9. **Post-match outcome-stage NSC filtering breaking matching weights.**
   Originally, matching ran on the full analytic sample and outcome
   estimation filtered to `has_nsc_record == 1`, breaking 1:k matched
   pairs whose units got dropped at the outcome stage. The filter is now
   applied pre-match in `prepare_matching_data`. Don't move it back.
10. **Cross-state matching in the all-states sample.** The all-states PSM
    used to allow PA-treated to be paired with non-PA controls on PS
    distance alone, which understates geographic-confounding adjustment.
    The all-states match now uses `exact = ~ year + pa_state` to enforce
    within-state comparisons. PA sample is unchanged (pa_state is constant
    in PA). Don't unify the exact specs across samples.

## Tables (script 8) — kableExtra LaTeX

The three tables are produced by `kableExtra::kbl()` with `format = "latex"`
and `booktabs = TRUE`. Each is saved as a standalone `.tex` file containing
`\begin{table}...\end{table}` with caption and label.

Conventions:
- Booktabs rules (`\toprule`, `\midrule`, `\bottomrule`); no vertical rules
- Italicized row-group section headings via `pack_rows(italic = TRUE)`
- Multi-column spanners via `add_header_above()`
- "Comparison" not "Control"; italicized `$n$` per APA convention
- Three-tier significance stars + dagger:
  `$^{\dagger}$` p < .10, `$^{*}$` p < .05, `$^{**}$` p < .01, `$^{***}$` p < .001
- No source notes inside the table — notes belong in manuscript prose

Content notes:
- Table 2 uses `cobalt::bal.tab()` defaults for `binary` (raw proportion
  difference) and `s.d.denom` so SMD values match what `5_1d_matching.R`
  prints to the run log. Variance ratios are reported only for continuous
  covariates (Austin, 2009 — values in [0.5, 2.0] are balanced); binary-
  covariate variance ratios are nonsense and shown as "--".
- Table 3 reports ATT in percentage points with 95% CIs derived from
  `marginaleffects::avg_comparisons()` `conf.low`/`conf.high`. The
  Comparison-mean column is the matched-sample percentage so a +16pp ATT
  reads against a counterfactual baseline.
- Continuous covariates carry an upstream zero-imputation paired with
  `_miss` indicators. Table 1 masks those rows to NA before computing
  descriptive *M* (*SD*) so reported moments reflect the observed
  distribution, not the imputation. PSAT Math is the most affected
  (≈20% imputed in PA).
- Don't re-add a raw `p` column to Table 3 — the stars convey the
  threshold; raw p-values for ns rows are clutter.

## Reproducibility

- `set.seed(20260428)` is set in script 5 before each `matchit()` call.
  Don't change unless you want to invalidate the published matched sample.
- The `marginaleffects` package is required for script 7. Install with
  `install.packages("marginaleffects")`.
- Script 8 requires `kableExtra`, `knitr`, and `cobalt`. No browser or
  rendering engine needed — output is plain LaTeX text.

## Useful commands

Run the full pipeline:

```r
source("code/run_all.R")
```

Re-run only the impact analysis (script 5 outputs already exist on disk):

```r
source("code/7_1d_impact.R")
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
- Don't restore `deg_bach_4y` or any year-window degree outcome to the
  headline (right-censored under current NSC pull)
- Don't drop the `set.seed()` calls
- Don't restore HC1 fallback or loosen the caliper from 0.25 SD
- Don't add `first_gen` back to the covariate set
- Don't move the `has_nsc_record == 1` filter out of `prepare_matching_data`
- Don't unify the exact-match specs (PA = year only; all-states = year + pa_state)
- Don't promote all-states back to primary — PA-public is the headline
- Don't add source notes back to the kableExtra tables — they belong
  in manuscript prose, not the table image (see "Tables" section)
- Verify with `git log` what the most recent commit's intent was before
  reverting anything
