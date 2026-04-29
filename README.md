# Hillman Summer Program Impact Study

Causal evaluation of the Hillman Summer STEM Program on college enrollment,
persistence, and degree attainment. The analysis uses propensity score matching
(PSM) with doubly-robust outcome regression to estimate the Average Treatment
Effect on the Treated (ATT) by comparing program participants to similar
non-participants drawn from the applicant pool.

---

## Directory Structure

```
2020_hillman/
├── code/                            # R analysis scripts (run via run_all.R)
├── data/
│   ├── raw/                         # Source files (do not edit)
│   ├── processed/                   # Intermediate datasets (scripts 1–4)
│   ├── matched/                     # Matched datasets and MatchIt objects (script 5)
│   └── files_for_danielle_nsc/      # NSC exchange files
├── output/
│   ├── tables/                      # Publication-ready HTML tables
│   ├── figures/                     # Publication figures (PNG)
│   └── counts/                      # Sample-size CSVs at each pipeline stage
└── docs/                            # Stakeholder documents
```

---

## Pipeline overview

Run the full analysis from R / RStudio:

```r
source("code/run_all.R")
```

Scripts execute in order:

| Script | Purpose |
|---|---|
| `helpers.R` | Shared utilities (e.g., `clean_person_name()`) |
| `1_1d_applicant_clean.R` | Load and clean year-specific applicant files (2017–2023) |
| `2_1d_alumni_clean.R` | Load and clean alumni tracker; create per-year treatment indicators |
| `3a_1d_master_merge.R` | Merge applicants to alumni; build per-student treatment timeline; drop 2020 (COVID) |
| `3b_1d_merge_clean.R` | Standardize covariates; merge NSC outcome data; restrict to HS-grad cohorts 2018–2021 |
| `4_1d_merge_school_info.R` | Normalize school names; merge PA school-level covariates |
| `5_1d_matching.R` | 1:3 propensity score matching (NN, with replacement, caliper = 0.25 SD, exact-on-year) |
| `7_1d_impact.R` | ATT estimation via g-computation (LPM + `marginaleffects::avg_comparisons()`, HC3 SEs); pooled and heterogeneity |

> Scripts use `here` package for paths. Open the `.Rproj` file to set the
> project root before running.

---

## Methodological design (current spec)

### Sample

- **Applicant cohorts (`year`):** 2017, 2018, 2019 — used for matching strata. 2020 excluded (program disrupted by COVID); 2021 excluded from matching (only 1 analytic-sample treated student all-states, 0 in PA); 2022/2023 application cohorts have no analyzable HS-grad windows yet.
- **HS graduation cohorts (`hs_grad_year`):** 2018–2021 — drives outcome availability. 2022 cohort excluded (NSC partial coverage); 2023+ excluded (NSC has no enrollment data yet).
- **Two analytic samples:**
  - **PA public schools (primary)** — pre-match 327 (83 T / 244 C, NSC-trackable); matched 197 (72 T / 125 unique C). The Hillman program is based in Pittsburgh and recruits primarily from western Pennsylvania; the PA sample is its natural target population and avoids cross-state geographic confounding.
  - **All states (robustness)** — pre-match 602 (136 T / 466 C); matched 314 (121 T / 193 unique C). Adds out-of-state applicants, but matched composition remains ~80% PA. Reported as a generalizability check.
  - Pre-match samples are restricted to students with `has_nsc_record == 1` so matching weights stay coherent through to outcome estimation (no broken 1:k pairs from post-match outcome filtering).

### Propensity-score matching (script 5)

- 1:3 nearest-neighbor with replacement
- Caliper = **0.25 pooled SDs** of the propensity score
- **Exact match on `year` for the PA sample; `(year, pa_state)` for the all-states sample.** The all-states match enforces within-state-of-residence comparisons so PA-treated are not paired with non-PA controls (and vice versa) on PS distance alone.
- Logistic PS model with grade dummies (grade 11 reference), 13 base covariates, 1 PA-residence indicator (all-states only), 5 school-level covariates (PA only), and missingness indicators where applicable. `first_gen` excluded due to structural missingness in 2017–2018 cohorts.
- `set.seed(20260428)` for reproducibility

### Outcome estimation (script 7)

- Doubly-robust linear probability model with matching weights
- ATT recovered via g-computation (`marginaleffects::avg_comparisons()`, `newdata = subset(treated == 1)`, `vcov = "HC3"`)
- HC3 SEs per Hill & Reiter (2006) for matching with replacement; no fallback (cells with degenerate vcov are surfaced as warnings rather than silently demoted)
- Year fixed effects via `factor(year)`
- All panels condition on `has_nsc_record == 1` (per PI input on non-NSC institution reporting)

### Reported outcomes (set of 6)

| Panel | Outcome | Filter |
|---|---|---|
| A | Seamless enrollment | NSC-matched |
| A | Seamless STEM enrollment | NSC-matched |
| B | Initial 4-year college entry | NSC-matched |
| B | Initial 2-year college entry | NSC-matched |
| C | Persisted to 2nd year (any institution) | NSC-matched + enrolled |
| C | Persisted in STEM (year 1) | NSC-matched + enrolled |

Year-window degree outcomes (`deg_bach_6y` / 7-year window, `deg_any_stem_6y` / 6-year window, `deg_any_6y` / 6-year window) are excluded from the headline because the current NSC pull (data through 2023-06-30) does not provide full follow-up for any 2018–2021 cohort. Script 3b sets these to NA in the analytic sample so they cannot be silently treated as "did not attain"; they will move into the headline once a future NSC refresh provides full follow-up.

### Additional analyses

- **Heterogeneity** — subgroup ATTs by `racially_marginalized`, `gender`, urban/rural for the focal STEM outcomes (`enroll_seamless_stem`, `pers_1y_stem`). Output: `att_results_het.rds`. Subgroups suppressed when fewer than 15 treated.

---

## Critical conventions

**`year` vs `hs_grad_year`:**
- `year` = year of Hillman attendance (application year). The cohort identifier; used for matching strata, regression FE, cohort ATT.
- `hs_grad_year` = year student finished high school. The outcome time anchor; used for NSC join keys and analytic-sample cap.
- They are derived (`hs_grad_year = year + 12 - grade` initially; overridden by NSC-authoritative value where available) but are NOT interchangeable. Each has a specific role.

**Year-window labels (for descriptive use only — not in current headline):**
- `deg_bach_6y` is actually a **7-year window** (Danielle's NSC loop uses `0/6` inclusive of year 0).
- `deg_any_stem_6y` is a **6-year window** (`0/6` loop, but only year 1+ contributes degrees).
- These are set to NA in script 3b for any cohort where the window has not elapsed at the NSC pull date (`NSC_DATA_THROUGH_YEAR = 2023`). Update the constant when the .dta is refreshed.

---

## Outputs

### Result objects (`output/`)

- `att_results_all_states.rds`, `att_results_pa.rds` — pooled ATT, by panel (HC3 SEs)
- `att_results_het.rds` — heterogeneity ATT (subgroup × focal STEM outcomes)

### Counts (`output/counts/`)

- `n_applicants_by_year.csv`, `n_alumni_by_year.csv`, `n_merged_by_year.csv`,
  `n_merged_clean_by_year.csv`, `n_merged_all_by_year.csv`, `n_merged_pa_by_year.csv`
  — sample-size attrition at each pipeline stage

---

## Sample and design summary

| | PA public schools (primary) | All states (robustness) |
|---|---:|---:|
| Cleaned analytic sample (hs_grad 2018–2021) | 388 (98 T / 290 C) | 722 (159 T / 563 C) |
| Pre-match (post-NSC + script-5 exclusions) | 327 (83 T / 244 C) | 602 (136 T / 466 C) |
| Matched (Panels A/B regression sample) | 197 (72 T / 125 unique C) | 314 (121 T / 193 unique C) |
| Enrolled (Panel C regression sample) | 177 | 275 |

Identification rests on conditional ignorability given the matched covariate
set, with doubly-robust adjustment via the outcome regression. The 2017 HS
grad cohort is the only cohort with a full 7-year follow-up window for
bachelor's-degree attainment.
