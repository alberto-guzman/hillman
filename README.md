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
| `5_1d_matching.R` | Propensity score matching (NN, with replacement, caliper = 0.25 SD, exact-on-year) |
| `7_1d_impact.R` | ATT estimation via g-computation (LPM + `marginaleffects::avg_comparisons()`, HC3 SEs); pooled, cohort, and heterogeneity |
| `7_1d_impact_subgroup.R` | Detailed subgroup ATT estimates (stipend, race, geography, etc.) |
| `8_1d_tables_figures.R` | Publication tables (HTML) and figures (PNG) |

> Scripts use `here` package for paths. Open the `.Rproj` file to set the
> project root before running.

---

## Methodological design (current spec)

### Sample

- **Applicant cohorts (`year`):** 2017, 2018, 2019, 2021 — used for matching strata. 2020 excluded (program disrupted by COVID); 2022/2023 application cohorts have no analyzable HS-grad windows yet.
- **HS graduation cohorts (`hs_grad_year`):** 2018–2021 — drives outcome availability. 2022 cohort excluded (NSC partial coverage); 2023+ excluded (NSC has no enrollment data yet).
- **Two analytic samples:**
  - **All states** — pre-match 644 (142 T / 502 C); matched 237 (136 T / 101 C)
  - **PA public schools only** — pre-match 334 (87 T / 247 C); matched 137 (78 T / 59 C)

### Propensity-score matching (script 5)

- Nearest-neighbor with replacement
- Caliper = **0.25 pooled SDs** of the propensity score
- Exact match on `year` (application year)
- Logistic PS model with grade dummies (grade 11 reference), 14 base covariates, 1 PA-residence indicator (all-states only), 5 school-level covariates (PA only), and missingness indicators where applicable
- `set.seed(20260428)` for reproducibility

### Outcome estimation (script 7)

- Doubly-robust linear probability model with matching weights
- ATT recovered via g-computation (`marginaleffects::avg_comparisons()`, `newdata = subset(treated == 1)`, `vcov = "HC3"`)
- HC3 SEs per Hill & Reiter (2006) for matching with replacement; HC1 fallback for leverage = 1
- Year fixed effects via `factor(year)`
- All panels condition on `has_nsc_record == 1` (per PI input on non-NSC institution reporting)

### Reported outcomes (consolidated set of 8)

| Panel | Outcome | Filter |
|---|---|---|
| A | Seamless enrollment | NSC-matched |
| A | Seamless STEM enrollment | NSC-matched |
| B | Initial 4-year college entry | NSC-matched |
| B | Initial 2-year college entry | NSC-matched |
| C | Persisted to 2nd year (any institution) | NSC-matched + enrolled |
| C | Persisted in STEM (year 1) | NSC-matched + enrolled |
| C | Bachelor's degree within 7 years | NSC-matched + enrolled |
| C | Any STEM degree within 6 years | NSC-matched + enrolled |

### Additional analyses

- **Cohort ATT** — per-application-year ATT for the 4 focal STEM outcomes (`enroll_seamless`, `enroll_seamless_stem`, `pers_1y_stem`, `deg_bach_6y`). Output: `att_results_cohort.rds`. Cells with <15 treated suppressed.
- **Heterogeneity** — subgroup ATTs by `racially_marginalized`, `first_gen`, `gender`, urban/rural for the focal STEM outcomes. Output: `att_results_het.rds`.
- **Detailed subgroup analysis** — extended subgroup tables in `7_1d_impact_subgroup.R`. Output: `att_subgroup_*.rds`.

---

## Critical conventions

**`year` vs `hs_grad_year`:**
- `year` = year of Hillman attendance (application year). The cohort identifier; used for matching strata, regression FE, cohort ATT.
- `hs_grad_year` = year student finished high school. The outcome time anchor; used for NSC join keys and analytic-sample cap.
- They are derived (`hs_grad_year = year + 12 - grade` initially; overridden by NSC-authoritative value where available) but are NOT interchangeable. Each has a specific role.

**Year-window labels:**
- `deg_bach_6y` is actually a **7-year window** (Danielle's NSC loop uses `0/6` inclusive of year 0). Reported as "Bachelor's within 7 years" in tables.
- `deg_any_stem_6y` is a **6-year window** (`0/6` loop, but only year 1+ contributes degrees). Reported as "Any STEM degree within 6 years."

---

## Outputs

### Tables (`output/tables/`)

- `table_1_descriptive.html` — Table 1: descriptive statistics by treatment
- `table_2_balance_all_states.html` — Table 2: covariate balance (all states)
- `table_3_att_all_states.html` — Table 3: main ATT results (all states), 3 panels
- `table_4_att_pa.html` — Table 4: main ATT results (PA public schools), 3 panels
- `appendix_a1_balance_pa.html` — Appendix A1: balance table (PA)
- `appendix_a2_sample_sizes.html` — Appendix A2: sample sizes by cohort

### Figures (`output/figures/`)

- `figure_1_love_plot.png` — Figure 1: covariate balance Love plot (all states)
- `figure_2_subgroup_effects.png` — Figure 2: subgroup ATT coefficient plot

### Result objects (`output/`)

- `att_results_all_states.rds`, `att_results_pa.rds` — pooled ATT, by panel
- `att_results_cohort.rds` — per-cohort ATT (focal outcomes)
- `att_results_het.rds` — heterogeneity ATT
- `att_subgroup_all_states.rds`, `att_subgroup_pa.rds` — detailed subgroup tables

---

## Sample and design summary

| | All states | PA public schools |
|---|---:|---:|
| Pre-match (analytic pool) | 644 (142 T / 502 C) | 334 (87 T / 247 C) |
| Matched | 237 (136 T / 101 C) | 137 (78 T / 59 C) |
| NSC-matched (Panels A/B regression sample) | 229 | 133 |
| Enrolled (Panel C regression sample) | 196 | 116 |

Identification rests on conditional ignorability given the matched covariate
set, with doubly-robust adjustment via the outcome regression. The 2017 HS
grad cohort is the only cohort with a full 7-year follow-up window for
bachelor's-degree attainment.
