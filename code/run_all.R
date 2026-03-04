# =============================================================================
# run_all.R
#
# Purpose: Master script — runs the full Hillman impact analysis pipeline
#          in sequence. Source this file from the project root to reproduce
#          all results end-to-end.
#
# Pipeline:
#   1_1d_applicant_clean.R     — clean raw applicant data
#                                → `applicants`
#
#   2_1d_alumni_clean.R        — clean alumni tracker; build per-year treatment
#                                indicators and ever-treated flags
#                                → `alum`
#
#   3a_1d_master_merge.R       — join applicants + alumni; deduplicate to one
#                                row per student (first treatment year for
#                                treated, last application year for controls);
#                                drop 2020 (COVID)
#                                → `merged_df`
#
#   3b_1d_merge_clean.R        — standardize covariates; merge NSC college
#                                outcome data; restrict to graduation cohorts
#                                2018–2022
#                                → `merged_clean`
#
#   4_1d_merge_school_info.R   — normalize school names; link to PA AUN codes
#                                via crosswalk; merge school-level covariates
#                                (PA public schools only)
#                                → `merged_df_all`, `merged_df_pa`
#                                → data/merged_all_states.csv
#                                → data/merged_df_pa_public.csv
#
#   5_1d_matching.R            — propensity score matching for both samples:
#                                nearest-neighbor, with replacement,
#                                caliper = 0.5 SD, exact match on year.
#                                PA model adds school-level covariates.
#                                → `matched_data_all`, `matched_data_pa`
#                                → data/matched_all_states_year_only.rds
#                                → data/matched_pa_year_only.rds
#                                → output/balance_table_all_states.html/.tex
#                                → output/balance_table_pa.html/.tex
#                                → output/sample_table_all_states.html/.tex
#                                → output/sample_table_pa.html/.tex
#
#   7_1d_impact.R              — ATT estimates via doubly-robust weighted LPMs
#                                with cohort FEs and cluster-robust SEs.
#                                Three outcome groups: enrollment (full),
#                                enrollment (NSC-matched), degree/persistence.
#                                GPA heterogeneity analysis for both samples.
#                                → `results_all`, `results_pa`
#                                → output/att_results_all_states_year_only.rds
#                                → output/att_results_pa_year_only.rds
#                                → output/att_table_all_states.html/.tex
#                                → output/att_table_pa.html/.tex
#                                → output/desc_by_year_all_states.html/.tex
#                                → output/desc_by_year_pa.html/.tex
#
#   7_1d_impact_subgroup.R     — subgroup ATT estimates by stipend,
#                                racially marginalized, urban, suburban, rural,
#                                disability, neg. school environment, first-gen.
#                                Subgroups with < 20 treated are suppressed.
#                                Sources 7_1d_impact.R internally to inherit
#                                shared objects (fit_att, matched data, etc.).
#                                → `results_subgroup_all`, `results_subgroup_pa`
#                                → output/att_subgroup_all_states.rds
#                                → output/att_subgroup_pa.rds
#                                → output/att_subgroup_table_all_states.html/.tex
#                                → output/att_subgroup_table_pa.html/.tex
#
# Notes:
#   - Requires the `here` package; project root is set by the .Rproj file.
#   - Year 2020 excluded (COVID disruption); year 2022 excluded (insufficient
#     follow-up for most outcomes).
#   - Non-NSC-matched students coded as 0 for enrollment outcomes in the main
#     spec; Panel B of impact tables restricts to NSC-matched students as a
#     robustness check.
#
# Usage:
#   source("code/run_all.R")
# =============================================================================

library(here)

scripts <- c(
  "1_1d_applicant_clean.R",
  "2_1d_alumni_clean.R",
  "3a_1d_master_merge.R",
  "3b_1d_merge_clean.R",
  "4_1d_merge_school_info.R",
  "5_1d_matching.R",
  "7_1d_impact.R",
  "7_1d_impact_subgroup.R"
)

for (script in scripts) {
  message("\n", strrep("=", 70))
  message("Running: ", script)
  message(strrep("=", 70), "\n")
  tryCatch(
    source(here("code", script), echo = FALSE),
    error = function(e) {
      stop(
        "\nPipeline failed in: ",
        script,
        "\nError: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

message("\n", strrep("=", 70))
message("Pipeline complete. All outputs saved to data/ and output/.")
message(strrep("=", 70))
