# =============================================================================
# run_all.R — Full pipeline driver for the Hillman Summer STEM Program impact
# study. Sources scripts in order; downstream scripts inherit upstream objects
# from the calling environment.
#
# Pipeline:
#   1_1d_applicant_clean.R      — clean raw applicant files (2017–2023)
#   2_1d_alumni_clean.R         — clean alumni tracker, build treatment indicators
#   3a_1d_master_merge.R        — merge applicants + alumni; drop 2020 (COVID)
#                                 → `merged_df`
#   3b_1d_merge_clean.R         — standardize covariates; merge NSC outcomes
#                                 → `merged_clean`
#   4_1d_merge_school_info.R    — link PA AUN codes; merge school-level covariates
#                                 → `merged_df_all`, `merged_df_pa`
#                                 → data/processed/merged_all_states.csv
#                                 → data/processed/merged_df_pa_public.csv
#   5_1d_matching.R             — PSM (nearest-neighbor, caliper 0.5 SD, exact on year)
#                                 → data/matched/matched_{all_states,pa}_year_only.rds
#                                 → data/matched/matchit_object_{all_states,pa}.rds
#                                 → data/matched/matching_data_{all_states,pa}.rds
#   7_1d_impact.R               — doubly-robust ATT via weighted LPMs (HC2 SEs)
#                                 → output/att_results_{all_states,pa}.rds
#                                 → output/att_results_het.rds
#   7_1d_impact_subgroup.R      — subgroup ATT estimates (suppressed if <20 treated)
#                                 → output/att_subgroup_{all_states,pa}.rds
#   8_1d_tables_figures.R       — all publication tables and figures (HTML/PNG)
#                                 → output/tables/table_{1,2,3,4}_*.html
#                                 → output/tables/appendix_a{1,2}_*.html
#                                 → output/figures/figure_{1,2}_*.png
#
# Notes:
#   - Requires the `here` package; project root is set by the .Rproj file.
#   - Year 2020 excluded (COVID); 2022 excluded from matching (NSC follow-up lag);
#     2023 excluded from PA matching (only 1 treated).
#   - Non-NSC-matched students coded as 0 for enrollment outcomes in the main
#     spec; Panel B restricts to NSC-matched students as a robustness check.
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
  "7_1d_impact_subgroup.R",
  "8_1d_tables_figures.R"
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

# To do: dosage effect
#
