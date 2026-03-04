# =============================================================================
# run_all.R
#
# Purpose: Master script — runs the full Hillman impact analysis pipeline
#          in sequence. Sources scripts 1–7 in order; stops with an informative
#          error message if any step fails.
#
# Pipeline overview:
#   1_1d_applicant_clean.R   — clean raw applicant data → `applicants`
#   2_1d_alumni_clean.R      — clean alumni tracker, build treatment indicators
#                              → `alum`
#   3a_1d_master_merge.R     — join applicants + alumni, deduplicate to one
#                              row per student → `merged_df`
#   3b_1d_merge_clean.R      — standardize covariates, merge NSC outcomes,
#                              restrict to cohorts 2018–2022 → `merged_clean`
#   4_1d_merge_school_info.R — normalize school names, add AUN codes and
#                              school-level covariates → `merged_df_all`,
#                              `merged_df_pa`; saves two CSVs to data/
#   5_1d_matching.R          — propensity score matching (NN, caliper = 0.5,
#                              exact on year) → matched RDS + balance table
#   7_1d_impact.R            — ATT estimates via weighted LPMs + GPA
#                              heterogeneity analysis → results RDS
#
# Outputs (written to disk):
#   data/merged_all_states.csv
#   data/merged_df_pa_public.csv
#   data/matched_all_states_year_only.rds
#   output/balance_table_all_states.html / .tex
#   output/att_results_all_states_year_only.rds
#
# Usage: source("code/run_all.R") from the project root, or open and run
#        interactively. Requires the here package; set project root with
#        here::i_am() or an .Rproj file.
# =============================================================================

# Define script paths in execution order
scripts <- c(
  "code/1_1d_applicant_clean.R",
  "code/2_1d_alumni_clean.R",
  "code/3a_1d_master_merge.R",
  "code/3b_1d_merge_clean.R",
  "code/4_1d_merge_school_info.R",
  "code/5_1d_matching.R",
  "code/7_1d_impact.R"
)

# Execute each script with error handling
message(
  "============================================================================="
)
message("Starting Hillman Analysis Pipeline")
message(
  "=============================================================================\n"
)

for (script in scripts) {
  message("--- Running: ", script, " ---")

  tryCatch(
    {
      source(script)
      message("✓ Completed: ", script, "\n")
    },
    error = function(e) {
      message("\n✗ ERROR in script: ", script)
      message("Error message: ", conditionMessage(e))
      message("\nExecution stopped. Please fix the error and try again.")
      stop(paste0("Script failed: ", script), call. = FALSE)
    }
  )
}

message(
  "============================================================================="
)
message("All scripts completed successfully!")
message(
  "============================================================================="
)
