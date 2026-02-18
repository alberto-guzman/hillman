# =============================================================================
# Master Script: Run All Hillman Analysis Scripts in Sequence
# =============================================================================
# This script sources all 7 R analysis scripts (1_ through 7_) in sequential
# order with error handling. If a script fails, execution stops and reports
# which script encountered an error.
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
message("=============================================================================")
message("Starting Hillman Analysis Pipeline")
message("=============================================================================\n")

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

message("=============================================================================")
message("All scripts completed successfully!")
message("=============================================================================")
