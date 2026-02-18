# =============================================================================
# Master Script: Execute All Hillman Analysis Scripts
# =============================================================================
# This script sources all 7 data processing and analysis scripts in sequence
# with error handling to catch and report any failures.

# Define the scripts in order
scripts <- c(
  "code/1_1d_applicant_clean.R",
  "code/2_1d_alumni_clean.R",
  "code/3a_1d_master_merge.R",
  "code/3b_1d_merge_clean.R",
  "code/4_1d_merge_school_info.R",
  "code/5_1d_matching.R",
  "code/7_1d_impact.R"
)

# Function to safely source a script with error handling
safe_source <- function(script_path) {
  message("\n========================================")
  message("Running: ", script_path)
  message("========================================\n")

  tryCatch(
    {
      source(script_path)
      message("\n✓ Successfully completed: ", script_path)
      return(TRUE)
    },
    error = function(e) {
      message("\n✗ ERROR in script: ", script_path)
      message("Error message: ", conditionMessage(e))
      message("\nExecution stopped.")
      stop(paste0("Script failed: ", script_path, "\nError: ", conditionMessage(e)))
    }
  )
}

# Execute all scripts
message("==============================================")
message("Starting Hillman Analysis Pipeline")
message("==============================================")
message("Total scripts to run: ", length(scripts))
message("Start time: ", Sys.time())

for (script in scripts) {
  safe_source(script)
}

message("\n==============================================")
message("All scripts completed successfully!")
message("End time: ", Sys.time())
message("==============================================")
