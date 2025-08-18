# Load packages
library(readxl)
library(dplyr)
library(stringr)
library(purrr)


# List only real .xlsx files; drop temp/lock files (~$*) and 20172018
files <- list.files(
  here("data/pa_stats"),
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)
files <- files[!grepl("~\\$", basename(files))] # drop Excel temp files
files <- files[!grepl("20172018", basename(files))] # exclude that year
files <- files[file.info(files)$size > 0] # drop zero-byte files

# Helper: read the 3rd sheet safely
read_one <- function(path) {
  # ensure the 3rd sheet exists
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) < 3) {
    warning("Skipping (less than 3 sheets): ", basename(path))
    return(tibble())
  }

  df <- readxl::read_excel(path, sheet = 3)

  # find school name column (in your headers it's "Name")
  name_col <- names(df)[
    tolower(names(df)) %in% c("name", "school", "school_name", "schl")
  ]
  if (length(name_col) == 0) {
    name_col <- names(df)[str_detect(tolower(names(df)), "name")]
  }
  if (length(name_col) == 0) {
    warning("Skipping (no school name column): ", basename(path))
    return(tibble())
  }
  name_col <- name_col[1]

  # find the college-enrollment (two-year lag) column
  target_regex <- regex(
    "percent.*graduates.*two\\s*years.*enrolled.*institution.*higher.*education.*all\\s*student",
    ignore_case = TRUE
  )
  match_cols <- names(df)[str_detect(names(df), target_regex)]
  if (length(match_cols) == 0) {
    warning("Skipping (no Higher Ed enrollment column): ", basename(path))
    return(tibble())
  }
  enroll_col <- match_cols[1]

  out <- df %>%
    select(
      school_name = all_of(name_col),
      college_enrollment_pct = all_of(enroll_col)
    ) %>%
    mutate(
      college_enrollment_pct = as.character(college_enrollment_pct),
      college_enrollment_pct = str_replace(college_enrollment_pct, "%", ""),
      college_enrollment_pct = as.numeric(college_enrollment_pct),
      college_enrollment_pct = ifelse(
        college_enrollment_pct <= 1,
        college_enrollment_pct * 100,
        college_enrollment_pct
      ),
      source_file = basename(path)
    )

  out
}

# Read all files, safely; drop empties
simple_df <- map(files, ~ try(read_one(.x), silent = TRUE)) %>%
  keep(~ inherits(.x, "data.frame") && nrow(.x) > 0) %>%
  bind_rows()

# Peek
print(simple_df, n = 10)

# Optional: save
# write.csv(simple_df, file.path(data_dir, "pa_simple_postsecondary_enrollment.csv"), row.names = FALSE)
