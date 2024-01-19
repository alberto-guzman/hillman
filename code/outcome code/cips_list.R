# Load necessary libraries
library(readr)       # For reading CSV files
library(stringr)     # For string manipulation
library(tidyverse)   # For data manipulation and visualization

# Read in the DHS STEM OPT extension eligible CIP codes from a text file
# Source: https://studyinthestates.dhs.gov/stem-opt-hub/additional-resources/eligible-cip-codes-for-the-stem-opt-extension
df <- read_csv(here("data", "cips/stemList2022.txt"))

# Filter the dataset to keep only CIP codes in the format '00.0000'
df <- df[grep("\\.[0-9]{4}", df$cip_code),]

# Convert the 'cip_code' column to a vector
DHSSTEMCIPS <- as.vector(df['cip_code'])

# Remove decimals from the CIP codes
DHSSTEMCIPS <- str_replace_all(string = DHSSTEMCIPS$cip_code, pattern = '\\.', '')

# Add additional CIP codes based on NSF definition and considerations for healthcare and technicians
cip_code <- c(DHSSTEMCIPS, 110000:119999, 140000:159999, 260000:279999, 400000:429999, 450000:459999, 510000:519999, 999999)

# Convert the list of CIP codes into a data frame and remove duplicates
cips <- as.data.frame(cip_code) 
cips <- cips %>% distinct()

# Label the group as 'STEM'
cips$group <- 'STEM'

# Read manually curated CIP codes from a CSV file
curated_cips <- read_csv(here("data", "cips/ManualCurationMajors.csv"))

# Rename columns in the curated_cips dataframe
curated_cips <- rename(curated_cips, group = Majors, cip_code = ManualCIP)

# Save the cips dataframe as a CSV file
write_csv(cips, "cip_codes_stem_final.csv")
write_csv(curated_cips, "cip_codes_curated_final.csv")








