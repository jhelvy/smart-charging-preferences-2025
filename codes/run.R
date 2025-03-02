# Run scripts in this file to obtain a full-course of survey analysis results.
# The processed files are saved in the "results" folder.

# Define directory and dates
library(here)

# Processed directory
processed_dir <- here("results")

# The start and end dates were used to facet different batches of survey
# recruitment, but they were finally combined as a pooled data. We chose
# to define them to span over 2024, which embraces our whole survey time
# span (from March to November 2024).
start_date <- "2024-01-01"
end_date <- "2024-12-31"

# The 0_source_scripts.R file performs basic settings and runs all code scripts
# under the "analysis" folder (excluding itself).
source(here("codes", "analysis", "0_source_scripts.R"))
