# Load packages
library(here)
library(tidyverse)
library(cbcTools)
library(logitr)
library(janitor)
library(lubridate)
library(fastDummies)
library(kableExtra)
library(tibble)
library(scales)
library(patchwork)
library(cowplot)
library(ggalt)

# Define directories
processed_dir <- here("results")
code_dir <- here("code", "analysis")
raw_dir <- here("raw")

# Date range
start_date <- "2024-01-01"
end_date <- "2024-12-31"

# Create result directories
result_dir <- c(
  "1_summary",
  "2_models",
  "3_enrollment_sensitivity",
  "4_scenario_analysis"
)
lapply(
  file.path(processed_dir, result_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# Filter dates function
filter_dates <- function(df) {
  df %>% filter(created >= start_date & created <= end_date)
}

# Select scripts to run
selected_files <- c(
  "1_clean_data.R",
  "2_summary_statistics.R",
  # "3_1_mnl_pref_model.R", # This script only needs to run once so it's commented out.
  "3_2_mnl_enrollment_sensitivity.R",
  "3_3_mnl_scenario_analysis.R",
  # "4_1_mxl_pref_model.R", # This script only needs to run once so it's commented out.
  "4_2_mxl_enrollment_sensitivity.R",
  "4_3_mxl_scenario_analysis.R",
  "4_4_mxl_sensitivity_monthly.R",
  # "5_1_mxl_model_gender.R", # This script only needs to run once so it's commented out.
  "5_2_mxl_enrollment_sensitivity_gender.R",
  "5_3_mxl_scenario_analysis_gender.R"
)

# Source selected scripts
code <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE)
files_to_run <- code[basename(code) %in% selected_files]
lapply(files_to_run, source)
