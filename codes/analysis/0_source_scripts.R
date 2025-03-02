# Load Packages
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

# Create directories
result_dir<- c("1_summary",
               "2_models",
               "3_enrollment_sensitivity",
               "4_scenario_analysis")

lapply(file.path(processed_dir, result_dir), dir.create,
       recursive = TRUE, showWarnings = FALSE)

# Pass directories as arguments
code_dir <- here("codes", "analysis")
raw_dir <- here("survey")

# Define a function to filter dates
filter_dates <- function(df) {
    df %>%
        filter(created >= start_date & created <= end_date)
}

# Source code scripts
codes <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE)
selected_files <- c(
    "1_clean_data.R",
    "2_summary_statistics_plots.R",
    "3_mnl_pref_model.R",
    "4_mnl_enrollment_sensitivity.R",
    "5_mnl_scenario_analysis.R"
)
files_to_run <- codes[basename(codes) %in% selected_files]
lapply(files_to_run, source)