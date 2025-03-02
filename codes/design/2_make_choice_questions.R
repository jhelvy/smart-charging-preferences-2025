library(tidyverse)
library(cbcTools)
library(here)

# 1. Section 2 - SMC Programs
# 1.1 Profiles
smc_profiles <- cbc_profiles(
    enrollment_cash      = c(50, 100, 200, 300), # USD (one time)
    monthly_cash         = c(2, 5, 10, 15, 20),  # USD per month
    override             = c(0, 1, 3, 5),        # times per month
    minimum_threshold    = c(20, 30, 40),        # percentage
    guaranteed_threshold = c(60, 70, 80)         # percentage
    
)

# 1.2 Full factorial design
smc_design <- cbc_design(
    profiles = smc_profiles,
    n_resp   = 4600, # Number of respondents
    n_alts   = 2,    # Number of alternatives per question
    n_q      = 6,    # Number of questions per respondent
    no_choice = TRUE
)

# 1.3 Save design
write_csv(smc_design, here("data", "smc_questions.csv"))

# 2. Section 3 - V2G Programs
# 2.1 Profiles
v2g_profiles <- cbc_profiles(
    enrollment_cash      = c(50, 100, 200, 300), # USD (one time)
    occurrence_cash      = c(2, 5, 10, 15, 20),  # USD per occurrence
    monthly_occurrence   = c(1, 2, 3, 4),        # Monthly occurrence
    lower_bound          = c(20, 30, 40),        # Percentage
    guaranteed_threshold = c(60, 70, 80)         # Percentage
)

# 2.2 Full factorial design
v2g_design <- cbc_design(
    profiles = v2g_profiles,
    n_resp   = 4600, # Number of respondents
    n_alts   = 2,    # Number of alternatives per question
    n_q      = 6,    # Number of questions per respondent
    no_choice = TRUE
)

# 2.3 Save design
write_csv(v2g_design, here("data", "v2g_questions.csv"))