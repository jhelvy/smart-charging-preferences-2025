library(tidyverse)
library(cbcTools)
library(here)

# 1. Profiles
smc_profiles <- cbc_profiles(
    enrollment_cash      = c(100, 200, 300),     # USD (one time)
    monthly_cash         = c(10, 15, 20),        # USD per month
    immediate_charge     = c(1, 2, 3, 4),        # times per month
    time_control         = c('5pm_9am', '24_7'),
    minimum_threshold    = c(20, 30, 40),        # percentage
    guaranteed_threshold = c(60, 70, 80)         # percentage
)

# 2. Full factorial design
smc_design <- cbc_design(
    profiles = smc_profiles,
    n_resp   = 1000,  # Number of respondents
    n_alts   = 2,     # Number of alternatives per question
    n_q      = 6,     # Number of questions per respondent
    no_choice = TRUE
)

# 3. Simulate random choices
smc_choice_rand <- cbc_choices(
    design = smc_design,
    obsID = 'obsID'
)

# 4. Power analysis
smc_power_rand <- cbc_power(
    data    = smc_choice_rand,
    pars    = c('enrollment_cash', 'monthly_cash', 'immediate_charge',
                'time_control_5pm_9am', 'minimum_threshold',
                'guaranteed_threshold'),
    obsID   = 'obsID',
    outcome = 'choice',
    nbreaks = 10,
    n_q     = 6
)

# 5. Plot
ggplot(smc_power_rand) +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 2) +
    geom_point(aes(x = sampleSize, y = se, color = coef)) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0, 0.125)) +
    theme_bw(base_family = 'Ubuntu') + 
    labs(
        x = 'Sample size', 
        y = 'Standard error', 
        color = 'Coefficient',
        title = 'Randomized Design Plot'
    )
