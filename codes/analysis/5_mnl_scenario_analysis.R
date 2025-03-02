# This script cannot run by itself.
# It has to be called by the R scripts in the parent folder.

################
# SMC Programs #
################

# 0. Load smc_mnl_model
load(file.path(processed_dir, "2_models", "smc_mnl_model.RData"))

# 1. Scenario Analysis
smc_scenarios <- read_csv(here("project", "data", "smc_scenarios.csv"))

smc_mnl_scenario <- predict(
    smc_mnl_model,
    newdata    = smc_scenarios,
    obsID      = "obs_id", 
    level      = 0.95,
    interval   = "confidence",
    returnData = TRUE
)

save(smc_mnl_scenario,
     file = file.path(processed_dir,
                      "4_scenario_analysis",
                      "smc_mnl_scenario.RData"))

# 2. Scenario Analysis Plots
smc_scenarios_grouped <- data.frame(
    scenario_id = 1:6,
    attribute = rep(c("One-time Cash",
                      "Recurring Cash",
                      "Flexibility"), each = 2),
    level = c("$50 Enrollment Cash", "$300 Enrollment Cash",
              "$5 Monthly Cash", "$20 Monthly Cash",
              "1 Day Override + 30/70% Battery",
              "5 Days Override + 40/80% Battery")
)

smc_mnl_scenario_plot <- smc_mnl_scenario %>% 
    filter(alt_id == 1) %>%
    left_join(smc_scenarios_grouped, by = c("obs_id" = "scenario_id")) %>%
    group_by(attribute) %>%
    mutate(
        is_higher = predicted_prob == max(predicted_prob),
        max_prob = max(predicted_prob)
    ) %>%
    ungroup() %>%
    mutate(attribute = factor(attribute, 
                              levels = c("One-time Cash", 
                                         "Recurring Cash", 
                                         "Flexibility"))) %>% 
    ggplot(aes(
        x = predicted_prob,
        y = attribute,
        fill = is_higher)) +
    geom_col(position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_errorbar(aes(xmin = predicted_prob_lower, 
                      xmax = predicted_prob_upper),
                  position = position_dodge(width = 0.8),
                  width = 0.4) +
    geom_text(aes(x = predicted_prob_upper + 0.01,
                  label = scales::percent(predicted_prob, accuracy = 1)),
              position = position_dodge(width = 0.8),
              hjust = 0, vjust = 0.5, size = 3.5, family = "Ubuntu") +
    geom_text(aes(x = 0.02, 
                  label = level,
                  y = attribute),
              position = position_dodge(width = 0.8),
              hjust = 0, vjust = 0.5, size = 3.5, family = "Ubuntu") +
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.25),
        labels = scales::percent,
        expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_manual(values = c("gray75", "gray60")) +
    labs(x = "Enrollment Rate",
         y = "Scenarios",
         title = "A) Supplier-Managed Charging (SMC)") +
    theme_minimal_vgrid(font_family = "Ubuntu") +
    theme(legend.position = "none")

smc_mnl_scenario_plot

ggsave(
    filename = file.path(processed_dir,
                         "4_scenario_analysis",
                         "smc_mnl_scenario_plot.png"),
    plot = smc_mnl_scenario_plot,
    width = 7,
    height = 7 / 1.618
)

################
# V2G Programs #
################

# 0. Load v2g_mnl_model
load(file.path(processed_dir, "2_models", "v2g_mnl_model.RData"))

# 1. Scenario Analysis
v2g_scenarios <- read_csv(here("project", "data", "v2g_scenarios.csv"))

v2g_mnl_scenario <- predict(
    v2g_mnl_model,
    newdata    = v2g_scenarios, 
    obsID      = "obs_id", 
    level      = 0.95,
    interval   = "confidence",
    returnData = TRUE
)

save(v2g_mnl_scenario,
     file = file.path(processed_dir,
                      "4_scenario_analysis",
                      "v2g_mnl_scenario.RData"))

# 2. Scenario Analysis Plots
v2g_scenarios_grouped <- data.frame(
    scenario_id = 1:6,
    attribute = rep(c("One-time Cash",
                      "Recurring Cash",
                      "Flexibility"), each = 2),
    level = c("$50 Enrollment Cash", "$300 Enrollment Cash",
              "$5 Occurrence Cash + 2 Monthly Occurrence",
              "$20 Occurrence Cash + 4 Monthly Occurrence",
              "30/70% Battery", "40/80% Battery")
)

v2g_mnl_scenario_plot <- v2g_mnl_scenario %>% 
    filter(alt_id == 1) %>%
    left_join(v2g_scenarios_grouped, by = c("obs_id" = "scenario_id")) %>%
    group_by(attribute) %>%
    mutate(
        is_higher = predicted_prob == max(predicted_prob),
        max_prob = max(predicted_prob)
    ) %>%
    ungroup() %>%
    mutate(attribute = factor(attribute, 
                              levels = c("One-time Cash", 
                                         "Recurring Cash", 
                                         "Flexibility"))) %>% 
    ggplot(aes(
        x = predicted_prob,
        y = attribute,
        fill = is_higher)) +
    geom_col(position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_errorbar(aes(xmin = predicted_prob_lower, 
                      xmax = predicted_prob_upper),
                  position = position_dodge(width = 0.8),
                  width = 0.4) +
    geom_text(aes(x = predicted_prob_upper + 0.01,
                  label = scales::percent(predicted_prob, accuracy = 1)),
              position = position_dodge(width = 0.8),
              hjust = 0, vjust = 0.5, size = 3.5, family = "Ubuntu") +
    geom_text(aes(x = 0.02, 
                  label = level,
                  y = attribute),
              position = position_dodge(width = 0.8),
              hjust = 0, vjust = 0.5, size = 3.5, family = "Ubuntu") +
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.25),
        labels = scales::percent,
        expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_manual(values = c("gray75", "gray60")) +
    labs(x = "Enrollment Rate",
         y = "Scenarios",
         title = "B) Vehicle-to-Grid (V2G)") +
    theme_minimal_vgrid(font_family = "Ubuntu") +
    theme(legend.position = "none")

v2g_mnl_scenario_plot

ggsave(
    filename = file.path(processed_dir,
                         "4_scenario_analysis",
                         "v2g_mnl_scenario_plot.png"),
    plot = v2g_mnl_scenario_plot,
    width = 7,
    height = 7 / 1.618
)

####################
# Plot Combination #
####################

mnl_scenario_plots <- 
    (wrap_elements(full = smc_mnl_scenario_plot)) /
    (wrap_elements(full = v2g_mnl_scenario_plot))
mnl_scenario_plots

ggsave(
    filename = file.path(processed_dir,
                         "4_scenario_analysis",
                         "mnl_scenario_plots.png"),
    plot = mnl_scenario_plots,
    width = 10,
    height = 10 / 1.618
)
