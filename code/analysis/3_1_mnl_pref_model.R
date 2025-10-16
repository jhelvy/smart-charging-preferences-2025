# This script cannot run by itself.
# It has to be called by the R scripts in the parent folder.

################
# SMC Programs #
################

# 1. SMC Model Summary
# 1.1 Use the logitr package to generate the model
smc_data <- read_csv(file.path(processed_dir, "smc_data.csv"))

# Count of No Choice
# smc_data %>%
#     filter(choice == 1) %>%
#     mutate(no_choice_chosen = choice == no_choice) %>%
#     count(no_choice_chosen, source, q_id) %>%
#     group_by(source) %>%
#     mutate(p = n / sum(n)) %>%
#     arrange(no_choice_chosen, q_id)

smc_mnl_model <- logitr(
    data = smc_data,
    outcome = "choice",
    obsID = "obs_id",
    pars = c(
        "enrollment_cash",
        "monthly_cash",
        "override_days",
        "override_flag",
        "minimum_threshold",
        "guaranteed_threshold",
        "no_choice"
    )
)

# 1.2 Use the summary() function to see model summary
summary(smc_mnl_model)

# 2. SMC Model Evaluation
# 2.1 First order condition - Is the gradient close to zero?
smc_mnl_model$gradient

# 2.2 Second order condition - Is the hessian negative definite?
eigen(smc_mnl_model$hessian)$values

# 2.3 Save the model
save(
    smc_mnl_model,
    file = file.path(processed_dir, "2_models", "smc_mnl_model.RData")
)

# 3. SMC Model Equation
# 3.1 Estimated values and std err
smc_mnl_table <- data.frame(
    Coefficient = c(
        "&beta;<sub>1</sub>",
        "&beta;<sub>2</sub>",
        "&beta;<sub>3</sub>",
        "&beta;<sub>4</sub>",
        "&beta;<sub>5</sub>",
        "&beta;<sub>6</sub>",
        "&beta;<sub>7</sub>"
    ),
    Meaning = c(
        "Enrollment Cash",
        "Monthly Cash",
        "Override Days",
        "Override Flag",
        "Minimum Threshold",
        "Guaranteed Threshold",
        "No Choice"
    ),
    Estimate = coef(smc_mnl_model),
    StdError = se(smc_mnl_model),
    Level = c(
        "50, 100, 200, 300",
        "2, 5, 10, 15, 20",
        "0, 1, 3, 5",
        "-",
        "20, 30, 40",
        "60, 70, 80",
        "-"
    ),
    Unit = c("USD", "USD", "Days", "-", "%", "%", "-")
)
smc_mnl_table$Estimate <- round(smc_mnl_table$Estimate, 4)
smc_mnl_table$StdError <- round(smc_mnl_table$StdError, 4)

# 3.2 Model figure
# 3.2.1 Construct the data frame
smc_mnl_fig <- data.frame(
    Attribute = factor(
        c(
            "Enrollment Cash",
            "Monthly Cash",
            "Override Days",
            "Override Flag",
            "Min Threshold",
            "Guaranteed Threshold",
            "No Choice"
        ),
        levels = c(
            "No Choice",
            "Guaranteed Threshold",
            "Min Threshold",
            "Override Flag",
            "Override Days",
            "Monthly Cash",
            "Enrollment Cash"
        )
    ),
    Estimate = coef(smc_mnl_model),
    StdError = se(smc_mnl_model)
)

smc_mnl_fig$LowerBound = smc_mnl_fig$Estimate - 1.96 * smc_mnl_fig$StdError
smc_mnl_fig$UpperBound = smc_mnl_fig$Estimate + 1.96 * smc_mnl_fig$StdError

# 3.2.2 Create the plot
smc_mnl_plot <- smc_mnl_fig %>%
    ggplot(aes(x = Attribute, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = LowerBound, ymax = UpperBound), width = 0.2) +
    labs(
        title = "SMC MNL Coefficient Estimates",
        x = "Attribute",
        y = "Estimate"
    ) +
    coord_flip() +
    theme_bw(base_family = "Ubuntu")

smc_mnl_plot

ggsave(
    filename = file.path(processed_dir, "2_models", "smc_mnl_plot.png"),
    plot = smc_mnl_plot,
    width = 6,
    height = 6 / 1.618
)

################
# V2G Programs #
################

# 1. V2G Model Summary
# 1.1 Use the logitr package to generate the model
v2g_data <- read_csv(file.path(processed_dir, "v2g_data.csv"))

# Count of No Choice
# v2g_data %>%
#     filter(choice == 1) %>%
#     mutate(no_choice_chosen = choice == no_choice) %>%
#     count(no_choice_chosen)

v2g_mnl_model <- logitr(
    data = v2g_data,
    outcome = "choice",
    obsID = "obs_id",
    pars = c(
        "enrollment_cash",
        "occurrence_cash",
        "monthly_occurrence",
        "lower_bound",
        "guaranteed_threshold",
        "no_choice"
    )
)

# 1.2 Use the summary() function to see model summary
summary(v2g_mnl_model)

# 2. V2G Model Evaluation
# 2.1 First order condition - Is the gradient close to zero?
v2g_mnl_model$gradient

# 2.2 Second order condition - Is the hessian negative definite?
eigen(v2g_mnl_model$hessian)$values

# 2.3 Save the model
save(
    v2g_mnl_model,
    file = file.path(processed_dir, "2_models", "v2g_mnl_model.RData")
)

# 3. V2G Model Equation
# 3.1 Estimated values and std err
v2g_mnl_table <- data.frame(
    Coefficient = c(
        "&beta;<sub>1</sub>",
        "&beta;<sub>2</sub>",
        "&beta;<sub>3</sub>",
        "&beta;<sub>4</sub>",
        "&beta;<sub>5</sub>",
        "&beta;<sub>6</sub>"
    ),
    Meaning = c(
        "Enrollment Cash",
        "Occurrence Cash",
        "Monthly Occurrence",
        "Lower Bound",
        "Guaranteed Threshold",
        "No Choice"
    ),
    Estimate = coef(v2g_mnl_model),
    StdError = se(v2g_mnl_model),
    Level = c(
        "50, 100, 200, 300",
        "2, 5, 10, 15, 20",
        "1, 2, 3, 4",
        "20, 30, 40",
        "60, 70, 80",
        "-"
    ),
    Unit = c("USD", "USD", "Times", "%", "%", "-")
)
v2g_mnl_table$Estimate <- round(v2g_mnl_table$Estimate, 4)
v2g_mnl_table$StdError <- round(v2g_mnl_table$StdError, 4)

# 3.2 Model figure
# 3.2.1 Construct the data frame
v2g_mnl_fig <- data.frame(
    Attribute = factor(
        c(
            "Enrollment Cash",
            "Occurrence Cash",
            "Monthly Occurrence",
            "Lower Bound",
            "Guaranteed Threshold",
            "No Choice"
        ),
        levels = c(
            "No Choice",
            "Guaranteed Threshold",
            "Lower Bound",
            "Monthly Occurrence",
            "Occurrence Cash",
            "Enrollment Cash"
        )
    ),
    Estimate = coef(v2g_mnl_model),
    StdError = se(v2g_mnl_model)
)

v2g_mnl_fig$LowerBound = v2g_mnl_fig$Estimate - 1.96 * v2g_mnl_fig$StdError
v2g_mnl_fig$UpperBound = v2g_mnl_fig$Estimate + 1.96 * v2g_mnl_fig$StdError

# 3.2.2 Create the plot
v2g_mnl_plot <- v2g_mnl_fig %>%
    ggplot(aes(x = Attribute, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = LowerBound, ymax = UpperBound), width = 0.2) +
    labs(
        title = "V2G MNL Coefficient Estimates",
        x = "Attribute",
        y = "Estimate"
    ) +
    coord_flip() +
    theme_bw(base_family = "Ubuntu")

v2g_mnl_plot

ggsave(
    filename = file.path(processed_dir, "2_models", "v2g_mnl_plot.png"),
    plot = v2g_mnl_plot,
    width = 6,
    height = 6 / 1.618
)
