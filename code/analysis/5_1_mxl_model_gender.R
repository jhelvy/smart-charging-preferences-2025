# This script cannot run by itself.
# It has to be called by the R scripts in the parent folder.

################
# SMC Programs #
################

# 1. SMC Model Summary
# 1.1 Use the logitr package to generate the model
smc_data <- read_csv(file.path(processed_dir, "smc_data.csv"))
smc_data <- smc_data %>%
    mutate(weights = ifelse(tolower(mc_gender) == "male", 0.688, 1.832))

smc_mxl_model <- logitr(
    data = smc_data,
    outcome = "choice",
    obsID = "obs_id",
    panelID = "resp_id",
    pars = c(
        "enrollment_cash",
        "monthly_cash",
        "override_days",
        "override_flag",
        "minimum_threshold",
        "guaranteed_threshold",
        "no_choice"
    ),
    weights = "weights",
    randPars = c(
        override_days = "n",
        override_flag = "n",
        guaranteed_threshold = "cn"
    ),
    numMultiStarts = 50,
    drawType = "sobol",
    numDraws = 500,
    numCores = 3
)

# Get the coefficients
smc_mxl_coef <- coef(smc_mxl_model)
# smc_mxl_cov <- vcov(smc_mxl_model)
#
# # Take 10^4 draws of the coefs
# smc_mxl_coef_draws <- as.data.frame(MASS::mvrnorm(10^4, smc_mxl_coef, smc_mxl_cov))
#
# # exp the ln paras
# smc_mxl_coef_draws$enrollment_cash <- exp(smc_mxl_coef_draws$enrollment_cash)
# smc_mxl_coef_draws$monthly_cash <- exp(smc_mxl_coef_draws$monthly_cash)
#
# # compute se - preferred by journals
# smc_mxl_se <- apply(smc_mxl_coef_draws, 2, sd)
#
# # get ci
# smc_mxl_coef_ci <- ci(smc_mxl_coef_draws, level = 0.95)

# 1.2 Use the summary() function to see model summary
summary(smc_mxl_model)

# 2. Model Evaluation
# 2.1 First order condition - Is the gradient close to zero?
smc_mxl_model$gradient

# 2.2 Second order condition - Is the hessian negative definite?
eigen(smc_mxl_model$hessian)$values

# 2.3 Save the model
save(
    smc_mxl_model,
    file = file.path(processed_dir, "2_models", "smc_mxl_model_gender.RData")
)

# 3. SMC Model Equation
# 3.1 Estimated values and std err
smc_mxl_table <- data.frame(
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
    Estimate = smc_mxl_coef[1:7],
    StdError = se(smc_mxl_model)[1:7],
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
smc_mxl_table$Estimate <- round(smc_mxl_table$Estimate, 4)
smc_mxl_table$StdError <- round(smc_mxl_table$StdError, 4)

# 3.2 Model figure
# 3.2.1 Construct the data frame
smc_mxl_fig <- data.frame(
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
    Estimate = head(smc_mxl_coef, 7),
    StdError = head(se(smc_mxl_model), 7)
)

smc_mxl_fig$LowerBound = smc_mxl_fig$Estimate - 1.96 * smc_mxl_fig$StdError
smc_mxl_fig$UpperBound = smc_mxl_fig$Estimate + 1.96 * smc_mxl_fig$StdError

# 3.2.2 Create the plot
smc_mxl_plot <- smc_mxl_fig %>%
    ggplot(aes(x = Attribute, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = LowerBound, ymax = UpperBound), width = 0.2) +
    labs(
        title = "SMC MXL Coefficient Estimates",
        x = "Attribute",
        y = "Estimate"
    ) +
    coord_flip() +
    theme_bw(base_family = "Ubuntu")

smc_mxl_plot

ggsave(
    filename = file.path(
        processed_dir,
        "2_models",
        "smc_mxl_plot_gender.png"
    ),
    plot = smc_mxl_plot,
    width = 6,
    height = 6 / 1.618
)

################
# V2G Programs #
################

# 1. V2G Model Summary
# 1.1 Use the logitr package to generate the model
v2g_data <- read_csv(file.path(processed_dir, "v2g_data.csv"))
v2g_data <- v2g_data %>%
    mutate(weights = ifelse(tolower(mc_gender) == "male", 0.688, 1.832))

v2g_mxl_model <- logitr(
    data = v2g_data,
    outcome = "choice",
    obsID = "obs_id",
    panelID = "resp_id",
    pars = c(
        "enrollment_cash",
        "occurrence_cash",
        "monthly_occurrence",
        "lower_bound",
        "guaranteed_threshold",
        "no_choice"
    ),
    weights = "weights",
    randPars = c(
        monthly_occurrence = "n",
        lower_bound = "n",
        guaranteed_threshold = "cn"
    ),
    numMultiStarts = 50,
    drawType = "sobol",
    numDraws = 500,
    numCores = 3
)

# Get the coefficients
v2g_mxl_coef <- coef(v2g_mxl_model)

# 1.2 Use the summary() function to see model summary
summary(v2g_mxl_model)

# 2. Model Evaluation
# 2.1 First order condition - Is the gradient close to zero?
v2g_mxl_model$gradient

# 2.2 Second order condition - Is the hessian negative definite?
eigen(v2g_mxl_model$hessian)$values

# 2.3 Save the model
save(
    v2g_mxl_model,
    file = file.path(processed_dir, "2_models", "v2g_mxl_model_gender.RData")
)

# 3. V2G Model Equation
# 3.1 Estimated values and std err
v2g_mxl_table <- data.frame(
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
    Estimate = v2g_mxl_coef[1:6],
    StdError = se(v2g_mxl_model)[1:6],
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
v2g_mxl_table$Estimate <- round(v2g_mxl_table$Estimate, 4)
v2g_mxl_table$StdError <- round(v2g_mxl_table$StdError, 4)

# 3.2 Model figure
# 3.2.1 Construct the data frame
v2g_mxl_fig <- data.frame(
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
    Estimate = head(v2g_mxl_coef, 6),
    StdError = head(se(v2g_mxl_model), 6)
)

v2g_mxl_fig$LowerBound = v2g_mxl_fig$Estimate - 1.96 * v2g_mxl_fig$StdError
v2g_mxl_fig$UpperBound = v2g_mxl_fig$Estimate + 1.96 * v2g_mxl_fig$StdError

# 3.2.2 Create the plot
v2g_mxl_plot <- v2g_mxl_fig %>%
    ggplot(aes(x = Attribute, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = LowerBound, ymax = UpperBound), width = 0.2) +
    labs(
        title = "V2G MXL Coefficient Estimates",
        x = "Attribute",
        y = "Estimate"
    ) +
    coord_flip() +
    theme_bw(base_family = "Ubuntu")

v2g_mxl_plot

ggsave(
    filename = file.path(
        processed_dir,
        "2_models",
        "v2g_mxl_plot_gender.png"
    ),
    plot = v2g_mxl_plot,
    width = 6,
    height = 6 / 1.618
)
