load(file.path(processed_dir, "2_models", "smc_mxl_model.RData"))
summary(smc_mxl_model)

smc_baseline_2 <- data.frame(
  alt_id = c(1, 2),
  obs_id = c(1, 1),
  enrollment_cash = c(0, 0),
  monthly_cash = c(0, 0),
  override_days = c(0, 0),
  override_flag = c(0, 0),
  minimum_threshold = c(0, 0),
  guaranteed_threshold = c(40, 0),
  no_choice = c(0, 1)
)

# Sensitivity of user enrollment to changes in "monthly_cash"
smc_monthly_levels_2 <- seq(0, 85, by = 1)
smc_monthly_numbers_2 <- length(smc_monthly_levels_2)
smc_monthly_scenarios_2 <- do.call(
  bind_rows,
  replicate(smc_monthly_numbers_2, smc_baseline_2, simplify = FALSE)
)
smc_monthly_scenarios_2$obs_id <- rep(seq(smc_monthly_numbers_2), each = 2)
smc_monthly_scenarios_2$monthly_cash[which(
  smc_monthly_scenarios_2$alt_id == 1
)] <- smc_monthly_levels_2

smc_mxl_sens_monthly_2 <- predict(
  smc_mxl_model,
  newdata = smc_monthly_scenarios_2,
  obsID = "obs_id",
  level = 0.95,
  interval = "confidence",
  returnData = TRUE
) %>%
  filter(alt_id == 1) %>%
  select(monthly_cash, starts_with("predicted_"))

# Plot of user enrollment change to "monthly_cash"
smc_mxl_sens_monthly_plot_2 <- smc_mxl_sens_monthly_2 %>%
  ggplot(aes(
    x = monthly_cash,
    y = predicted_prob,
    ymin = predicted_prob_lower,
    ymax = predicted_prob_upper
  )) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_line(
    data = smc_mxl_sens_monthly_2 %>%
      filter(monthly_cash <= 20, monthly_cash >= 2),
    linetype = "solid"
  ) +
  expand_limits(x = c(0, 85), y = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Monthly Cash (USD)",
    y = "Enrollment Rate",
    title = "SMC Enrollment Rate Sensitivity of Monthly Cash"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(panel.grid.minor = element_blank())

smc_mxl_sens_monthly_plot_2

# Find the monthly cash value that achieves close to 100% enrollment rate
target_enrollment <- 0.99
closest_row <- which.min(abs(
  smc_mxl_sens_monthly_2$predicted_prob - target_enrollment
))
required_monthly_cash <- smc_mxl_sens_monthly_2$monthly_cash[closest_row]
achieved_rate <- smc_mxl_sens_monthly_2$predicted_prob[closest_row]

cat("To achieve", target_enrollment * 100, "% enrollment rate:\n")
cat("Required monthly cash: $", required_monthly_cash, "\n")
cat("Achieved enrollment rate:", round(achieved_rate * 100, 2), "%\n")

# Save the plot
ggsave(
  filename = file.path(
    processed_dir,
    "3_enrollment_sensitivity",
    "smc_sens_monthly_plot.png"
  ),
  plot = smc_mxl_sens_monthly_plot_2,
  width = 6,
  height = 6 / 1.618
)

# Save the data
save(
  smc_mxl_sens_monthly_2,
  file = file.path(
    processed_dir,
    "3_enrollment_sensitivity",
    "smc_sens_monthly.RData"
  )
)
