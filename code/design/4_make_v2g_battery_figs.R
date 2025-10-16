library(tidyverse)
library(here)

# Define the function
generate_plot <- function(user_rng, guar_thre, lower_bound) {
    # Thresholds
    threshold <- data.frame(
        xmin = c(0, lower_bound, guar_thre),
        xmax = c(lower_bound, guar_thre, user_rng),
        ymin = 0,
        ymax = 0.1,
        fill = c("lightgrey", "sandybrown", "lightgreen")
    )

    # Plot
    battery_plot <- ggplot(
        threshold,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)
    ) +

        geom_rect(color = "black", linewidth = 0.3) +

        geom_segment(
            aes(x = lower_bound, xend = lower_bound, y = 0, yend = 0.1),
            color = "royalblue",
            size = 0.3
        ) +
        geom_segment(
            aes(x = guar_thre, xend = guar_thre, y = 0, yend = 0.1),
            color = "royalblue",
            size = 0.3
        ) +

        geom_label(
            aes(x = lower_bound, y = 0.17, label = "Low"),
            label.padding = unit(0.2, "lines"),
            size = 4,
            hjust = 0.5,
            family = "Ubuntu",
            fill = "lightblue"
        ) +
        geom_label(
            aes(x = guar_thre, y = 0.17, label = "Guaranteed"),
            label.padding = unit(0.2, "lines"),
            size = 4,
            hjust = 0.5,
            family = "Ubuntu",
            fill = "lightblue"
        ) +

        geom_segment(
            aes(x = lower_bound, xend = lower_bound, y = 0.11, yend = 0.13),
            arrow = arrow(
                length = unit(0.03, "npc"),
                type = "closed",
                ends = "first"
            )
        ) +
        geom_segment(
            aes(x = guar_thre, xend = guar_thre, y = 0.11, yend = 0.13),
            arrow = arrow(
                length = unit(0.03, "npc"),
                type = "closed",
                ends = "first"
            )
        ) +

        scale_fill_identity() +
        scale_x_continuous(
            breaks = c(0, lower_bound, guar_thre, user_rng),
            labels = c(0, lower_bound, guar_thre, paste(user_rng, "miles"))
        ) +
        labs(x = "", y = "") +
        coord_cartesian(ylim = c(0, 0.2)) +
        theme_bw(base_family = "Ubuntu") +
        theme(
            legend.position = "none",
            axis.text.x = element_text(size = 12),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            plot.margin = margin(5, 20, 5, 5)
        )

    # Save
    plot_name <- paste0(
        "u_",
        user_rng,
        "_g_",
        guar_thre,
        "_l_",
        lower_bound,
        ".png"
    )
    ggsave(
        filename = here("battery_v2g", plot_name),
        plot = battery_plot,
        width = 4,
        height = 4 * 0.3,
        units = "in"
    )
}

# Call the function to generate every possible combination
user_rng_vals <- seq(100, 600, by = 50)
guar_pct <- c(0.60, 0.70, 0.80)
low_pct <- c(0.20, 0.30, 0.40)

for (user_rng in user_rng_vals) {
    for (guar_p in guar_pct) {
        for (low_p in low_pct) {
            guar_thre <- round((user_rng * guar_p) / 5) * 5
            lower_bound <- round((user_rng * low_p) / 5) * 5
            generate_plot(user_rng, guar_thre, lower_bound)
        }
    }
}
