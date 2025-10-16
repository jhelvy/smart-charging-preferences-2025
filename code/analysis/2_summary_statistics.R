# This script cannot run by itself.
# It has to be called by the R scripts in the parent folder.

# 1. Car Ownership
# 1.1 Load and pivot the data
load(file.path(processed_dir, "choice_data_wider.RData"))
choice_data_ownership <- choice_data_wider %>%
    mutate(across(
        c(
            mc_car_number,
            has_bev,
            mc_commute_method,
            mc_commute_days,
            mc_distance,
            mc_neighbor
        ),
        as.character
    )) %>%
    pivot_longer(
        cols = c(
            mc_car_number,
            has_bev,
            mc_commute_method,
            mc_commute_days,
            mc_distance,
            mc_neighbor
        ),
        names_to = "category",
        values_to = "value"
    )

# 1.2 Reorder and relabel the values based on category
choice_data_ownership <- choice_data_ownership %>%
    separate_rows(value, sep = ",\\s*") %>%
    mutate(value = trimws(value)) %>%
    mutate(
        category = case_when(
            category == "mc_car_number" ~ "Household Car Number",
            category == "has_bev" ~ "Own BEV",
            category == "mc_commute_method" ~ "Commute Method",
            category == "mc_commute_days" ~ "Commute Days",
            category == "mc_distance" ~ "Daily Distance",
            category == "mc_neighbor" ~ "Neighbor Ownership",
            TRUE ~ category
        ),
        value = case_when(
            category == "Household Car Number" ~
                factor(
                    value,
                    levels = c("0", "1", "2", "3", "4", "5"),
                    labels = c("0", "1", "2", "3", "4", "5 or More")
                ),
            category == "Own BEV" ~
                factor(
                    value,
                    levels = c("yes", "no"),
                    labels = c("Yes", "No but Plan to")
                ),
            category == "Commute Method" ~
                factor(
                    value,
                    levels = c("by_car", "by_bus", "by_train", "by_walk", "NA"),
                    labels = c("Car", "Bus", "Train", "Walk", "No Need")
                ),
            category == "Commute Days" ~
                factor(
                    value,
                    levels = c("NA", "1", "2", "3", "4", "5", "6", "7"),
                    labels = c("0", "1", "2", "3", "4", "5", "6", "7")
                ),
            category == "Daily Distance" ~
                factor(
                    value,
                    levels = c(
                        "below_10",
                        "10_to_30",
                        "31_to_50",
                        "51_to_100",
                        "over_100",
                        "no"
                    ),
                    labels = c(
                        "<10",
                        "10-30",
                        "31-50",
                        "51-100",
                        ">100",
                        "Don't Drive"
                    )
                ),
            category == "Neighbor Ownership" ~
                factor(
                    value,
                    levels = c("yes", "no", "not_sure"),
                    labels = c("Own BEV", "Don't Own", "Not Sure")
                ),
            TRUE ~ as.factor(value)
        )
    )

# 1.3 Plot the ownership summary
ownership_plot <- choice_data_ownership %>%
    ggplot(aes(x = value)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(
        ~ fct_relevel(
            category,
            "Household Car Number",
            "Own BEV",
            "Commute Method",
            "Commute Days",
            "Daily Distance",
            "Neighbor Ownership"
        ),
        scales = "free_x",
        nrow = 3
    ) +
    labs(
        x = NULL,
        y = "Count",
        title = "Car Ownership",
        subtitle = "Demographics Summary 1"
    ) +
    theme_bw(base_family = "Ubuntu")

ownership_plot

ggsave(
    filename = file.path(processed_dir, "1_summary", "1_ownership.png"),
    plot = ownership_plot,
    width = 8,
    height = 8 / 1.618
)

# 2. Charging Preferences
# 2.1 Pivot the data
choice_data_charging <- choice_data_wider %>%
    mutate(across(
        c(
            mc_home_charge,
            mc_home_days,
            mc_home_hours,
            mc_work_charge,
            mc_work_days,
            mc_work_hours,
            mc_umc,
            mc_lv2_charger
        ),
        as.character
    )) %>%
    pivot_longer(
        cols = c(
            mc_home_charge,
            mc_home_days,
            mc_home_hours,
            mc_work_charge,
            mc_work_days,
            mc_work_hours,
            mc_umc,
            mc_lv2_charger
        ),
        names_to = "category",
        values_to = "value"
    )

# 2.2 Reorder and relabel the values based on category
choice_data_charging <- choice_data_charging %>%
    mutate(
        category = case_when(
            category == "mc_home_charge" ~ "Home Charge",
            category == "mc_home_days" ~ "Home Days",
            category == "mc_home_hours" ~ "Home Hours",
            category == "mc_work_charge" ~ "Work Charge",
            category == "mc_work_days" ~ "Work Days",
            category == "mc_work_hours" ~ "Work Hours",
            category == "mc_umc" ~ "Charge Management",
            category == "mc_lv2_charger" ~ "Lv2 Charger",
            TRUE ~ category
        ),
        value = case_when(
            category == "Home Charge" ~
                factor(value, levels = c("yes", "no"), labels = c("Yes", "No")),
            category == "Home Days" ~
                factor(
                    value,
                    levels = c("1", "2", "3", "4", "5", "6", "7", "NA"),
                    labels = c(
                        "1",
                        "2",
                        "3",
                        "4",
                        "5",
                        "6",
                        "7",
                        as.character(NA)
                    )
                ),
            category == "Home Hours" ~
                factor(
                    value,
                    levels = c(
                        "less_than_3",
                        "3_to_5",
                        "5_to_8",
                        "more_than_8",
                        "NA"
                    ),
                    labels = c("<3", "3-5", "5-8", ">8", as.character(NA))
                ),
            category == "Work Charge" ~
                factor(value, levels = c("yes", "no"), labels = c("Yes", "No")),
            category == "Work Days" ~
                factor(
                    value,
                    levels = c("1", "2", "3", "4", "5", "6", "7", "NA"),
                    labels = c(
                        "1",
                        "2",
                        "3",
                        "4",
                        "5",
                        "6",
                        "7",
                        as.character(NA)
                    )
                ),
            category == "Work Hours" ~
                factor(
                    value,
                    levels = c(
                        "less_than_3",
                        "3_to_5",
                        "5_to_8",
                        "more_than_8",
                        "NA"
                    ),
                    labels = c("<3", "3-5", "5-8", ">8", as.character(NA))
                ),
            category == "Charge Management" ~
                factor(
                    value,
                    levels = c("app", "smc", "no"),
                    labels = c("App", "SMC", as.character(NA))
                ),
            category == "Lv2 Charger" ~
                factor(value, levels = c("yes", "no"), labels = c("Yes", "No")),
            TRUE ~ as.factor(value)
        )
    )

# 2.3 Plot the ownership summary
charging_plot <- choice_data_charging %>%
    ggplot(aes(x = value)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(
        ~ fct_relevel(
            category,
            "Home Charge",
            "Home Days",
            "Home Hours",
            "Work Charge",
            "Work Days",
            "Work Hours",
            "Charge Management",
            "Lv2 Charger"
        ),
        scales = "free_x",
        nrow = 3
    ) +
    labs(
        x = NULL,
        y = "Count",
        title = "Charging Preferences",
        subtitle = "Demographics Summary 2"
    ) +
    theme_bw(base_family = "Ubuntu")

charging_plot

ggsave(
    filename = file.path(processed_dir, "1_summary", "2_charging.png"),
    plot = charging_plot,
    width = 8,
    height = 8 / 1.618
)

# 3. Personal Info A - Major Info
# 3.1 Pivot the data
choice_data_personal_a <- choice_data_wider %>%
    mutate(across(
        c(mc_climate, mc_gender, mc_work, mc_party, mc_v2g, mc_v2g_charger),
        as.character
    )) %>%
    pivot_longer(
        cols = c(
            mc_climate,
            mc_gender,
            mc_work,
            mc_party,
            mc_v2g,
            mc_v2g_charger
        ),
        names_to = "category",
        values_to = "value"
    )

# 3.2 Reorder and relabel the values based on category
choice_data_personal_a <- choice_data_personal_a %>%
    mutate(
        category = case_when(
            category == "mc_climate" ~ "Climate Awareness",
            category == "mc_gender" ~ "Gender",
            category == "mc_work" ~ "Work Status",
            category == "mc_party" ~ "Party",
            category == "mc_v2g" ~ "V2G Interest",
            category == "mc_v2g_charger" ~ "Pay for V2G Charger",
            TRUE ~ category
        ),
        value = case_when(
            category == "Climate Awareness" ~
                factor(
                    value,
                    levels = c("not", "somewhat", "neutral", "believe", "very"),
                    labels = c("Not", "Somewhat", "Neutral", "Believe", "Very")
                ),
            category == "Gender" ~
                factor(
                    value,
                    levels = c("male", "female", "non_bi", "prefer_not_say"),
                    labels = c("Male", "Female", "Non-Binary", as.character(NA))
                ),
            category == "Work Status" ~
                factor(
                    value,
                    levels = c(
                        "student",
                        "employed_under40",
                        "employed_over40",
                        "not_employed_yes_looking",
                        "not_employed_not_looking",
                        "retired",
                        "disabled",
                        "prefer_not_say"
                    ),
                    labels = c(
                        "Student",
                        "Part-time",
                        "Full-time",
                        "Looking",
                        as.character(NA),
                        "Retired",
                        "Disabled",
                        as.character(NA)
                    )
                ),
            category == "Party" ~
                factor(
                    value,
                    levels = c(
                        "democratic",
                        "republican",
                        "independent",
                        "prefer_not_say"
                    ),
                    labels = c(
                        "Democratic",
                        "Republican",
                        "Independent",
                        as.character(NA)
                    )
                ),
            category == "V2G Interest" ~
                factor(value, levels = c("yes", "no"), labels = c("Yes", "No")),
            category == "Pay for V2G Charger" ~
                factor(
                    value,
                    levels = c("yes", "no", "already_have", "NA"),
                    labels = c(
                        "Willing to",
                        "Don't Want",
                        "Already Have",
                        as.character(NA)
                    )
                ),
            TRUE ~ as.factor(value)
        )
    )

# 3.3 Plot the personal info summary A
personal_plot_a <- choice_data_personal_a %>%
    ggplot(aes(x = value)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(
        ~ fct_relevel(
            category,
            "Climate Awareness",
            "Gender",
            "Work Status",
            "Party",
            "V2G Interest",
            "Pay for V2G Charger"
        ),
        scales = "free_x",
        nrow = 3
    ) +
    labs(
        x = NULL,
        y = "Count",
        title = "Personal Info A - Major Info",
        subtitle = "Demographics Summary 3"
    ) +
    theme_bw(base_family = "Ubuntu")

personal_plot_a

ggsave(
    filename = file.path(processed_dir, "1_summary", "3_personal_a.png"),
    plot = personal_plot_a,
    width = 9,
    height = 9 / 1.618
)

# 4. Personal Info B - Ethnicity & Education
# 4.1 Pivot the data
choice_data_personal_b <- choice_data_wider %>%
    mutate(across(c(mc_ethnicity, mc_education), as.character)) %>%
    pivot_longer(
        cols = c(mc_ethnicity, mc_education),
        names_to = "category",
        values_to = "value"
    )

# 4.2 Reorder and relabel the values based on category
choice_data_personal_b <- choice_data_personal_b %>%
    mutate(
        category = case_when(
            category == "mc_ethnicity" ~ "Ethnicity",
            category == "mc_education" ~ "Education",
            TRUE ~ category
        ),
        value = case_when(
            category == "Ethnicity" ~
                factor(
                    value,
                    levels = c(
                        "asian",
                        "black",
                        "white",
                        "hispanic",
                        "native",
                        "pacific",
                        "other",
                        "prefer_not_say"
                    ),
                    labels = c(
                        "Asian",
                        "Black",
                        "White",
                        "Hispanic",
                        "Native American",
                        "Pacific Islander",
                        "Other",
                        "Other"
                    )
                ),
            category == "Education" ~
                factor(
                    value,
                    levels = c(
                        "no_hs",
                        "hs",
                        "college_some",
                        "vocational",
                        "degree_associate",
                        "degree_bs",
                        "degree_grad",
                        "other",
                        "prefer_not_say"
                    ),
                    labels = c(
                        "< High School",
                        "High School",
                        "Some College",
                        "Vocational",
                        "Associate Degree",
                        "Bachelor Degree",
                        "Graduate Degree",
                        "Other",
                        "Other"
                    )
                ),
            TRUE ~ as.factor(value)
        )
    )

# 4.3 Plot the personal info summary B
personal_plot_b <- choice_data_personal_b %>%
    ggplot(aes(x = value)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(
        ~ fct_relevel(category, "Ethnicity", "Education"),
        scales = "free_x",
        nrow = 2
    ) +
    labs(
        x = NULL,
        y = "Count",
        title = "Personal Info B - Ethnicity & Education",
        subtitle = "Demographics Summary 4"
    ) +
    theme_bw(base_family = "Ubuntu")

personal_plot_b

ggsave(
    filename = file.path(processed_dir, "1_summary", "4_personal_b.png"),
    plot = personal_plot_b,
    width = 9,
    height = 9 / 1.618
)

# 5. Personal Info C - Year of Birth
current_year <- 2024
choice_data_personal_c <- choice_data_wider %>%
    mutate(
        mc_yob = ifelse(mc_yob == "prefer_not_say", NA, mc_yob),
        mc_yob = as.numeric(mc_yob),
        age = current_year - mc_yob
    ) %>%
    mutate(
        age_group = case_when(
            age <= 30 ~ "<=30",
            age > 30 & age <= 40 ~ "31-40",
            age > 40 & age <= 50 ~ "41-50",
            age > 50 & age <= 60 ~ "51-60",
            age > 60 & age <= 70 ~ "61-70",
            age > 70 ~ ">70",
            TRUE ~ "NA"
        )
    ) %>%
    mutate(
        age_group = factor(
            age_group,
            levels = c("<=30", "31-40", "41-50", "51-60", "61-70", ">70", "NA")
        )
    )

personal_plot_c <- choice_data_personal_c %>%
    ggplot(aes(x = age_group)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Age Group",
        y = "Count",
        title = "Personal Info C - Age Group",
        subtitle = "Demographics Summary 5"
    ) +
    theme_bw(base_family = "Ubuntu")

personal_plot_c

ggsave(
    filename = file.path(processed_dir, "1_summary", "5_personal_c.png"),
    plot = personal_plot_c,
    width = 6,
    height = 6 / 1.618
)

# 6. Household Info A - Major Info
# 6.1 Pivot the data
choice_data_house_a <- choice_data_wider %>%
    mutate(across(
        c(mc_house_type, mc_house_owner, mc_household_size),
        as.character
    )) %>%
    pivot_longer(
        cols = c(mc_house_type, mc_house_owner, mc_household_size),
        names_to = "category",
        values_to = "value"
    )

# 6.2 Reorder and relabel the values based on category
choice_data_house_a <- choice_data_house_a %>%
    mutate(
        category = case_when(
            category == "mc_house_type" ~ "House Type",
            category == "mc_house_owner" ~ "House Ownership",
            category == "mc_household_size" ~ "Household Size",
            TRUE ~ category
        ),
        value = case_when(
            category == "House Type" ~
                factor(
                    value,
                    levels = c(
                        "mobile",
                        "apartment",
                        "townhome",
                        "condo",
                        "detached",
                        "other",
                        "prefer_not_say"
                    ),
                    labels = c(
                        "Mobile",
                        "Apt",
                        "Townhome",
                        "Condo",
                        "Detached",
                        "Other",
                        "Other"
                    )
                ),
            category == "House Ownership" ~
                factor(
                    value,
                    levels = c("own", "rent", "prefer_not_say"),
                    labels = c("Own", "Rent", "NA")
                ),
            category == "Household Size" ~
                factor(
                    value,
                    levels = c(
                        "1",
                        "2",
                        "3",
                        "4",
                        "5_or_more",
                        "prefer_not_say"
                    ),
                    labels = c("1", "2", "3", "4", ">4", as.character(NA))
                ),
            TRUE ~ as.factor(value)
        )
    )

# 6.3 Plot the household info summary A
house_plot_a <- choice_data_house_a %>%
    ggplot(aes(x = value)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(
        ~ fct_relevel(
            category,
            "House Type",
            "House Ownership",
            "Household Size"
        ),
        scales = "free_x",
        nrow = 2
    ) +
    labs(
        x = NULL,
        y = "Count",
        title = "Household Info A - Major Info",
        subtitle = "Demographics Summary 6"
    ) +
    theme_bw(base_family = "Ubuntu")

house_plot_a

ggsave(
    filename = file.path(processed_dir, "1_summary", "6_house_a.png"),
    plot = house_plot_a,
    width = 8,
    height = 8 / 1.618
)

# 7. Household Info B - Income
house_plot_b <- choice_data_wider %>%
    mutate(
        mc_income = factor(
            mc_income,
            levels = c(
                "prefer_not_say",
                "below_25",
                "25_to_35",
                "35_to_50",
                "50_to_75",
                "75_to_100",
                "100_to_150",
                "150_to_200",
                "200_to_250",
                "250_to_300",
                "300_to_400",
                "over_400"
            ),
            labels = c(
                as.character(NA),
                "<25k",
                "25-35k",
                "35-50k",
                "50-75k",
                "75-100k",
                "100-150k",
                "150-200k",
                "200-250k",
                "250-300k",
                "300-400k",
                ">400k"
            )
        )
    ) %>%
    ggplot(aes(x = mc_income)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        hjust = -0.5,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Annual Household Income",
        y = "Count",
        title = "Household Info B - Income",
        subtitle = "Demographics Summary 7"
    ) +
    coord_flip() +
    theme_bw(base_family = "Ubuntu")

house_plot_b

ggsave(
    filename = file.path(processed_dir, "1_summary", "7_house_b.png"),
    plot = house_plot_b,
    width = 7,
    height = 7 / 1.618
)

# 8. BEV model ownership ranking
top_bev <- choice_data_wider %>%
    mutate(
        user_bev = case_when(
            is_bev_1 == "yes" ~ user_car_1,
            is_bev_1 == "no" & is_bev_2 == "yes" ~ user_car_2,
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(user_bev)) %>%
    count(user_bev) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    mutate(user_bev = str_to_title(gsub("_", " ", user_bev)))

top_bev_plot <- top_bev %>%
    ggplot(aes(x = reorder(user_bev, n), y = n)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_text(aes(label = n), hjust = -0.3, size = 4, family = "Ubuntu") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        title = "Top 10 BEV Models",
        subtitle = "Demographics Summary 8",
        x = "Car Make & Model",
        y = "Count"
    ) +
    theme_bw(base_family = "Ubuntu") +
    theme(axis.text = element_text(size = 11))

top_bev_plot

ggsave(
    filename = file.path(processed_dir, "1_summary", "8_top_bev.png"),
    plot = top_bev_plot,
    width = 7,
    height = 7 / 1.618
)

# 9. Tesla Ownership & Political Affiliation
# 9.1 Tesla Ownership
tesla_ownership <- choice_data_wider %>%
    mutate(
        owns_tesla = if_else(
            grepl("^tesla", user_car_1, ignore.case = TRUE) |
                grepl("^tesla", user_car_2, ignore.case = TRUE),
            "yes",
            "no"
        )
    )

tesla_owner_plot <- tesla_ownership %>%
    mutate(
        owns_tesla = factor(
            owns_tesla,
            levels = c("yes", "no"),
            labels = c("Yes", "No")
        )
    ) %>%
    ggplot(aes(x = owns_tesla)) +
    geom_bar(width = 0.6, stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Owns Tesla",
        y = "Count",
        title = "Tesla Ownership",
        subtitle = "Demographics Summary 9A"
    ) +
    theme_bw(base_family = "Ubuntu")

tesla_owner_plot

ggsave(
    filename = file.path(processed_dir, "1_summary", "9a_tesla_owner.png"),
    plot = tesla_owner_plot,
    width = 6,
    height = 6 / 1.618
)

# 9.2 Tesla Political Affiliation
tesla_party <- choice_data_wider %>%
    mutate(
        owns_tesla = if_else(
            grepl("^tesla", user_car_1, ignore.case = TRUE) |
                grepl("^tesla", user_car_2, ignore.case = TRUE),
            "yes",
            "no"
        )
    ) %>%
    mutate(
        owns_tesla = factor(
            owns_tesla,
            levels = c("yes", "no"),
            labels = c("Yes", "No")
        ),
        mc_party = factor(
            mc_party,
            levels = c(
                "democratic",
                "republican",
                "independent",
                "prefer_not_say"
            ),
            labels = c(
                "Democratic",
                "Republican",
                "Independent",
                "Prefer not to say"
            )
        )
    ) %>%
    mutate(
        category = case_when(
            owns_tesla == "Yes" ~ "Tesla Owner Party",
            owns_tesla == "No" ~ "Non-Tesla Owner Party",
            TRUE ~ "Owns Tesla"
        )
    )
tesla_party_plot <- tesla_party %>%
    ggplot(aes(x = mc_party)) +
    geom_bar(width = 0.6, position = "dodge", stat = "count") +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.3,
        size = 3,
        family = "Ubuntu"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Political Affiliation",
        y = "Count",
        title = "Tesla Political Affiliation",
        subtitle = "Demographics Summary 9B"
    ) +
    theme_bw(base_family = "Ubuntu") +
    facet_wrap(
        ~ fct_relevel(category, "Tesla Owner Party", "Non-Tesla Owner Party"),
        scales = "free_x",
        nrow = 2
    )

tesla_party_plot

ggsave(
    filename = file.path(processed_dir, "1_summary", "9b_tesla_party.png"),
    plot = tesla_party_plot,
    width = 7,
    height = 7 / 1.618
)
