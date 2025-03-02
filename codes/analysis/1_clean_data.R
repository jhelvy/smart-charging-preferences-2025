# This script cannot run by itself.
# It has to be called by the R scripts in the parent folder.

# 1. Read survey data
# 1.1 Read social data
carSocial0a <- read_csv(file.path(raw_dir, "social", "carSocial0a.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial0b <- read_csv(file.path(raw_dir, "social", "carSocial0b.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial1 <- read_csv(file.path(raw_dir, "social", "carSocial1.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial2 <- read_csv(file.path(raw_dir, "social", "carSocial2.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial3 <- read_csv(file.path(raw_dir, "social", "carSocial3.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial4a <- read_csv(file.path(raw_dir, "social", "carSocial4a.csv")) %>% 
    filter_dates %>% mutate(source = "social")
carSocial4b <- read_csv(file.path(raw_dir, "social", "carSocial4b.csv")) %>% 
    filter_dates %>% mutate(source = "social")

# 1.2 Read dynata data
carDynata0a <- read_csv(file.path(raw_dir, "dynata", "carDynata0a.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata0b <- read_csv(file.path(raw_dir, "dynata", "carDynata0b.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata1 <- read_csv(file.path(raw_dir, "dynata", "carDynata1.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata2 <- read_csv(file.path(raw_dir, "dynata", "carDynata2.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata3 <- read_csv(file.path(raw_dir, "dynata", "carDynata3.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata4a <- read_csv(file.path(raw_dir, "dynata", "carDynata4a.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")
carDynata4b <- read_csv(file.path(raw_dir, "dynata", "carDynata4b.csv")) %>% 
    filter_dates %>% mutate(source = "dynata")

# 1.3 Bind rows
safe_bind_rows <- function(df_social, df_dynata) {
    # Function to reorder columns and add new ones
    reorder_and_add_columns <- function(df, ref_columns, new_columns) {
        existing_cols <- intersect(ref_columns, names(df))
        additional_cols <- setdiff(names(df), ref_columns)
        df[, c(existing_cols, new_columns, additional_cols)]
    }
    
    # Convert common columns to the same type
    common_cols <- intersect(names(df_social), names(df_dynata))
    
    for (col in common_cols) {
        if (typeof(df_social[[col]]) != typeof(df_dynata[[col]])) {
            # Convert to character if any of the columns is character
            if (is.character(df_social[[col]]) || is.character(df_dynata[[col]])) {
                df_social[[col]] <- as.character(df_social[[col]])
                df_dynata[[col]] <- as.character(df_dynata[[col]])
            }
            # Convert to numeric if both are numeric but of different types
            else if (is.numeric(df_social[[col]]) && is.numeric(df_dynata[[col]])) {
                df_social[[col]] <- as.numeric(df_social[[col]])
                df_dynata[[col]] <- as.numeric(df_dynata[[col]])
            }
        }
    }
    
    if (nrow(df_social) == 0 && nrow(df_dynata) == 0) {
        # If both dataframes are empty, return df_social
        return(df_social)
    } else if (nrow(df_social) == 0) {
        # If df_social is empty, reorder df_dynata based on df_social's columns
        return(reorder_and_add_columns(df_dynata, names(df_social), character(0)))
    } else if (nrow(df_dynata) == 0) {
        # If df_dynata is empty, return df_social as is
        return(df_social)
    } else {
        # If both dataframes have rows, bind them and then reorder
        combined_df <- bind_rows(df_social, df_dynata)
        return(reorder_and_add_columns(combined_df, names(df_social), character(0)))
    }
}

carData0a <- safe_bind_rows(carSocial0a, carDynata0a)
carData0b <- safe_bind_rows(carSocial0b, carDynata0b)
carData1  <- safe_bind_rows(carSocial1, carDynata1)
carData2  <- safe_bind_rows(carSocial2, carDynata2)
carData3  <- safe_bind_rows(carSocial3, carDynata3)
carData4a <- safe_bind_rows(carSocial4a, carDynata4a)
carData4b <- safe_bind_rows(carSocial4b, carDynata4b)

# 2. Format and combine the dataframes
# 2.1 Format carData0a
carData0a <- carData0a %>% 
    filter(!is.na(ended)) %>%
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_0a = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, source, created, any_of("psid"), time_sec_0a, ip_address,
           browser, zip_code, user_car_1, mc_car_year_1, is_bev_1,
           user_car_2, mc_car_year_2, is_bev_2)

# 2.2 Format carData0b
carData0b <- carData0b %>% 
    filter(!is.na(ended)) %>%
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_0b = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_0b,
           user_car_planned = user_bev,
           mc_car_year_planned = mc_bev_year)

# 2.3 Format carData1
carData1 <- carData1 %>% 
    filter(!is.na(ended)) %>%
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_1 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_1, user_bev:mc_work_hours)

# 2.4 Format carData2
carData2 <- carData2 %>% 
    filter(!is.na(ended)) %>%
    mutate(
        time_2_created = ymd_hms(created),
        time_2_ended =  ymd_hms(ended),
        time_sec_2 = as.numeric(time_2_ended - time_2_created,
                                units = "secs")) %>%
    # Select important columns
    select(session, time_sec_2, time_2_created, time_2_ended, smc_respID, starts_with("mc"), smc_all_same)

# 2.5 Format carData3
carData3 <- carData3 %>% 
    filter(!is.na(ended)) %>%
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_3 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_3, mc_climate:mc_income)

# 2.6 Format carData4a
carData4a <- carData4a %>% 
    filter(!is.na(ended)) %>%
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_4a = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_4a, mc_v2g:txt_feedback_1)

# 2.7 Format carData4b
carData4b <- carData4b %>% 
    filter(!is.na(ended)) %>%
    mutate(
        time_4b_created = ymd_hms(created),
        time_4b_ended =  ymd_hms(ended),
        time_sec_4b = as.numeric(time_4b_ended - time_4b_created,
                                 units = "secs")) %>%
    # Select important columns
    select(session, time_sec_4b, time_4b_created, time_4b_ended, v2g_respID, starts_with("mc"), v2g_all_same, txt_feedback_2)

# 3. Create wider df
# 3.1 Combine all parts together using the session variable
choice_data_wider <- carData0a %>% 
    left_join(carData0b, by = "session") %>% 
    left_join(carData1,  by = "session") %>% 
    left_join(carData2,  by = "session") %>% 
    left_join(carData3,  by = "session") %>% 
    left_join(carData4a, by = "session") %>% 
    left_join(carData4b, by = "session") %>% 
    select(-session) %>% 
    filter(has_bev == "yes" | plan_bev == "yes") %>% 
    filter(!is.na(mc_v2g))

# Filter out invalid results, including speeding, all same, and no BEV
choice_data_wider <- choice_data_wider %>%
    mutate(
        smc_speeding = case_when(
            source == "social" & time_sec_2 < 200 ~ "yes",
            source == "dynata" & time_sec_2 < 100 ~ "yes",
            TRUE ~ "no"
        ),
        v2g_speeding = case_when(
            source == "social" & time_sec_4b < 150 ~ "yes",
            source == "dynata" & time_sec_4b < 60 ~ "yes",
            TRUE ~ "no"
        )
    ) %>%
    filter(smc_speeding == "no") %>% 
    filter(smc_all_same == "no") %>% 
    filter(has_bev == "yes")

choice_data_wider_smc <- choice_data_wider

choice_data_wider_v2g <- choice_data_wider %>% 
    filter(!is.na(mc_v2g_6)) %>% 
    filter(v2g_all_same == "no") %>% 
    filter(v2g_speeding == "no")

# Tagging data without filtering
# choice_data_wider <- choice_data_wider %>%
#     mutate(across(c(smc_speeding, v2g_speeding, smc_all_same, v2g_all_same), 
#                   ~ replace_na(., "no"))) %>%
#     mutate(is_speeding = if_else(smc_speeding == "yes" | v2g_speeding == "yes", "yes", "no"),
#            is_all_same = if_else(smc_all_same == "yes" | v2g_all_same == "yes", "yes", "no"),
#            filtering = if_else(is_speeding == "yes" | is_all_same == "yes", "filtered", "kept")) %>%
#     arrange(factor(filtering, levels = c("kept", "filtered"))) %>% 
#     select(source, filtering, is_speeding, is_all_same, psid, everything())

# 3.2 Save the wider df into csv and RData
write_csv(choice_data_wider, file.path(processed_dir, "choice_data_wider.csv"))
save(choice_data_wider, file = file.path(processed_dir, "choice_data_wider.RData"))

write_csv(choice_data_wider_smc, file.path(processed_dir, "choice_data_wider_smc.csv"))
save(choice_data_wider_smc, file = file.path(processed_dir, "choice_data_wider_smc.RData"))

write_csv(choice_data_wider_v2g, file.path(processed_dir, "choice_data_wider_v2g.csv"))
save(choice_data_wider_v2g, file = file.path(processed_dir, "choice_data_wider_v2g.RData"))

# 4. Create longer df of smc data
# 4.1 Convert the data to longer format
smc_data <- choice_data_wider_smc %>% 
    pivot_longer(
        cols = mc_smc_1:mc_smc_6,
        names_to = "qID",
        values_to = "choice") %>% 
    # Convert the qID variable to a number
    mutate(qID = parse_number(qID))

# 4.2 Read in smc questions and join it to smc_data
smc_survey <- read_csv("https://raw.githubusercontent.com/pingfan-hu/My-Resources/main/bev/smc_questions.csv") %>% 
    rename(override_days = override) %>% 
    mutate(override_flag = ifelse(override_days == 0, 0, 1))

smc_data <- smc_data %>% 
    rename(respID = smc_respID) %>% 
    left_join(smc_survey, by = c("respID", "qID"), relationship = "many-to-many")

# 4.3 Convert choice column to 1 or 0 based on if the alternative was chosen
smc_data <- smc_data %>% 
    mutate(
        mc_car_year_1 = ifelse(mc_car_year_1 == "idk", NA, mc_car_year_1),
        choice = case_when(
            str_detect(choice, "^option_") ~
                as.numeric(str_replace(choice, "option_", "")),
            choice == "not_interested" ~ 3,
            TRUE ~ as.numeric(choice)
        ),
        choice = ifelse(choice == altID, 1, 0)
    ) %>% 
    select(source, respID, qID, profileID, altID, obsID, has_bev,
           enrollment_cash, monthly_cash, starts_with("override"),
           minimum_threshold, guaranteed_threshold, choice, no_choice,
           starts_with("mc_"), -starts_with("mc_v2g_"))

# 4.4 Create new values for respID & obsID
smc_n_respondents <- nrow(choice_data_wider_smc)
smc_n_alts <- max(smc_survey$altID)
smc_n_questions <- max(smc_survey$qID)
smc_data$respID <- rep(seq(smc_n_respondents), each = smc_n_alts*smc_n_questions)
smc_data$obsID <- rep(seq(smc_n_respondents*smc_n_questions), each = smc_n_alts)

# 4.5 Clean up names for smc_data
smc_data <- clean_names(smc_data)

# 4.6 Save cleaned data for modeling
write_csv(smc_data, file.path(processed_dir, "smc_data.csv"))
save(smc_data, file = file.path(processed_dir, "smc_data.RData"))

# 5. Create longer df of v2g data
# 5.1 Convert the data to longer format
v2g_data <- choice_data_wider_v2g %>% 
    pivot_longer(
        cols = mc_v2g_1:mc_v2g_6,
        names_to = "qID",
        values_to = "choice") %>% 
    # Convert the qID variable to a number
    mutate(qID = as.numeric(gsub("mc_v2g_", "", qID)))

# 5.2 Read in v2g questions and join it to v2g_data
v2g_survey <- read_csv("https://raw.githubusercontent.com/pingfan-hu/My-Resources/main/bev/v2g_questions.csv")

v2g_data <- v2g_data %>% 
    rename(respID = v2g_respID) %>% 
    left_join(v2g_survey, by = c("respID", "qID"), relationship = "many-to-many")

# 5.3 Convert choice column to 1 or 0 based on if the alternative was chosen 
v2g_data <- v2g_data %>% 
    mutate(
        mc_car_year_1 = ifelse(mc_car_year_1 == "idk", NA, mc_car_year_1),
        choice = case_when(
            str_detect(choice, "^option_") ~
                as.numeric(str_replace(choice, "option_", "")),
            choice == "not_interested" ~ 3,
            TRUE ~ as.numeric(choice)
        ),
        choice = ifelse(choice == altID, 1, 0)
    ) %>% 
    select(source, respID, qID, profileID, altID, obsID, has_bev,
           enrollment_cash, occurrence_cash, starts_with("monthly_occurrence"),
           lower_bound, guaranteed_threshold, choice, no_choice,
           starts_with("mc_"), -starts_with("mc_smc_"))

# 5.4 Create new values for respID & obsID
v2g_n_respondents <- nrow(choice_data_wider_v2g)
v2g_n_alts <- max(v2g_survey$altID)
v2g_n_questions <- max(v2g_survey$qID)
v2g_data$respID <- rep(seq(v2g_n_respondents), each = v2g_n_alts*v2g_n_questions)
v2g_data$obsID <- rep(seq(v2g_n_respondents*v2g_n_questions), each = v2g_n_alts)

# 5.5 Clean up names for v2g_data
v2g_data <- clean_names(v2g_data)

# 5.6 Save cleaned data for modeling
write_csv(v2g_data, file.path(processed_dir, "v2g_data.csv"))
save(v2g_data, file = file.path(processed_dir, "v2g_data.RData"))