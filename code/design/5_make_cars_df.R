library(tidyverse)
library(rvest)
library(janitor)
library(here)
library(snakecase)

# Function to scrape a single page
get_page_table <- function(url) {
    page <- read_html(url)
    table <- page %>%
        html_nodes("table") %>%
        html_table(header = TRUE, trim = TRUE)
    return(table)
}

# Empty list to store data from each page
car_data <- list()
base_url <- "https://carsheet.io/all-cars/"
entries_per_page <- 100
total_entries <- 70000
total_pages <- ceiling(total_entries / entries_per_page)

for (i in 0:(total_pages - 1)) {
    # Construct the URL for each page
    if (i == 0) {
        url <- base_url # First page
    } else {
        start_entry <- i * entries_per_page
        end_entry <- start_entry + entries_per_page
        url <- paste0(base_url, start_entry, "-", end_entry, "/")
    }

    cat("Scraping:", url, "\n")

    tryCatch({
        page_data <- get_page_table(url)
        if (length(page_data) >= 1) {
            car_data[[length(car_data) + 1]] <- page_data[[1]] %>%
                clean_names()
        } else {
            cat("No table found on page", i + 1, "\n")
        }
    }, error = function(e) {
        cat("Error scraping page", i + 1, ": ", e$message, "\n")
    })
}

# Combine all tables into df
car_data <- lapply(car_data, function(df) {
    if (nrow(df) == 0) {
        df <- tibble(
            make = character(),
            model = character(),
            engine_aspiration = character()
        )
    }
    return(df)
})
car_df <- bind_rows(car_data) %>%
    select(make, model, engine_aspiration) %>%
    distinct() %>%
    arrange(tolower(make), tolower(model)) %>%
    mutate(
        is_bev = if_else(engine_aspiration == "Electric Motor", "Yes", "No"),
        make_snake = to_snake_case(make),
        model_snake = to_snake_case(model)
    ) %>%
    select(make_snake, make, model_snake, model, everything())

# Show only makes and models
car_model_df <- car_df %>% 
    select(make_snake, make, model_snake, model) %>% 
    distinct() %>% 
    arrange(tolower(make), tolower(model))

# Show only distinctive makes
car_make_df <- car_df %>% 
    select(make_snake, make) %>% 
    distinct() %>% 
    arrange(tolower(make))

##########################################
# Don't execute the scripts below.       #
# The csv files have been hand-modified. #
##########################################

# Write into csv
# write_csv(car_df, here("project", "data", "cars.csv"))
# write_csv(car_make_df, here("project", "data", "car_makes.csv"))
# write_csv(car_model_df, here("project", "data", "car_models.csv"))