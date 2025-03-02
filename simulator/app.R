library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(logitr)
library(markdown)

# Load the models
load(file.path("model", "smc_mnl_model.RData"))
load(file.path("model", "v2g_mnl_model.RData"))

# Helper function to check if a value is numeric
is_valid_numeric <- function(x, positive = FALSE, max_value = NULL, field_name = "") {
    # Basic numeric validation
    if (!(!is.null(x) && !is.na(x) && is.numeric(x))) {
        return(list(valid = FALSE, message = paste(field_name, "must be a valid number")))
    }
    
    # Non-negative check (x >= 0)
    if (x < 0) {
        return(list(valid = FALSE, message = paste(field_name, "must be non-negative")))
    }
    
    # Positive check (x > 0) if required
    if (positive && x <= 0) {
        return(list(valid = FALSE, message = paste(field_name, "must be positive")))
    }
    
    # Max value check if provided
    if (!is.null(max_value) && x > max_value) {
        return(list(valid = FALSE, message = paste(field_name, "cannot exceed", max_value)))
    }
    
    return(list(valid = TRUE, message = NULL))
}

ui <- tagList(
    useShinyjs(),
    
    # Disable page warning
    tags$head(
        tags$script("window.onbeforeunload = null;")
    ),
    
    # Custom CSS
    tags$head(
        tags$style(HTML("
            /* Navbar Styles */
            .navbar-default {
                background-color: #018081 !important;
                border-color: #018081 !important;
            }
            
            .navbar-default .navbar-nav > li > a:hover,
            .navbar-default .navbar-nav > li > a:focus {
                background-color: #016f70 !important;
                color: white !important;
            }
            
            .navbar-default .navbar-nav > .active > a,
            .navbar-default .navbar-nav > .active > a:focus,
            .navbar-default .navbar-nav > .active > a:hover {
                background-color: #016f70 !important;
                color: white !important;
            }
            
            .navbar-default .navbar-nav > li > a {
                color: white !important;
            }
            
            /* Other Styles */
            .btn-primary {
                background-color: #018081 !important;
                border-color: #018081 !important;
            }
            
            .btn-primary:hover {
                background-color: #016f70 !important;
                border-color: #016f70 !important;
            }
            
            .btn-custom-reset {
                background-color: #DA5917 !important;
                border-color: #DA5917 !important;
                color: white !important;
            }
    
            .btn-custom-reset:hover {
                background-color: #C24F14 !important;
                border-color: #C24F14 !important;
            }
            
            a {
                color: #018081;
            }
            
            a:hover {
                color: #016f70;
                text-decoration: none;
            }
            
            .progress-bar {
                background-color: #018081 !important;
            }
            
            .progress {
                background-color: #E5E5E5 !important;
            }
            
            /* Fonts */
            body {
                font-size: 16px !important;
                background-color: #F8F7F0 !important;
            }
            
            .h4, h4 {
                font-size: 18px;
            }
            
            .table {
                font-size: 16px !important;
            }
        "))
    ),
    navbarPage(
        title = "Smart Charging Enrollment Simulator",
        theme = shinytheme("united"),
        
        # About Page
        tabPanel(
            title = "About",
            icon = icon("home"),
            includeMarkdown("about.qmd")
        ),
        # SMC Page
        tabPanel(
            title = "SMC (Supplier-Managed Charging)",
            icon = icon("bolt"),
            sidebarLayout(
                sidebarPanel(
                    h3("SMC Attributes:"),
                    numericInput("smc_enrollment_cash", "Enrollment Cash ($)", 
                                 value = 0, min = 0, max = 1000, step = 1),
                    numericInput("smc_monthly_cash", "Monthly Cash ($)", 
                                 value = 0, min = 0, max = 100, step = 0.1),
                    numericInput("smc_override_days", "Override Allowance per Month", 
                                 value = 0, min = 0, max = 31, step = 1),
                    sliderInput("smc_minimum_threshold", "Minimum Threshold (%)", 
                                value = 20, min = 0, max = 100, step = 5),
                    sliderInput("smc_guaranteed_threshold", "Guaranteed Threshold (%)", 
                                value = 60, min = 0, max = 100, step = 5),
                    
                    div(style = "display: flex; justify-content: flex-end; margin-top: 20px;",
                        actionButton("smc_reset", "Reset",
                                     class = "btn-custom-reset")
                    )
                ),
                
                mainPanel(
                    h3("Predicted SMC Enrollment Probability:"),
                    div(style = "margin: 10px 0;",
                        div(class = "progress",
                            div(id = "smc-progress",
                                class = "progress-bar",
                                role = "progressbar",
                                style = "width: 0%"
                            )
                        )
                    ),
                    div(style = "margin: 10px 0; display: flex; align-items: flex-end; gap: 15px;",
                        div(style = "font-size: 40px; font-weight: bold;",
                            textOutput("smc_enrollment_prob")
                        ),
                        div(style = "color: red; font-size: 16px; margin-bottom: 0.8rem;",
                            textOutput("smc_warning")
                        )
                    ),
                    h3("About SMC:"),
                    HTML("
        <ul>
            <li>SMC (Supplier-Managed Charging) allows the utility to monitor, manage, and restrict BEV charging to optimize energy flow during night charging at home.</li>
            <li>By participating in SMC, your BEV will be mostly charged during off-peak periods.</li>
        </ul>
    "),
                    h3("SMC Attributes Explained:"),
                    HTML("
        <div class='table-responsive'>
            <table class='table table-bordered'>
                <thead>
                    <tr>
                        <th style='width: 25%'>Attribute</th>
                        <th>Description</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td><strong>Enrollment Cash</strong></td>
                        <td>The one-time payment you'll receive if you stay for at least 3 months.</td>
                    </tr>
                    <tr>
                        <td><strong>Monthly Cash</strong></td>
                        <td>The recurring monthly payment you'll receive if you don't exceed override allowance.</td>
                    </tr>
                    <tr>
                        <td><strong>Override Allowance</strong></td>
                        <td>The monthly frequency of override to normal charging, effective for 24hrs. If you exceed the limit, no monthly cash for this month.</td>
                    </tr>
                    <tr>
                        <td><strong>Minimum Threshold</strong></td>
                        <td>SMC won't be triggered below this threshold. In the survey it's converted to miles.</td>
                    </tr>
                    <tr>
                        <td><strong>Guaranteed Threshold</strong></td>
                        <td>SMC will give you this much of range by the morning (8 hrs' charging). In the survey it's converted to miles.</td>
                    </tr>
                </tbody>
            </table>
        </div>
    ")
                )
            )
        ),
        
        # V2G Page
        tabPanel(
            title = HTML('V2G (Vehicle-to-Grid)</a></li><li><a href="https://github.com/jhelvy/smart-charging-preferences-2025" target="_blank"><i class="fa fa-github fa-fw"></i>'),
            icon = icon("charging-station"),
            sidebarLayout(
                sidebarPanel(
                    h3("V2G Attributes:"),
                    numericInput("v2g_enrollment_cash", "Enrollment Cash ($)", 
                                 value = 0, min = 0, max = 1000, step = 1),
                    numericInput("v2g_occurrence_cash", "Occurrence Cash ($)", 
                                 value = 2, min = 0, max = 100, step = 0.1),
                    numericInput("v2g_monthly_occurrence", "Monthly Occurrence", 
                                 value = 1, min = 0, max = 31, step = 1),
                    sliderInput("v2g_lower_bound", "Lower Bound (%)", 
                                value = 20, min = 0, max = 100, step = 5),
                    sliderInput("v2g_guaranteed_threshold", "Guaranteed Threshold (%)", 
                                value = 60, min = 0, max = 100, step = 5),
                    
                    div(style = "display: flex; justify-content: flex-end; margin-top: 20px;",
                        actionButton("v2g_reset", "Reset",
                                     class = "btn-custom-reset")
                    )
                ),
                
                mainPanel(
                    h3("Predicted V2G Enrollment Probability:"),
                    div(style = "margin: 10px 0;",
                        div(class = "progress",
                            div(id = "v2g-progress",
                                class = "progress-bar",
                                role = "progressbar",
                                style = "width: 0%"
                            )
                        )
                    ),
                    div(style = "margin: 10px 0; display: flex; align-items: flex-end; gap: 15px;",
                        div(style = "font-size: 40px; font-weight: bold;",
                            textOutput("v2g_enrollment_prob")
                        ),
                        div(style = "color: red; font-size: 16px; margin-bottom: 0.8rem;",
                            textOutput("v2g_warning")
                        )
                    ),
                    h3("About V2G:"),
                    HTML("
        <ul>
            <li>V2G (Vehicle-to-Grid) lets your vehicle supply the grid as external power during peak times, reducing the need for additional battery storage and benefiting the environment.</li>
            <li>If you enroll, your utility may purchase electricity from your car as needed. You'll receive prior notification, and your vehicle will be recharged to a guaranteed range in the end.</li>
        </ul>
    "),
                    h3("V2G Attributes Explained:"),
                    HTML("
        <div class='table-responsive'>
            <table class='table table-bordered'>
                <thead>
                    <tr>
                        <th style='width: 25%'>Attribute</th>
                        <th>Description</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td><strong>Enrollment Cash</strong></td>
                        <td>The one-time payment you'll receive if you stay for at least 3 months.</td>
                    </tr>
                    <tr>
                        <td><strong>Occurrence Cash</strong></td>
                        <td>The amount you'll earn for each occurrence of V2G.</td>
                    </tr>
                    <tr>
                        <td><strong>Monthly Occurrence</strong></td>
                        <td>The monthly occurrence of V2G.</td>
                    </tr>
                    <tr>
                        <td><strong>Lower Bound</strong></td>
                        <td>V2G won't drain your battery below this percentage. In the survey it's converted to miles.</td>
                    </tr>
                    <tr>
                        <td><strong>Guaranteed Threshold</strong></td>
                        <td>V2G will charge your battery back to this percentage with 8 hrs' charging. In the survey it's converted to miles.</td>
                    </tr>
                </tbody>
            </table>
        </div>
    ")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    # Create reactive values to track input changes
    smc_inputs <- reactive({
        list(
            enrollment_cash = input$smc_enrollment_cash,
            monthly_cash = input$smc_monthly_cash,
            override_days = input$smc_override_days,
            minimum_threshold = input$smc_minimum_threshold,
            guaranteed_threshold = input$smc_guaranteed_threshold
        )
    })
    
    v2g_inputs <- reactive({
        list(
            enrollment_cash = input$v2g_enrollment_cash,
            occurrence_cash = input$v2g_occurrence_cash,
            monthly_occurrence = input$v2g_monthly_occurrence,
            lower_bound = input$v2g_lower_bound,
            guaranteed_threshold = input$v2g_guaranteed_threshold
        )
    })
    
    # Create debounced versions of the input changes
    smc_inputs_debounced <- debounce(smc_inputs, 500)
    v2g_inputs_debounced <- debounce(v2g_inputs, 500)
    
    # SMC Reset handler
    observeEvent(input$smc_reset, {
        updateNumericInput(session, "smc_enrollment_cash", value = 50)
        updateNumericInput(session, "smc_monthly_cash", value = 2)
        updateNumericInput(session, "smc_override_days", value = 1)
        updateSliderInput(session, "smc_minimum_threshold", value = 20)
        updateSliderInput(session, "smc_guaranteed_threshold", value = 60)
    })
    
    # V2G Reset handler
    observeEvent(input$v2g_reset, {
        updateNumericInput(session, "v2g_enrollment_cash", value = 50)
        updateNumericInput(session, "v2g_occurrence_cash", value = 2)
        updateNumericInput(session, "v2g_monthly_occurrence", value = 1)
        updateSliderInput(session, "v2g_lower_bound", value = 20)
        updateSliderInput(session, "v2g_guaranteed_threshold", value = 60)
    })
    
    # SMC prediction
    smc_result <- reactive({
        # Trigger on input changes or reset
        smc_inputs_debounced()
        
        # Input validation
        validations <- list(
            is_valid_numeric(input$smc_enrollment_cash, max_value = 1000, field_name = "Enrollment Cash"),
            is_valid_numeric(input$smc_monthly_cash, max_value = 100, field_name = "Monthly Cash"),
            is_valid_numeric(input$smc_override_days, max_value = 31, field_name = "Override Days"),
            is_valid_numeric(input$smc_minimum_threshold, field_name = "Minimum Threshold"),
            is_valid_numeric(input$smc_guaranteed_threshold, field_name = "Guaranteed Threshold")
        )
        
        # Check for validation errors
        invalid_inputs <- Filter(function(x) !x$valid, validations)
        if (length(invalid_inputs) > 0) {
            return(list(prob = NA, warning = paste(sapply(invalid_inputs, function(x) x$message), collapse = "\n")))
        }
        
        if (input$smc_minimum_threshold > input$smc_guaranteed_threshold) {
            return(list(prob = NA, warning = "Minimum threshold cannot be greater than guaranteed threshold"))
        }
        
        override_flag_value <- ifelse(input$smc_override_days > 0, 1, 0)
        
        newdata <- data.frame(
            obs_id = c(1, 1),
            alt_id = c(1, 2),
            enrollment_cash = c(input$smc_enrollment_cash, 0),
            monthly_cash = c(input$smc_monthly_cash, 0),
            override_days = c(input$smc_override_days, 0),
            override_flag = c(override_flag_value, 0),
            minimum_threshold = c(input$smc_minimum_threshold, 0),
            guaranteed_threshold = c(input$smc_guaranteed_threshold, 0),
            no_choice = c(0, 1)
        )
        
        pred <- predict(
            smc_mnl_model,
            newdata = newdata,
            obsID = "obs_id",
            level = 0.95,
            interval = "confidence",
            returnData = TRUE
        )
        
        list(prob = pred$predicted_prob[1], warning = NULL)
    })
    
    # V2G prediction
    v2g_result <- reactive({
        # Trigger on input changes or reset
        v2g_inputs_debounced()
        
        # Input validation
        validations <- list(
            is_valid_numeric(input$v2g_enrollment_cash, max_value = 1000, field_name = "Enrollment Cash"),
            is_valid_numeric(input$v2g_occurrence_cash, max_value = 100, field_name = "Occurrence Cash"),
            is_valid_numeric(input$v2g_monthly_occurrence, positive = TRUE, max_value = 31, field_name = "Monthly Occurrence"),
            is_valid_numeric(input$v2g_lower_bound, field_name = "Lower Bound"),
            is_valid_numeric(input$v2g_guaranteed_threshold, field_name = "Guaranteed Threshold")
        )
        
        # Check for validation errors
        invalid_inputs <- Filter(function(x) !x$valid, validations)
        if (length(invalid_inputs) > 0) {
            return(list(prob = NA, warning = paste(sapply(invalid_inputs, function(x) x$message), collapse = "\n")))
        }
        
        if (input$v2g_lower_bound > input$v2g_guaranteed_threshold) {
            return(list(prob = NA, warning = "Lower bound cannot be greater than guaranteed threshold"))
        }
        
        newdata <- data.frame(
            obs_id = c(1, 1),
            alt_id = c(1, 2),
            enrollment_cash = c(input$v2g_enrollment_cash, 0),
            occurrence_cash = c(input$v2g_occurrence_cash, 0),
            monthly_occurrence = c(input$v2g_monthly_occurrence, 0),
            lower_bound = c(input$v2g_lower_bound, 0),
            guaranteed_threshold = c(input$v2g_guaranteed_threshold, 0),
            no_choice = c(0, 1)
        )
        
        pred <- predict(
            v2g_mnl_model,
            newdata = newdata,
            obsID = "obs_id",
            level = 0.95,
            interval = "confidence",
            returnData = TRUE
        )
        
        list(prob = pred$predicted_prob[1], warning = NULL)
    })
    
    # Update SMC progress bar and text
    observe({
        result <- smc_result()
        if (!is.na(result$prob)) {
            width <- paste0(round(result$prob * 100, 1), "%")
            shinyjs::runjs(sprintf("document.getElementById('smc-progress').style.width = '%s'", width))
        } else {
            shinyjs::runjs("document.getElementById('smc-progress').style.width = '0%'")
        }
    })
    
    # Update V2G progress bar and text
    observe({
        result <- v2g_result()
        if (!is.na(result$prob)) {
            width <- paste0(round(result$prob * 100, 1), "%")
            shinyjs::runjs(sprintf("document.getElementById('v2g-progress').style.width = '%s'", width))
        } else {
            shinyjs::runjs("document.getElementById('v2g-progress').style.width = '0%'")
        }
    })
    
    # Display results
    output$smc_enrollment_prob <- renderText({
        result <- smc_result()
        if (is.na(result$prob)) return("NA")
        paste0(round(result$prob * 100, 1), "%")
    })
    
    output$smc_warning <- renderText({
        smc_result()$warning
    })
    
    output$v2g_enrollment_prob <- renderText({
        result <- v2g_result()
        if (is.na(result$prob)) return("NA")
        paste0(round(result$prob * 100, 1), "%")
    })
    
    output$v2g_warning <- renderText({
        v2g_result()$warning
    })
}

shinyApp(ui = ui, server = server)
