
<!-- README.md is generated from README.Rmd. Please edit that file -->

You can use this DOI to cite this repository:

(zenodo link here…)

This repository contains the data and code to reproduce results from our
study titled [**“Grid-Integration of Electric Vehicles: Consumer
Preferences for Supplier Managed Charging and Vehicle-to-Grid
Programs”**](). All code is written using the [R programming
language](https://www.r-project.org/).

It also contains the source code of a [**Smart Charging Enrollment
Simulator**](https://gwuvehicle.shinyapps.io/enrollment_simulator/).

For a brief introduction of this study, proceed to [**this
link**](https://sc.pingfanhu.com). It also contains the [survey
manuscript](https://gwu.quarto.pub/smartchargingsurvey/), [enrollment
simulator](https://gwuvehicle.shinyapps.io/enrollment_simulator/), and
related [poster](https://sc.pingfanhu.com/pdf/poster.pdf) and
[slides](https://pingfan-hu.github.io/2025-dqe-smart-charging/).

**Authors**:

| Name | ORCID |
|----|----|
| Pingfan Hu | [0009-0001-4877-4844](https://orcid.org/0009-0001-4877-4844) |
| Brian Tarroja, Ph.D., P.E. | [0000-0002-7736-8642](https://orcid.org/0000-0002-7736-8642) |
| Matthew Dean, Ph.D. | [0000-0002-0346-4316](https://orcid.org/0000-0002-0346-4316) |
| Kate Forrest, Ph.D. | [0000-0001-6375-1299](https://orcid.org/0000-0001-6375-1299) |
| Eric Hittinger, Ph.D. | [0000-0001-8439-4016](https://orcid.org/0000-0001-8439-4016) |
| Alan Jenn, Ph.D. | [0000-0003-4232-0697](https://orcid.org/0000-0003-4232-0697) |
| John Paul Helveston, Ph.D. | [0000-0002-2657-9191](https://orcid.org/0000-0002-2657-9191) |

**DOI**: DOI here…

**Abstract**: As power systems transition toward renewable energy
sources, integrating Battery Electric Vehicles (BEVs) into grid
operations becomes crucial for optimizing energy resources and enabling
multi-energy sector coupling. Smart charging programs can help utilities
balance electricity supply and demand while facilitating renewable
energy integration, but their success depends on BEV owner
participation. This study examines two grid-integration strategies:
Supplier-Managed Charging (SMC), which enables utilities to optimize
charging timing and duration, and Vehicle-to-Grid (V2G), which
transforms BEVs into distributed energy storage resources that can
support grid stability. Using a discrete choice experiment with 1,356
current BEV owners, recruited through social media advertisements and
survey panels, we quantify how different program attributes influence
enrollment decisions. Our multinomial logit models reveal distinct
preference patterns that inform program design: SMC participants
predominantly value operational flexibility and recurring payments,
while V2G participants show stronger preferences for monetary
incentives, indicating willingness to provide grid services for
compensation. Through simulation analysis, we identify program
\`\`attribute equivalencies’’ that quantify the changes needed in
program attributes to achieve equivalent enrollment levels, offering
utilities guidance for designing grid-integration programs that balance
system needs with consumer preferences. These findings offer insights
for developing market mechanisms and policy frameworks that can
accelerate the integration of BEVs into future energy systems while
supporting power system decarbonization.

# Replication Steps

1.  Install [R](https://cran.r-project.org) and
    [RStudio](https://posit.co/downloads/).
2.  Double click on the `smart-charging.Rproj` file to launch RStudio.
3.  Open `code/run.R`, select all scripts using `Ctrl/cmd + A`.
4.  Run the code scripts using `Ctrl/cmd + Enter`, or by pushing the
    “Run” button on the top right corner.

# File Organization

## code

Contains the source code of survey design and analysis. To reperform the
analysis, simply run the `run.R` file.

| file | description |
|----|----|
| `run.R` | A single file to reproduce all analysis. |
| `analysis/0_source_scripts.R` | Triggers all analysis code, called by `run.R`. |
| `analysis/1_clean_data.R` | Cleans the raw survey data and saves into the `results/` directory. |
| `analysis/2_summary_statistics_plots.R` | Generates the summarized plots of the raw survey data and saves into the `results/1_summary/` directory. |
| `analysis/3_mnl_pref_model.R` | Constructs the multinomial logit model with user preference space and saves into the `results/2_models/` directory. |
| `analysis/4_mnl_enrollment_sensitivity.R` | Simulates the user enrollment sensitivity of smart charging programs and saves into the `results/3_enrollment_sensitivity/` directory. |
| `analysis/5_mnl_scenario_analysis.R` | Simulates the scenario analysis and saves into the `results/4_scenario_analysis/` directory. |
| `design/1_power_analysis_smc.R` | Performs power analysis conjoint survey questions to help determine the expected number of participants. |
| `design/2_make_choice_questions.R` | Generates conjoint choice questions and save csv files into the `data/` directory. |
| `design/3_make_smc_battery_figs.R` | Generates battery condition figures for the SMC smart charging programs and saves into the `figs/` directory. |
| `design/4_make_v2g_battery_figs.R` | Generates battery condition figures for the V2G smart charging programs and saves into the `figs/` directory. |
| `design/5_make_cars_df.R` | Generates car makes and models and save csv files into the `data/` directory. |

Comments:

1.  Simply run the `run.R` file will generate all analysis code and save
    the results in the `results/` directory.
2.  The `run.R` file triggers the `analysis/0_source_scripts.R` file,
    and then further triggers the five other R files under `analysis/`.
3.  The five R files under `analysis/` cannot be executed alone unless
    the `run.R` file is executed.
4.  The files in the `design/` directory are used for designing the
    survey and don’t need to be executed anymore.
5.  The `design/2_make_choice_questions.R` file contains randomization.
    If you run this file again, the resulted `smc_questions.csv` and
    `v2g_questions.csv` files will be different, which will not match
    with the questions used in our survey recruitment. If you want to
    see the performance of this file, better save the original question
    csv files somewhere else.

## data

Contains miscellaneous files that are used in survey design and
analysis.

| file | description |
|----|----|
| `bev_makes.csv` | Brands that sell BEVs in the U.S. market. |
| `bevs.csv` | BEV models with makes, models, and ranges. |
| `car_makes.csv` | All car makes in the U.S. market. |
| `car_models.csv` | All car models in the U.S. market. |
| `cars.csv` | Raw car data scrapped from cars.com. All other car data files are generated from this file, with further manual modification. |
| `smc_questions.csv` | SMC program conjoint question file generated from `code/design/2_make_choice_questions.R`. |
| `smc_scenarios.csv` | SMC program scenarios used for scenario analysis, hand generated. |
| `v2g_questions.csv` | V2G program conjoint question file generated from `code/design/2_make_choice_questions.R`. |
| `v2g_scenarios.csv` | V2g program scenarios used for scenario analysis, hand generated. |
| Six Image Files | Images used as banners and educational visual aids in survey. The banner images are generated using DALL-E, and educational images are made using Affinity Designer 2. |

## figs

Contains all battery conditional figures of SMC and V2G smart charging
programs, generated by `code/design/3_make_smc_battery_figs.R` and
`code/design/4_make_v2g_battery_figs.R`.

## results

Contains all data files generated by `code/run.R`, which triggers the R
code scripts in the `code/analysis/` directory.

1.  The csv files and `.RData` files are the processed survey results.
    We generated these 2 file types to comply with the needs of data
    analysis in R code.
2.  The four folders contain the results of different code script files
    in `code/analysis/`.

## simulator

This is a standalone folder that contains the Shiny App source code of
the [**Smart Charging Enrollment
Simulator**](https://gwuvehicle.shinyapps.io/enrollment_simulator/).

## survey

Contains the raw survey responses. We recruited our survey on these two
sources:

1.  Meta - the well-know social media, including
    [Facebook](https://www.facebook.com),
    [Instagram](https://www.instagram.com) and
    [Messenger](https://www.messenger.com).
2.  [Dynata](https://www.dynata.com) - a largely used survey panel.

Survey files are in 7 pieces (for both sources) due to the limitation of
[formr](https://formr.org), the survey platform we used for this study.

## tables

The `.tex` formatted tables used in our paper.

| file | description |
|----|----|
| `table_demographics.tex` | Demographic results table, generated from `code/analysis/2_summary_statistics_plots.R`. |
| `table_mnl_model_smc.tex` | MNL SMC model table, generated from `code/analysis/3_1_mnl_pref_model.R`. |
| `table_mnl_model_v2g.tex` | MNL V2G model table, generated from `code/analysis/3_1_mnl_pref_model.R`. |
| `table_mxl_model_smc.tex` | MXL SMC model table, generated from `code/analysis/4_1_mxl_pref_model.R`. |
| `table_mxl_model_v2g.tex` | MXL V2G model table, generated from `code/analysis/4_1_mxl_pref_model.R`. |
| `table_sample_size.tex` | Sample size of the survey, containing both Meta and Dynata. |
| `table_smc_attr.tex` | SMC program attributes, helps to design the survey. |
| `table_smc_equiv.tex` | SMC program equivalency table using MXL, generated from the “Equivalency Table” section in `code/analysis/4_2_mxl_enrollment_sensitivity.R`. |
| `table_v2g_attr.tex` | V2G program attributes, helps to design the survey. |
| `table_v2g_equiv.tex` | V2G program equivalency table using MXL, generated from the “Equivalency Table” section in `code/analysis/4_2_mxl_enrollment_sensitivity.R`. |
| `table_vehicles.tex` | Vehicle ownership results table, generated from `code/analysis/2_summary_statistics_plots.R`. |
