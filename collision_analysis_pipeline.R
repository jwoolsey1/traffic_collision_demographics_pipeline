
# ==============================================================================
# Project: Motor Vehicle Collision Demographic Analysis Pipeline
# Objective: Ingest and cross-join multi-table crash datasets to generate 
#            vulnerable demographic safety reporting profiles.
# ==============================================================================

# 1. ENVIRONMENT SETUP & DEPENDENCIES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, janitor, lubridate, gt)

# 2. RELATIONAL DATA INGESTION (WITH PORTFOLIO REPRODUCIBILITY FALLBACK)
crash_path <- "Crashes 2025.xlsx"
party_path <- "Parties 2025.xlsx"
injury_path <- "Injuried witness.xlsx"

if (file.exists(crash_path) & file.exists(party_path) & file.exists(injury_path)) {
  # Load local records if available
  trinity_crash <- read.xlsx(crash_path) %>% clean_names()
  parties       <- read.xlsx(party_path) %>% clean_names() 
  injured       <- read.xlsx(injury_path) %>% clean_names() 
} else {
  # GENERATE MOCK REPRODUCIBILITY ENGINE FOR PORTFOLIO SHOWCASE
  set.seed(123)
  mock_ids <- 1001:1040
  
  trinity_crash <- tibble(
    collision_id = mock_ids,
    county_code = "53",
    crash_date_time = sample(seq(as.Date('2025/01/01'), as.Date('2025/12/31'), by="day"), 40)
  )
  parties <- tibble(
    collision_id = sample(mock_ids, 90, replace = TRUE),
    party_type = sample(c("Driver", "Passenger", "Pedestrian"), 90, replace = TRUE, prob = c(0.6, 0.3, 0.1))
  )
  injured <- tibble(
    collision_id = sample(mock_ids, 50, replace = TRUE),
    extent_of_injury_code = sample(c("ComplaintOfPain", "SuspectSerious", "Fatal"), 50, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    stated_age = sample(4:75, 50, replace = TRUE)
  )
  message("Notice: Baseline sheets not found. Simulating data environment for portfolio execution.")
}

# 3. DATE CLEANING & REFACTORING
yourcounty_crash <- yourcounty_crash %>% 
  mutate(
    # Handle numeric Excel dates or standard character strings smoothly
    crash_date_time = if_else(is.numeric(crash_date_time), 
                              as.Date(crash_date_time, origin = "1899-12-30"), 
                              as.Date(crash_date_time)),
    year = year(crash_date_time),
    quarter = quarter(crash_date_time, with_year = TRUE)
  )

# 4. MULTI-LEVEL STRUCTURAL RELATIONAL AGGREGATIONS
# Summarize party attributes to individual crash thresholds
party_summary <- parties %>%
  group_by(collision_id) %>%
  summarize(
    n_parties    = n(),                                  
    n_drivers    = sum(party_type == "Driver", na.rm = TRUE),
    n_passengers = sum(party_type == "Passenger", na.rm = TRUE),
    .groups      = "drop"
  )

# Aggregate universal injuries to specific event ids
injury_summary <- injured %>%
  group_by(collision_id) %>%
  summarize(
    total_injured         = sum(extent_of_injury_code != "Fatal", na.rm = TRUE),
    total_severe_injuries = sum(extent_of_injury_code == "SuspectSerious", na.rm = TRUE),
    total_fatalalities    = sum(extent_of_injury_code == "Fatal", na.rm = TRUE),
    .groups               = "drop"
  )

# Demographic Slicing: Vulnerable Youth Cohort (< 16 years)
injury_summary_LT16 <- injured %>% 
  filter(stated_age < 16) %>%
  group_by(collision_id) %>%
  summarize(
    total_injured_LT16   = sum(extent_of_injury_code != "Fatal", na.rm = TRUE),
    severe_injuries_LT16 = sum(extent_of_injury_code == "SuspectSerious", na.rm = TRUE),
    fatal_LT16           = sum(extent_of_injury_code == "Fatal", na.rm = TRUE),
    .groups              = "drop"
  )

# Demographic Slicing: Vulnerable Early Childhood Cohort (< 8 years)
injury_summary_LT8 <- injured %>% 
  filter(stated_age < 8) %>%
  group_by(collision_id) %>%
  summarize(
    total_injured_LT8   = sum(extent_of_injury_code != "Fatal", na.rm = TRUE),
    severe_injuries_LT8 = sum(extent_of_injury_code == "SuspectSerious", na.rm = TRUE),
    fatal_LT8           = sum(extent_of_injury_code == "Fatal", na.rm = TRUE),
    .groups             = "drop"
  )

# 5. DATA COHORT MERGING & REPORT COMPILATION
crash_summary <- yourcounty_crash %>%
  left_join(party_summary, by = "collision_id") %>%
  left_join(injury_summary, by = "collision_id") %>%
  left_join(injury_summary_LT16, by = "collision_id") %>%
  left_join(injury_summary_LT8, by = "collision_id") 

crash_year_summary <- crash_summary %>% 
  filter(county_code == "53") %>%
  group_by(quarter) %>%
  summarize(
    n_crashes             = n(),
    injured_total         = sum(total_injured, na.rm = TRUE),
    injured_LT16          = sum(total_injured_LT16, na.rm = TRUE),
    injured_LT8           = sum(total_injured_LT8, na.rm = TRUE),
    severe_injuries_total = sum(total_severe_injuries, na.rm = TRUE),
    severe_inj_LT16       = sum(severe_injuries_LT16, na.rm = TRUE),
    severe_injuries_LT8   = sum(severe_injuries_LT8, na.rm = TRUE),
    fatal_total           = sum(total_fatalalities, na.rm = TRUE),
    fatal_LT16            = sum(fatal_LT16, na.rm = TRUE),
    fatal_LT8             = sum(fatal_LT8, na.rm = TRUE),
    .groups               = "drop"
  ) 

# Helper function to pivot and structuralize specific variable groupings
pivot_metric_table <- function(data, prefix) {
  data %>% 
    select(starts_with(prefix), quarter) %>%
    pivot_longer(cols = starts_with(prefix), names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = quarter, values_from = Value)
}

crash_table      <- pivot_metric_table(crash_year_summary, "n_crashes")
inj_table        <- pivot_metric_table(crash_year_summary, "injured")
severe_inj_table <- pivot_metric_table(crash_year_summary, "severe_inj")
fatal_table      <- pivot_metric_table(crash_year_summary, "fatal")

complete_table <- bind_rows(crash_table, inj_table, severe_inj_table, fatal_table) %>%
  mutate(Metric = case_match(
    Metric,
    "n_crashes" ~ "Total crashes", "injured_total" ~ "Total injuries", 
    "severe_injuries_total" ~ "Total severe injuries", "fatal_total" ~ "Total fatalities", 
    "injured_LT16" ~ "Injuries < 16 years", "injured_LT8" ~ "Injuries < 8 years",
    "severe_inj_LT16" ~ "Severe injuries < 16 years", "severe_injuries_LT8" ~ "Severe injuries < 8 years",
    "fatal_LT16" ~ "Fatalities < 16 years", "fatal_LT8" ~ "Fatalities < 8 years"
  ))

# 6. EXECUTIVE GT PERFORMANCE VISUALIZATION
clean_table <- gt(complete_table) %>%   
  tab_header(
    title = "Motor Vehicle Collisions Demographics, YEAR",
    subtitle = "Analysis by Temporal Quarter Breakdowns"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Metric, 
      rows = stringr::str_starts(Metric, "Total")
    )
  ) %>%
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_body(
      columns = Metric, 
      rows = !stringr::str_starts(Metric, "Total")
    )
  ) %>%
  tab_source_note(
    source_note = md("_Data source: CA Open Data Portal, California Crash Reporting System._")
  )

print(clean_table)
