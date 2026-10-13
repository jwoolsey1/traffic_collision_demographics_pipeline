# traffic_collision_demographics_pipeline
An R pipeline analyzing motor vehicle collision datasets from Open Data Portal, aggregating injury severities by age demographics into formatted executive reports

# Motor Vehicle Collision Demographic Analysis Pipeline

## Project Overview
This establishes a reproducible data aggregation and analysis pipeline in R to process state-level transportation safety data. The pipeline ingests multi-table relational datasets (Crashes, Parties, and Injured Persons) to isolate, analyze, and track collision severity trends, specifically highlighting impacts on vulnerable youth demographics.

## Key Technical Features
* **Relational Data Joins:** Performs complex primary/foreign key joins (`left_join`) to aggregate granular, person-level injury metrics up to macro crash-level records.
* **Tidy Data Reshaping:** Extensively utilizes `tidyr` (`pivot_longer` and `pivot_wider`) to transform structural metrics across time dimensions (Quarters) for cohort comparisons.
* **Demographic Cohort Stratification:** Implements logical age filters to isolate public health indicators across specific age groups (`stated_age < 16` and `< 8`).
* **Executive Report Formatting:** Leverages the `gt` package to programmatically generate publication-ready tables featuring custom typographic hierarchies and conditional text alignments.

## Privacy & Compliance Notice
This script features an automated synthetic data engine. The pipeline dynamically mocks identical schema tables. This allows developers and recruiters to verify the data architecture and report generation.
