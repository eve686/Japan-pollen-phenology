# Artificial Light at Night and Pollen Phenology in Japan

## Overview

This repository contains the data processing, statistical analysis, and visualization code for a research study investigating the effects of artificial light at night (ALAN), climate, and urbanization on pollen phenology and abundance across Japan.

The study focuses on two major allergenic tree species:

- Japanese cedar (*Cryptomeria japonica*, sugi)
- Japanese cypress (*Chamaecyparis obtusa*, hinoki)

Using long-term pollen monitoring records from 1986 to 2023, this study examines spatial and temporal patterns in pollen phenology and abundance and their relationships with environmental conditions.

## Research Objectives

The main objectives are to:

1. Characterize long-term changes in pollen season onset, peak timing, maximum daily pollen abundance, and annual total pollen abundance.
2. Investigate spatial variation in pollen phenology across monitoring stations in Japan.
3. Examine relationships between pollen dynamics and environmental factors, including ALAN, temperature, precipitation, and land cover.
4. Evaluate the relative importance of environmental predictors using machine learning and SHAP analysis.

## Repository Contents

| File | Description |
|---|---|
| `Pollen.R` | Main R script for data processing, temporal trend analysis, quantile analysis, environmental comparisons, and figure generation. |
| `Merged_files_copy.xlsx` | Merged pollen observations and environmental variables used for statistical analyses. |
| `Merged_files_annual.xlsx` | Annual pollen and environmental dataset, including climate variables. |
| `station_locations_unique.xlsx` | Geographic information for pollen monitoring stations. |
| `ALANMap2023.tif` | Artificial light at night raster for 2023|
| `LCMap2022/merge.tif` | Land-cover raster used in the mapping analysis, provided separately. |

Large Land cover raster datasets are not be included directly in this repository.

## Data and Variables

### Pollen observations

Four pollen-related variables are examined for both Japanese cedar and Japanese cypress:

- Start of pollen season (day of year, DOY)
- Peak pollen date (DOY)
- Maximum daily pollen abundance
- Annual total pollen abundance

### Environmental variables

Environmental predictors include:

- Artificial light at night (ALAN)
- Air temperature
- Precipitation
- Land-cover characteristics
- Urbanization indicators

ALAN values are log-transformed as `log10(ALAN + 1)` where specified in the analysis.

## Statistical Analyses

Analyses are conducted primarily in R.

The current code includes:

- Data cleaning and monitoring-station selection
- Temporal trend analysis using linear regression
- Annual quantile analysis (10th, 25th, 50th, 75th, and 90th percentiles)
- Spatial visualization of station-level trends
- Land-cover comparisons
- Environmental trend analysis
- Figure generation using ggplot2

Machine-learning analyses using XGBoost and SHAP will be documented and added as the code is finalized.

## Software Requirements

The analyses use R and packages including:

`tidyverse`, `dplyr`, `readr`, `readxl`, `lubridate`, `ggplot2`, `sf`, `terra`, `patchwork`, `broom`, `purrr`, `ggh4x`, `viridis`, and `maps`.

Additional package requirements and software versions will be documented in a future update.

## Reproducibility

The analysis scripts currently contain some local file paths that will need to be adjusted for use on other computers.

To reproduce the analyses:

1. Download or clone the repository.
2. Obtain the required pollen and environmental datasets.
3. Install the required R packages.
4. Update the data paths in the analysis scripts.
5. Run the scripts in the appropriate sequence.

The code and documentation will be updated to improve reproducibility.

## Data Availability

Data availability is subject to the licensing and redistribution conditions of the original data providers.

Some large environmental raster datasets may be accessed through their original data sources rather than directly through GitHub.

## Code Availability

**Status: Under development**

Analysis scripts and documentation will continue to be updated. A stable version of the code is planned for release in association with the research publication.

## Contact

For questions about the code or research, please open an issue in this repository.
