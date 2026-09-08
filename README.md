# Childhood Anemia and Maternal Media Exposure in Sub-Saharan Africa

## Overview

This repository contains the R analysis code for the manuscript:

> **Childhood Anemia and Maternal Media Exposure in Sub-Saharan Africa:
> A Multi-Country Cross-Sectional Analysis Across Ghana, Nigeria, and Tanzania**
>
> Enock Adu Bonsu¹, Daniel Ebo², Dorcas Doku³
>
> ¹ Department of Epidemiology and Biostatistics, Mel and Enid Zuckerman College of Public Health, University of Arizona  
> ² Department of Communication, Georgia State University  
> ³ Department of Communication Studies, University of Iowa
>
> Corresponding author: Enock Adu Bonsu (enocka@arizona.edu)

**Preprint:** [medRxiv — DOI to be added when posted]  
**OSF Pre-registration:** https://doi.org/10.17605/OSF.IO/HKA4F


---

## Background

Childhood anemia affects over half of children under five in sub-Saharan Africa.
This study examines the association between maternal media exposure and childhood
anemia across Ghana, Nigeria, and Tanzania using nationally representative DHS
biomarker data, with a key methodological contribution addressing 61.5% hemoglobin
missingness through a validated two-stage multiple imputation framework.

---

## Data

Data are publicly available from the **Demographic and Health Surveys (DHS) Program**
at [https://dhsprogram.com](https://dhsprogram.com).

Three surveys are used:
- Ghana DHS 2022 (Children's Recode, KR file)
- Nigeria DHS 2023-24 (Children's Recode, KR file)
- Tanzania DHS 2022 (Children's Recode, KR file)

> ⚠️ **Raw data files are NOT included in this repository** per DHS data use
> terms and conditions. Researchers must register and apply for access
> independently at dhsprogram.com.

---

## Repository Structure

```
├── 00_packages_and_setup.R          # Package installation and global settings
├── 01_data_loading_cleaning.R       # DHS data loading, variable construction
├── 02_missing_data_characterization.R  # Missingness analysis, Little's MCAR test
├── 03_two_stage_imputation.R        # Two-stage multiple imputation (m=100)
├── 04_primary_gee_analysis.R        # Primary GEE analysis with Rubin's rules
├── 05_sensitivity_analyses.R        # Six pre-specified sensitivity analyses (SA1-SA6)
├── 06_mediation_analysis.R          # Exploratory mediation analysis
├── 07_figures_updated.R             # All manuscript figures (Figures 1-2, Supp 1-3)
├── README.md                        # This file
└── LICENSE                          # MIT License
```

---

## Requirements

**R version:** 4.5.3 or higher

**Key packages:**

| Package | Purpose |
|---------|---------|
| haven | Read DHS .dta files |
| dplyr, tidyr | Data manipulation |
| mice | Stage 1 covariate imputation |
| glmmTMB | Stage 2 hemoglobin imputation (multilevel Gaussian) |
| geepack | Primary GEE analysis |
| broom, broom.mixed | Tidy model output |
| survey | Survey-weighted analysis (SA2, SA3) |
| mediation | Mediation analysis (SA7) |
| foreach, doParallel | Parallel computing |
| ggplot2, patchwork | Figures |
| naniar | Missing data visualization |

Install all packages by running `00_packages_and_setup.R` first.

---

## Usage

Run scripts in numerical order:

```r
# Step 1: Install packages and set up environment
source("00_packages_and_setup.R")

# Step 2: Load and clean DHS data
# NOTE: Update file paths in this script to your local DHS data location
source("01_data_loading_cleaning.R")

# Step 3: Characterize missing data
source("02_missing_data_characterization.R")

# Step 4: Run two-stage multiple imputation (~4-8 hours)
# Results saved to data/processed/twostage_imputations_FIXED.rds
source("03_two_stage_imputation.R")

# Step 5: Primary GEE analysis (~30-60 minutes)
source("04_primary_gee_analysis.R")

# Step 6: Six sensitivity analyses (~2-3 hours)
source("05_sensitivity_analyses.R")

# Step 7: Mediation analysis (~15 minutes)
source("06_mediation_analysis.R")

# Step 8: Generate all figures
source("07_figures_updated.R")
```

> ⚠️ **Script 03** takes approximately 4-8 hours depending on hardware.
> Set `N_IMPUTATIONS <- 10` for a quick test run before the full analysis.

---

## Key Analytical Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Child age variable | b19 (birth history age) | hw1 missing for 60% of children without hemoglobin measurement |
| Anemia derivation | hw56/10 < 11.0 g/dL | hw71 contains continuous values in DHS-8, not binary flag |
| Imputation target | Altitude-adjusted Hb (hw56) | Ensures consistent classification for observed and imputed |
| Pooling method | Normal approximation (qnorm) | Barnard-Rubin df collapses to ~1 at m=100 with stable imputation |
| Primary model | Population-averaged GEE | Policy-relevant population-level estimates |

---

## Results Summary

| Analysis | OR | 95% CI | p-value |
|----------|----|--------|---------|
| Primary GEE MI | 0.946 | 0.917-0.976 | <0.001 |
| SA1: GLMM MI | 0.942 | 0.912-0.974 | <0.001 |
| SA2: Complete-case | 0.946 | 0.895-0.999 | 0.047 |
| SA3: Weighted MI | 0.941 | 0.906-0.978 | 0.002 |
| SA4: Ghana | 0.914 | 0.852-0.979 | 0.010 |
| SA4: Nigeria | 0.961 | 0.920-1.004 | 0.077 |
| SA4: Tanzania | 0.947 | 0.888-1.009 | 0.090 |
| SA5: Continuous Hb | β=0.046 g/dL | 0.025-0.066 | <0.001 |
| SA6: MNAR tipping point | No reversal within ±1 SD | — | — |

---

## Citation

If you use this code, please cite the manuscript:

```
Adu Bonsu E, Ebo D, Doku D. Childhood Anemia and Maternal Media Exposure
in Sub-Saharan Africa: A Multi-Country Cross-Sectional Analysis Across Ghana,
Nigeria, and Tanzania. [Journal, Year, DOI — We will update when published]
```

---

## License

This code is released under the MIT License. See [LICENSE](LICENSE) for details.

The DHS data used in this analysis are subject to DHS Program data use terms
and are not redistributed here.

---

## Acknowledgements

We thank ICF International and the DHS Program for data access, and the Ghana
Statistical Service, National Population Commission of Nigeria, and Tanzania
National Bureau of Statistics for conducting the surveys.
