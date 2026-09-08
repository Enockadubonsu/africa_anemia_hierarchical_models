# ==============================================================================
# SCRIPT 00: PACKAGES AND SETUP
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Authors: Enock Adu Bonsu, Daniel Ebo, Dorcas Doku
# Affiliation: University of Arizona / Georgia State University / University of Iowa
# Journal target: BMJ Open
# ==============================================================================
# RUN THIS SCRIPT FIRST before any other script
# All subsequent scripts assume these packages are installed and loaded
# ==============================================================================

# ------------------------------------------------------------------------------
# INSTALL PACKAGES (run once only — comment out after first use)
# ------------------------------------------------------------------------------

# install.packages(c(
#   "haven",       # Read DHS .dta files
#   "dplyr",       # Data manipulation
#   "tidyr",       # Data reshaping
#   "labelled",    # Handle haven_labelled variables
#   "mice",        # Multiple imputation (Stage 1 — covariates)
#   "glmmTMB",     # Multilevel models (Stage 2 — hemoglobin imputation)
#   "geepack",     # GEE for primary analysis
#   "broom",       # Tidy model output
#   "broom.mixed", # Tidy mixed model output
#   "survey",      # Survey-weighted analysis (SA2)
#   "mediation",   # Formal mediation analysis
#   "parallel",    # Parallel computing
#   "doParallel",  # Parallel backend
#   "foreach",     # Parallel loops
#   "ggplot2",     # Figures
#   "patchwork",   # Combine figures
#   "naniar",      # Missing data visualization
#   "knitr",       # Tables
#   "kableExtra"   # Table formatting
# ))

# ------------------------------------------------------------------------------
# LOAD ALL PACKAGES
# ------------------------------------------------------------------------------

library(haven)
library(dplyr)
library(tidyr)
library(labelled)
library(mice)
library(glmmTMB)
library(geepack)
library(broom)
library(broom.mixed)
library(survey)
library(mediation)
library(parallel)
library(doParallel)
library(foreach)
library(ggplot2)
library(patchwork)
library(naniar)

# ------------------------------------------------------------------------------
# GLOBAL SETTINGS
# ------------------------------------------------------------------------------

# Working directory — update to your local path
setwd("path/to/your/working/directory")  # Update to your local path

# Reproducibility
set.seed(123456)

# Analysis settings
N_IMPUTATIONS <- 100   # Number of imputations for final run
                        # Use 10 for development/testing
ALPHA <- 0.05          # Two-sided significance level

# ------------------------------------------------------------------------------
# CREATE OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------

dirs <- c(
  "data/processed",
  "output/tables",
  "output/figures",
  "output/tables/supplementary",
  "output/figures/supplementary"
)

for(d in dirs) {
  if(!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("==============================================================================\n")
cat("  SETUP COMPLETE\n")
cat("==============================================================================\n")
cat("Working directory:", getwd(), "\n")
cat("Imputations set to:", N_IMPUTATIONS, "\n")
cat("All packages loaded successfully\n")
cat("Output directories created\n")
cat("==============================================================================\n\n")
cat("Run scripts in this order:\n")
cat("  01_data_loading_cleaning.R\n")
cat("  02_missing_data_characterization.R\n")
cat("  03_two_stage_imputation.R\n")
cat("  04_primary_gee_analysis.R\n")
cat("  05_sensitivity_analyses.R\n")
cat("  06_mediation_analysis.R\n")
cat("==============================================================================\n")
