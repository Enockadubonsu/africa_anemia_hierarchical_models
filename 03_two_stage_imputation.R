# ==============================================================================
# SCRIPT 03: TWO-STAGE MULTIPLE IMPUTATION (COMPLETE SELF-CONTAINED VERSION)
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Purpose: Address missingness in hemoglobin measurements using two-stage MI
# Input:  data/processed/three_countries_full.rds
# Output: data/processed/twostage_imputations_FIXED.rds
#         output/tables/imputation_validation_summary.csv
# ==============================================================================
# TO RUN: Copy and paste this entire script into R console, or run:
#         source("03_two_stage_imputation.R")
# Output is logged to output/script03_log.txt AND shown in console
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP — all packages and settings defined here, no external dependencies
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(mice)
library(glmmTMB)
library(broom)
library(broom.mixed)

# Fix namespace conflict
select <- dplyr::select

# Settings — change N_IMPUTATIONS to 10 for a quick test run first
N_IMPUTATIONS <- 100   # Final run
# N_IMPUTATIONS <- 10  # Uncomment for quick test (~30 min)

SEED <- 123456
set.seed(SEED)

# Working directory — update if needed
setwd("path/to/your/working/directory")  # Update to your local path

# Start logging to file AND console simultaneously
log_file <- "output/script03_log.txt"
if(!dir.exists("output")) dir.create("output", recursive = TRUE)
if(!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Open log connection — split=TRUE means output goes to BOTH console and file
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("==============================================================================\n")
cat("SCRIPT 03: TWO-STAGE MULTIPLE IMPUTATION\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("==============================================================================\n\n")
cat("N_IMPUTATIONS:", N_IMPUTATIONS, "\n")
cat("SEED:", SEED, "\n\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

cat("Loading data...\n")
all_countries <- readRDS("data/processed/three_countries_full.rds")
cat("Loaded:", nrow(all_countries), "children\n\n")

# ------------------------------------------------------------------------------
# CLEAN HAVEN_LABELLED VARIABLES
# ------------------------------------------------------------------------------

clean_haven <- function(df) {
  df %>%
    mutate(across(where(~inherits(., "haven_labelled")),
                  ~as.numeric(as.character(.)))) %>%
    mutate(
      child_sex_binary    = as.numeric(child_sex_binary),
      had_diarrhea        = as.numeric(had_diarrhea),
      had_fever           = as.numeric(had_fever),
      had_cough           = as.numeric(had_cough),
      newspaper_weekly    = as.numeric(newspaper_weekly),
      radio_weekly        = as.numeric(radio_weekly),
      tv_weekly           = as.numeric(tv_weekly),
      internet_use        = as.numeric(internet_use),
      improved_water      = as.numeric(improved_water),
      improved_sanitation = as.numeric(improved_sanitation),
      iron_supp           = as.numeric(iron_supp),
      urban               = as.numeric(urban),
      country_factor      = as.factor(country_factor),
      unique_cluster_id   = as.character(unique_cluster_id),
      unique_household_id = as.character(unique_household_id)
    )
}

all_countries <- clean_haven(all_countries)

cat("Sample size:", nrow(all_countries), "\n")
cat("Hemoglobin missingness (hb_adjusted):",
    round(mean(is.na(all_countries$hb_adjusted)) * 100, 1), "%\n")
cat("Missing age:", sum(is.na(all_countries$child_age_months)), "\n\n")

# ==============================================================================
# STAGE 1: COVARIATE IMPUTATION USING MICE
# ==============================================================================

cat("==============================================================================\n")
cat("STAGE 1: Imputing covariates using MICE\n")
cat("==============================================================================\n\n")

covariate_data <- all_countries %>%
  select(
    child_age_months, child_sex_binary, birth_order,
    had_diarrhea, had_fever, had_cough,
    maternal_age, maternal_education_years, maternal_bmi,
    newspaper_weekly, radio_weekly, tv_weekly, internet_use, media_score,
    wealth_index, household_size, improved_water, improved_sanitation,
    anc_visits, iron_supp,
    urban, country_factor,
    unique_cluster_id, unique_household_id
  )

# Report missingness
covariate_missing <- covariate_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(covariate_data) * 100, 1)) %>%
  filter(n_missing > 0) %>%
  arrange(desc(pct_missing))

cat("Covariates with missing values:\n")
print(covariate_missing)
cat("\n")

# Imputation method setup
imp_method_cov <- make.method(covariate_data)
imp_method_cov["unique_cluster_id"]   <- ""
imp_method_cov["unique_household_id"] <- ""
imp_method_cov["country_factor"]      <- "polyreg"

# Predictor matrix
pred_matrix_cov <- make.predictorMatrix(covariate_data)
pred_matrix_cov[, c("unique_cluster_id", "unique_household_id")] <- 0
pred_matrix_cov["unique_cluster_id", ]   <- 0
pred_matrix_cov["unique_household_id", ] <- 0

cat("Running MICE covariate imputation (m =", N_IMPUTATIONS, ", maxit = 20)...\n")
cat("This may take 30-60 minutes...\n\n")

stage1_start <- Sys.time()

imp_cov <- mice(
  covariate_data,
  m               = N_IMPUTATIONS,
  maxit           = 20,
  method          = imp_method_cov,
  predictorMatrix = pred_matrix_cov,
  seed            = SEED,
  printFlag       = FALSE
)

stage1_end <- Sys.time()
cat("✓ Stage 1 complete\n")
cat("  Time taken:", round(difftime(stage1_end, stage1_start, units="mins"), 1), "minutes\n\n")

# Extract imputed covariate datasets
covariate_imputations <- lapply(1:N_IMPUTATIONS, function(i) complete(imp_cov, i))

# ==============================================================================
# STAGE 2: HEMOGLOBIN IMPUTATION USING MULTILEVEL GAUSSIAN MODELS
# ==============================================================================

cat("==============================================================================\n")
cat("STAGE 2: Imputing altitude-adjusted hemoglobin using multilevel Gaussian models\n")
cat("==============================================================================\n\n")

cat("NOTE: Imputing hb_adjusted (altitude-adjusted) not hb_gdl (raw)\n")
cat("      to ensure consistent anemia classification for observed and\n")
cat("      imputed children (both use same altitude-adjusted threshold).\n\n")

hemoglobin_data <- all_countries %>%
  select(hb_adjusted, unique_cluster_id, unique_household_id)

impute_hb_for_dataset <- function(cov_data, hb_data, imp_number) {

  combined <- cov_data %>%
    bind_cols(hb_data %>% select(hb_adjusted)) %>%
    rename(hb_gdl = hb_adjusted)

  observed_idx <- which(!is.na(combined$hb_gdl))
  missing_idx  <- which(is.na(combined$hb_gdl))

  # Progress every 10 imputations
  if(imp_number %% 10 == 0) {
    cat("  Imputation", imp_number, "of", N_IMPUTATIONS,
        "— Observed:", length(observed_idx),
        "Missing:", length(missing_idx),
        "— Time:", format(Sys.time(), "%H:%M:%S"), "\n")
  }

  if(length(missing_idx) > 0) {

    hb_model <- tryCatch({
      glmmTMB(
        hb_gdl ~ child_age_months + child_sex_binary + birth_order +
          maternal_age + maternal_education_years + maternal_bmi +
          wealth_index + media_score + urban + country_factor +
          (1 | unique_cluster_id),
        data   = combined[observed_idx, ],
        family = gaussian
      )
    }, error = function(e) {
      cat("    Imputation", imp_number, "— falling back to lm:", e$message, "\n")
      lm(
        hb_gdl ~ child_age_months + child_sex_binary + birth_order +
          maternal_age + maternal_education_years + maternal_bmi +
          wealth_index + media_score + urban + country_factor,
        data = combined[observed_idx, ]
      )
    })

    if(inherits(hb_model, "glmmTMB")) {
      pred      <- predict(hb_model, newdata = combined[missing_idx, ],
                           allow.new.levels = TRUE, type = "response")
      resid_var <- sigma(hb_model)^2
    } else {
      pred      <- predict(hb_model, newdata = combined[missing_idx, ])
      resid_var <- summary(hb_model)$sigma^2
    }

    pred_with_error <- rnorm(length(pred), mean = pred, sd = sqrt(resid_var))
    pred_with_error <- pmax(2, pmin(25, pred_with_error))
    combined$hb_gdl[missing_idx] <- pred_with_error
  }

  # Derive anemia deterministically — same threshold for observed and imputed
  combined$anemia <- ifelse(combined$hb_gdl < 11.0, 1, 0)
  combined$unique_cluster_id   <- as.character(combined$unique_cluster_id)
  combined$unique_household_id <- as.character(combined$unique_household_id)

  return(combined)
}

cat("Starting Stage 2 —", N_IMPUTATIONS, "imputations...\n")
cat("Progress reported every 10 imputations.\n\n")

stage2_start <- Sys.time()

final_imputations <- list()
for(i in 1:N_IMPUTATIONS) {
  final_imputations[[i]] <- impute_hb_for_dataset(
    covariate_imputations[[i]],
    hemoglobin_data,
    i
  )
}

stage2_end <- Sys.time()
cat("\n✓ Stage 2 complete\n")
cat("  Time taken:", round(difftime(stage2_end, stage2_start, units="mins"), 1), "minutes\n\n")

# ==============================================================================
# VALIDATE IMPUTED DISTRIBUTIONS
# ==============================================================================

cat("==============================================================================\n")
cat("VALIDATION\n")
cat("==============================================================================\n\n")

imp1   <- final_imputations[[1]]
obs_hb <- imp1$hb_gdl[!is.na(all_countries$hb_adjusted)]
imp_hb <- imp1$hb_gdl[is.na(all_countries$hb_adjusted)]

cat("Observed hemoglobin (altitude-adjusted):\n")
print(summary(obs_hb))
cat("  SD:", round(sd(obs_hb), 2), "\n\n")

cat("Imputed hemoglobin:\n")
print(summary(imp_hb))
cat("  SD:", round(sd(imp_hb), 2), "\n\n")

cat("Mean difference (observed - imputed):",
    round(mean(obs_hb) - mean(imp_hb), 2), "g/dL\n")
cat("Expected: small (<0.5 g/dL)\n\n")

cat("Truncation check:\n")
cat("  Values at exactly 2.0:", sum(imp_hb == 2.0), "\n")
cat("  Values at exactly 25.0:", sum(imp_hb == 25.0), "\n")
if(sum(imp_hb == 2.0) > 100 | sum(imp_hb == 25.0) > 100) {
  cat("  ⚠ WARNING: Truncation artifacts — review bounds\n\n")
} else {
  cat("  ✓ No major truncation artifacts\n\n")
}

anemia_prevs <- sapply(final_imputations, function(d) mean(d$anemia) * 100)
cat("Anemia prevalence across", N_IMPUTATIONS, "imputations:\n")
cat("  Mean:", round(mean(anemia_prevs), 1), "%\n")
cat("  SD:", round(sd(anemia_prevs), 2), "%\n")
cat("  Range:", round(min(anemia_prevs), 1), "% to",
    round(max(anemia_prevs), 1), "%\n")
cat("  Expected mean: ~55-60%\n\n")

# Anemia prevalence by country
cat("Anemia prevalence by country (imputation 1):\n")
data.frame(
  country     = imp1$country_factor,
  anemia      = imp1$anemia,
  was_missing = is.na(all_countries$hb_adjusted)
) %>%
  group_by(country) %>%
  summarise(
    n_observed      = sum(!was_missing),
    n_imputed       = sum(was_missing),
    anemia_observed = round(mean(anemia[!was_missing]) * 100, 1),
    anemia_imputed  = round(mean(anemia[was_missing]) * 100, 1),
    anemia_overall  = round(mean(anemia) * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nExpected observed anemia: Ghana ~54%, Nigeria ~58%, Tanzania ~61%\n\n")

# ==============================================================================
# RESTORE COUNTRY LABELS
# ==============================================================================

cat("Restoring country labels in all imputed datasets...\n")
country_vector <- as.character(all_countries$country)
for(i in 1:length(final_imputations)) {
  final_imputations[[i]]$country_factor <- country_vector
}
cat("Country distribution in imputation 1:\n")
print(table(final_imputations[[1]]$country_factor))

# ==============================================================================
# SAVE
# ==============================================================================

cat("\nSaving results...\n")
saveRDS(final_imputations, "data/processed/twostage_imputations_FIXED.rds")

validation_summary <- data.frame(
  Metric = c(
    "N Observed hemoglobin",
    "N Imputed hemoglobin",
    "Overall missingness (%)",
    "Mean Hb Observed (g/dL)",
    "Mean Hb Imputed (g/dL)",
    "SD Hb Observed (g/dL)",
    "SD Hb Imputed (g/dL)",
    "Anemia prevalence Observed (%)",
    "Anemia prevalence Imputed mean (%)",
    "Between-imputation SD (%)"
  ),
  Value = c(
    length(obs_hb),
    length(imp_hb),
    round(mean(is.na(all_countries$hb_adjusted)) * 100, 1),
    round(mean(obs_hb), 2),
    round(mean(imp_hb), 2),
    round(sd(obs_hb), 2),
    round(sd(imp_hb), 2),
    round(mean(obs_hb < 11) * 100, 1),
    round(mean(anemia_prevs), 1),
    round(sd(anemia_prevs), 2)
  )
)

write.csv(validation_summary,
          "output/tables/imputation_validation_summary.csv",
          row.names = FALSE)

total_time <- round(difftime(Sys.time(), stage1_start, units = "mins"), 1)

cat("\n==============================================================================\n")
cat("SCRIPT 03 COMPLETE\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total time:", total_time, "minutes\n")
cat("Saved: data/processed/twostage_imputations_FIXED.rds\n")
cat("Saved: output/tables/imputation_validation_summary.csv\n")
cat("Log:   output/script03_log.txt\n")
cat("Next:  Run 04_primary_gee_analysis.R\n")
cat("==============================================================================\n")

# Close log
sink(type = "output")
sink(type = "message")
close(log_con)
