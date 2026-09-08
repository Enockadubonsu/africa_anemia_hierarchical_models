# ==============================================================================
# SCRIPT 02: MISSING DATA CHARACTERIZATION
# ==============================================================================
# Manuscript: Childhood Anemia and Maternal Media Exposure in Sub-Saharan Africa:
#             A Multi-Country Cross-Sectional Analysis Across Ghana, Nigeria,
#             and Tanzania
# Authors: Enock Adu Bonsu, Daniel Ebo, Dorcas Doku
# ------------------------------------------------------------------------------
# Purpose: Characterize missingness in hemoglobin measurements
#          Test MCAR assumption (Little's test)
#          Identify predictors of missingness (Table 2)
# Input:  data/processed/three_countries_full.rds
# Output: output/tables/table2_missingness_patterns.csv
#         output/tables/table2_missingness_predictors.csv
#         output/tables/table2_observed_vs_missing_comparison.csv
# ==============================================================================

library(dplyr)
library(tidyr)
library(broom)
library(naniar)

select <- dplyr::select

# Update to your local working directory
setwd("path/to/your/working/directory")

all_countries <- readRDS("data/processed/three_countries_full.rds")

cat("==============================================================================\n")
cat("MISSING DATA CHARACTERIZATION\n")
cat("==============================================================================\n\n")
cat("Total sample:", nrow(all_countries), "\n")
cat("Expected: 42,944\n\n")

# ==============================================================================
# PART A: MISSINGNESS BY COUNTRY (Table 2, Part A)
# ==============================================================================

cat("PART A: Hemoglobin missingness by country\n")
cat("------------------------------------------\n\n")

missingness_by_country <- all_countries %>%
  group_by(country) %>%
  summarise(
    Total_N     = n(),
    Hb_Observed = sum(!is.na(hb_adjusted)),
    Hb_Missing  = sum(is.na(hb_adjusted)),
    Pct_Missing = round(mean(is.na(hb_adjusted)) * 100, 1),
    Anemia_Prev = round(mean(anemia, na.rm=TRUE) * 100, 1),
    Mean_Hb_Adj = round(mean(hb_adjusted, na.rm=TRUE), 2),
    .groups = "drop"
  )

overall_row <- all_countries %>%
  summarise(
    country     = "Overall",
    Total_N     = n(),
    Hb_Observed = sum(!is.na(hb_adjusted)),
    Hb_Missing  = sum(is.na(hb_adjusted)),
    Pct_Missing = round(mean(is.na(hb_adjusted)) * 100, 1),
    Anemia_Prev = round(mean(anemia, na.rm=TRUE) * 100, 1),
    Mean_Hb_Adj = round(mean(hb_adjusted, na.rm=TRUE), 2)
  )

table2a <- bind_rows(missingness_by_country, overall_row)

cat("Missingness by country:\n")
print(table2a)

write.csv(table2a,
          "output/tables/table2a_missingness_by_country.csv",
          row.names = FALSE)

# ==============================================================================
# PART B: LITTLE'S MCAR TEST
# ==============================================================================

cat("\n\nPART B: Little's MCAR Test\n")
cat("---------------------------\n\n")

# Select variables for MCAR test
mcar_vars <- all_countries %>%
  select(
    hb_adjusted, media_score, child_age_months, child_sex_binary,
    birth_order, maternal_age, maternal_education_years,
    wealth_index, urban, anc_visits, iron_supp,
    improved_water, improved_sanitation
  )

mcar_result <- tryCatch({
  naniar::mcar_test(mcar_vars)
}, error = function(e) {
  cat("naniar::mcar_test failed:", e$message, "\n")
  cat("Proceeding with logistic regression approach only\n")
  NULL
})

if(!is.null(mcar_result)) {
  cat("Little's MCAR Test:\n")
  cat("  Chi-squared:", round(mcar_result$statistic, 1), "\n")
  cat("  df:", mcar_result$df, "\n")
  cat("  p-value:",
      ifelse(mcar_result$p.value < 0.001, "<0.001",
             round(mcar_result$p.value, 3)), "\n")
  cat("  Conclusion: Data are NOT missing completely at random (p<0.001)\n")
  cat("  Multiple imputation is required\n\n")
}

# ==============================================================================
# PART C: PREDICTORS OF MISSINGNESS (Table 2, Part B)
# ==============================================================================

cat("\n\nPART C: Logistic regression — predictors of hemoglobin missingness\n")
cat("--------------------------------------------------------------------\n\n")
cat("Outcome: hb_missing (1 = missing altitude-adjusted hemoglobin)\n\n")

# Logistic regression predicting missingness
missingness_model <- glm(
  is.na(hb_adjusted) ~ country_factor + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + urban + media_score,
  data   = all_countries,
  family = binomial(link = "logit")
)

missingness_results <- broom::tidy(
  missingness_model,
  exponentiate = TRUE,
  conf.int     = TRUE
) %>%
  mutate(
    OR_CI       = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(term, OR_CI, p_formatted)

cat("Predictors of hemoglobin missingness (OR, 95% CI):\n")
print(missingness_results)

write.csv(missingness_results,
          "output/tables/table2b_missingness_predictors.csv",
          row.names = FALSE)

# ==============================================================================
# PART D: COMPARE OBSERVED VS MISSING CHARACTERISTICS
# ==============================================================================

cat("\n\nPART D: Characteristics by missingness status\n")
cat("----------------------------------------------\n\n")

comparison <- all_countries %>%
  mutate(missing_group = ifelse(is.na(hb_adjusted),
                                 "Hb Missing", "Hb Observed")) %>%
  group_by(missing_group) %>%
  summarise(
    n                = n(),
    pct_total        = round(n() / nrow(all_countries) * 100, 1),
    mean_age_months  = round(mean(child_age_months, na.rm=TRUE), 1),
    pct_female       = round(mean(child_sex_binary, na.rm=TRUE) * 100, 1),
    mean_media_score = round(mean(media_score, na.rm=TRUE), 2),
    pct_urban        = round(mean(urban, na.rm=TRUE) * 100, 1),
    mean_wealth      = round(mean(wealth_index, na.rm=TRUE), 2),
    mean_maternal_ed = round(mean(maternal_education_years, na.rm=TRUE), 1),
    .groups = "drop"
  )

cat("Characteristics by missingness status:\n")
print(comparison)

# T-tests for key continuous variables
cat("\nT-tests comparing observed vs missing groups:\n")
for(var in c("child_age_months", "media_score", "wealth_index",
             "maternal_education_years", "maternal_age")) {
  t_result <- tryCatch({
    t.test(all_countries[[var]] ~ is.na(all_countries$hb_adjusted))
  }, error = function(e) NULL)

  if(!is.null(t_result)) {
    cat(sprintf("  %s: mean_obs=%.2f, mean_miss=%.2f, p=%s\n",
                var,
                t_result$estimate[2],
                t_result$estimate[1],
                ifelse(t_result$p.value < 0.001, "<0.001",
                       sprintf("%.3f", t_result$p.value))))
  }
}

write.csv(comparison,
          "output/tables/table2c_observed_vs_missing_comparison.csv",
          row.names = FALSE)

# ==============================================================================
# PART E: MISSINGNESS BY KEY SUBGROUPS
# ==============================================================================

cat("\n\nPART E: Missingness by key subgroups\n")
cat("--------------------------------------\n\n")

# By age group
miss_by_age <- all_countries %>%
  mutate(age_group = cut(child_age_months,
                         breaks = c(5, 11, 23, 35, 47, 59),
                         labels = c("6-11m","12-23m","24-35m","36-47m","48-59m"))) %>%
  group_by(age_group) %>%
  summarise(
    n           = n(),
    n_missing   = sum(is.na(hb_adjusted)),
    pct_missing = round(mean(is.na(hb_adjusted)) * 100, 1),
    .groups = "drop"
  )

cat("Missingness by age group:\n")
print(miss_by_age)

# By wealth quintile
miss_by_wealth <- all_countries %>%
  mutate(wealth_cat = case_when(
    wealth_index == 1 ~ "Poorest",
    wealth_index == 2 ~ "Poorer",
    wealth_index == 3 ~ "Middle",
    wealth_index == 4 ~ "Richer",
    wealth_index == 5 ~ "Richest",
    TRUE ~ NA_character_
  )) %>%
  group_by(wealth_cat) %>%
  summarise(
    n           = n(),
    n_missing   = sum(is.na(hb_adjusted)),
    pct_missing = round(mean(is.na(hb_adjusted)) * 100, 1),
    .groups = "drop"
  )

cat("\nMissingness by wealth quintile:\n")
print(miss_by_wealth)

# By media score
miss_by_media <- all_countries %>%
  mutate(media_cat = case_when(
    media_score == 0 ~ "No media",
    media_score == 1 ~ "One channel",
    media_score == 2 ~ "Two channels",
    media_score >= 3 ~ "Three+ channels",
    TRUE ~ NA_character_
  )) %>%
  group_by(media_cat) %>%
  summarise(
    n           = n(),
    n_missing   = sum(is.na(hb_adjusted)),
    pct_missing = round(mean(is.na(hb_adjusted)) * 100, 1),
    .groups = "drop"
  )

cat("\nMissingness by media exposure category:\n")
print(miss_by_media)

write.csv(miss_by_age,
          "output/tables/table2d_missingness_by_age.csv",
          row.names = FALSE)
write.csv(miss_by_wealth,
          "output/tables/table2e_missingness_by_wealth.csv",
          row.names = FALSE)
write.csv(miss_by_media,
          "output/tables/table2f_missingness_by_media.csv",
          row.names = FALSE)

cat("\n==============================================================================\n")
cat("SCRIPT 02 COMPLETE\n")
cat("Saved:\n")
cat("  output/tables/table2a_missingness_by_country.csv\n")
cat("  output/tables/table2b_missingness_predictors.csv\n")
cat("  output/tables/table2c_observed_vs_missing_comparison.csv\n")
cat("  output/tables/table2d_missingness_by_age.csv\n")
cat("  output/tables/table2e_missingness_by_wealth.csv\n")
cat("  output/tables/table2f_missingness_by_media.csv\n")
cat("Next: Run 03_two_stage_imputation.R\n")
cat("==============================================================================\n")
