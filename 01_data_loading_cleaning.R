# ==============================================================================
# SCRIPT 01: DATA LOADING AND CLEANING
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Purpose: Load DHS Children's Recode (KR) files for Ghana, Nigeria, Tanzania
#          and process into a clean analytical dataset
# Input:  Raw DHS .dta files
# Output: data/processed/three_countries_full.rds
# ==============================================================================
# NOTE: Update file paths below to match your local DHS data directory
# DHS data available at https://dhsprogram.com (registration required)
# ==============================================================================

source("00_packages_and_setup.R")

# ==============================================================================
# FUNCTION: Load and Process DHS Country Data
# ==============================================================================
# This function reads a DHS Children's Recode file and processes it into
# the analytical variables used in the manuscript

load_dhs_country <- function(country_name,
                              country_code,
                              kr_file,
                              survey_year,
                              dhs_phase) {

  cat("\n========================================\n")
  cat("Loading:", country_name, survey_year, "\n")
  cat("File:", basename(kr_file), "\n")
  cat("========================================\n")

  data <- read_dta(kr_file)

  cat("Original observations:", nrow(data), "\n")
  cat("Original variables:", ncol(data), "\n")

  if(!("hw53" %in% names(data))) {
    warning("hw53 (hemoglobin) not found — verify this is a Children's Recode file")
  }

  data_processed <- data %>%
    mutate(

      # ------------------------------------------------------------------
      # IDENTIFIERS
      # ------------------------------------------------------------------
      country       = country_name,
      country_code  = country_code,
      survey_year   = survey_year,
      dhs_phase     = dhs_phase,

      caseid            = as.character(caseid),
      cluster_id        = v001,
      household_id      = v002,
      line_number       = v003,

      unique_child_id     = paste0(country_code, "_", caseid, "_", bidx),
      unique_household_id = paste0(country_code, "_", v001, "_", v002),
      unique_cluster_id   = paste0(country_code, "_", v001),

      # ------------------------------------------------------------------
      # OUTCOME: HEMOGLOBIN AND ANEMIA
      # ------------------------------------------------------------------
      hb_raw      = hw53,
      hb_gdl      = case_when(
        as.numeric(hw53) >= 900 ~ NA_real_,   # Recode flag values to NA
        TRUE ~ as.numeric(hw53) / 10
      ),
      hb_adjusted = case_when(
        as.numeric(hw56) >= 900 ~ NA_real_,   # Recode flag values to NA
        TRUE ~ as.numeric(hw56) / 10
      ),

      # Anemia status derived directly from altitude-adjusted hemoglobin
      # WHO criteria: hemoglobin < 11.0 g/dL for children 6-59 months
      # NOTE: In DHS-8, hw71 contains continuous hemoglobin values NOT a
      # binary anemia flag. hw58 (the binary flag) is empty in these files.
      # Anemia is therefore derived deterministically from hb_adjusted.
      # This ensures perfect consistency between observed and imputed
      # anemia classification — both use the same threshold on the same
      # altitude-adjusted hemoglobin variable.
      anemia = case_when(
        is.na(hb_adjusted)  ~ NA_real_,
        hb_adjusted < 11.0  ~ 1,
        hb_adjusted >= 11.0 ~ 0
      ),

      anemia_severity = case_when(
        is.na(hb_adjusted)             ~ NA_character_,
        hb_adjusted < 7.0              ~ "Severe",
        hb_adjusted >= 7.0 & hb_adjusted < 10.0 ~ "Moderate",
        hb_adjusted >= 10.0 & hb_adjusted < 11.0 ~ "Mild",
        hb_adjusted >= 11.0            ~ "Not anemic"
      ),

      anemia_severity_ord = factor(
        anemia_severity,
        levels  = c("Not anemic", "Mild", "Moderate", "Severe"),
        ordered = TRUE
      ),

      hb_missing = is.na(hb_gdl),

      # ------------------------------------------------------------------
      # CHILD CHARACTERISTICS
      # ------------------------------------------------------------------
      # IMPORTANT: Use b19 (current age in months from birth history) rather
      # than hw1 (age at time of hemoglobin measurement).
      # hw1 is only populated for children who were measured (missing for
      # 28,596 of 47,919 children = 59.7%), making it unsuitable as a
      # predictor in the imputation model.
      # b19 is always populated for all listed children (zero missing)
      # and correlates perfectly (r=1.0) with hw1 when both are available.
      child_age_months = b19,

      child_sex = case_when(
        b4 == 1 ~ "Male",
        b4 == 2 ~ "Female",
        TRUE    ~ NA_character_
      ),

      # Binary: 0 = Male, 1 = Female
      child_sex_binary = case_when(
        b4 == 1 ~ 0,
        b4 == 2 ~ 1,
        TRUE    ~ NA_real_
      ),

      birth_order = bord,

      child_age_cat = case_when(
        child_age_months < 6  ~ "0-5 months",
        child_age_months < 12 ~ "6-11 months",
        child_age_months < 24 ~ "12-23 months",
        child_age_months < 36 ~ "24-35 months",
        child_age_months < 48 ~ "36-47 months",
        child_age_months <= 59 ~ "48-59 months",
        TRUE ~ NA_character_
      ),

      # Recent illness (past two weeks)
      had_diarrhea = case_when(h11 == 0 ~ 0, h11 %in% c(1,2) ~ 1, TRUE ~ NA_real_),
      had_fever    = case_when(h31 == 0 ~ 0, h31 %in% c(1,2) ~ 1, TRUE ~ NA_real_),
      had_cough    = case_when(h31b== 0 ~ 0, h31b%in% c(1,2) ~ 1, TRUE ~ NA_real_),

      any_recent_illness = case_when(
        had_diarrhea == 1 | had_fever == 1 | had_cough == 1 ~ 1,
        had_diarrhea == 0 & had_fever == 0 & had_cough == 0 ~ 0,
        TRUE ~ NA_real_
      ),

      # ------------------------------------------------------------------
      # MATERNAL CHARACTERISTICS
      # ------------------------------------------------------------------
      maternal_age = v012,

      maternal_age_cat = case_when(
        maternal_age < 20 ~ "15-19",
        maternal_age < 25 ~ "20-24",
        maternal_age < 30 ~ "25-29",
        maternal_age < 35 ~ "30-34",
        maternal_age < 40 ~ "35-39",
        maternal_age < 45 ~ "40-44",
        maternal_age <= 49 ~ "45-49",
        TRUE ~ NA_character_
      ),

      maternal_education_level   = as_factor(v106),
      maternal_education_years   = v107,

      maternal_education_cat = case_when(
        v106 == 0 ~ "No education",
        v106 == 1 ~ "Primary",
        v106 == 2 ~ "Secondary",
        v106 == 3 ~ "Higher",
        TRUE ~ NA_character_
      ),

      maternal_secondary_plus = case_when(
        v106 >= 2 ~ 1, v106 < 2 ~ 0, TRUE ~ NA_real_
      ),

      maternal_bmi = v445 / 100,

      maternal_bmi_cat = case_when(
        maternal_bmi < 18.5 ~ "Underweight",
        maternal_bmi < 25   ~ "Normal",
        maternal_bmi < 30   ~ "Overweight",
        maternal_bmi >= 30  ~ "Obese",
        TRUE ~ NA_character_
      ),

      # ------------------------------------------------------------------
      # MEDIA EXPOSURE (PRIMARY EXPOSURE)
      # DHS variables: v157=newspaper, v158=radio, v159=TV, v171a=internet
      # Coded: 0=not at all, 1=less than once/week, 2=at least once/week, 3=almost daily
      # Weekly = at least once per week (coded >= 2)
      # ------------------------------------------------------------------
      newspaper_freq   = v157,
      newspaper_weekly = case_when(v157 >= 2 ~ 1, v157 < 2 ~ 0, TRUE ~ NA_real_),

      radio_freq   = v158,
      radio_weekly = case_when(v158 >= 2 ~ 1, v158 < 2 ~ 0, TRUE ~ NA_real_),

      tv_freq   = v159,
      tv_weekly = case_when(v159 >= 2 ~ 1, v159 < 2 ~ 0, TRUE ~ NA_real_),

      internet_use = case_when(v171a == 1 ~ 1, v171a == 0 ~ 0, TRUE ~ NA_real_),

      # Composite media exposure score (0-4): sum of weekly exposure across channels
      media_score = newspaper_weekly + radio_weekly + tv_weekly +
        coalesce(internet_use, 0),

      any_media_weekly = case_when(
        media_score >= 1 ~ 1, media_score == 0 ~ 0, TRUE ~ NA_real_
      ),

      media_exposure_cat = case_when(
        media_score == 0 ~ "No media",
        media_score == 1 ~ "One channel",
        media_score == 2 ~ "Two channels",
        media_score >= 3 ~ "Three+ channels",
        TRUE ~ NA_character_
      ),

      media_exposure_cat_ord = factor(
        media_exposure_cat,
        levels  = c("No media", "One channel", "Two channels", "Three+ channels"),
        ordered = TRUE
      ),

      # ------------------------------------------------------------------
      # ANTENATAL CARE AND HEALTH BEHAVIORS
      # ------------------------------------------------------------------
      anc_visits = m14,

      anc_4plus = case_when(m14 >= 4 ~ 1, m14 < 4 ~ 0, TRUE ~ NA_real_),

      anc_visits_cat = case_when(
        m14 == 0             ~ "None",
        m14 < 4              ~ "1-3 visits",
        m14 >= 4 & m14 < 8  ~ "4-7 visits",
        m14 >= 8             ~ "8+ visits",
        TRUE ~ NA_character_
      ),

      # Iron supplementation during pregnancy
      iron_supp = case_when(m45 == 1 ~ 1, m45 == 0 ~ 0, TRUE ~ NA_real_),

      # ------------------------------------------------------------------
      # HOUSEHOLD CHARACTERISTICS
      # ------------------------------------------------------------------
      wealth_index = v190,

      wealth_quintile = case_when(
        v190 == 1 ~ "Poorest", v190 == 2 ~ "Poorer",
        v190 == 3 ~ "Middle",  v190 == 4 ~ "Richer",
        v190 == 5 ~ "Richest", TRUE ~ NA_character_
      ),

      wealth_quintile_ord = factor(
        wealth_quintile,
        levels  = c("Poorest", "Poorer", "Middle", "Richer", "Richest"),
        ordered = TRUE
      ),

      wealth_poor = case_when(v190 <= 2 ~ 1, v190 > 2 ~ 0, TRUE ~ NA_real_),

      household_size = v136,

      # ------------------------------------------------------------------
      # GEOGRAPHIC CHARACTERISTICS
      # ------------------------------------------------------------------
      urban_rural = v025,
      urban       = case_when(v025 == 1 ~ 1, v025 == 2 ~ 0, TRUE ~ NA_real_),
      region      = as_factor(v024),
      region_code = v024,

      # ------------------------------------------------------------------
      # WATER AND SANITATION
      # ------------------------------------------------------------------
      water_source      = as_factor(v113),
      water_source_code = v113,

      improved_water = case_when(
        v113 %in% c(11,12,13,14,21,31,41,51,61,62,71) ~ 1,
        TRUE ~ 0
      ),

      toilet_type      = as_factor(v116),
      toilet_type_code = v116,

      improved_sanitation = case_when(
        v116 %in% c(11,12,13,14,15,21,22,31) ~ 1,
        TRUE ~ 0
      ),

      # ------------------------------------------------------------------
      # SURVEY DESIGN VARIABLES
      # ------------------------------------------------------------------
      sample_weight = v005 / 1000000,
      psu           = v021,
      strata        = v022

    ) %>%
    select(
      country, country_code, survey_year, dhs_phase,
      unique_child_id, unique_household_id, unique_cluster_id,
      cluster_id, household_id, caseid,
      hb_gdl, hb_adjusted, anemia, anemia_severity, anemia_severity_ord, hb_missing,
      child_age_months, child_age_cat, child_sex, child_sex_binary,
      birth_order, had_diarrhea, had_fever, had_cough, any_recent_illness,
      maternal_age, maternal_age_cat, maternal_education_level,
      maternal_education_cat, maternal_education_years, maternal_secondary_plus,
      maternal_bmi, maternal_bmi_cat,
      newspaper_freq, newspaper_weekly, radio_freq, radio_weekly,
      tv_freq, tv_weekly, internet_use, media_score, any_media_weekly,
      media_exposure_cat, media_exposure_cat_ord,
      anc_visits, anc_4plus, anc_visits_cat, iron_supp,
      wealth_index, wealth_quintile, wealth_quintile_ord, wealth_poor, household_size,
      urban_rural, urban, region, region_code,
      water_source, water_source_code, improved_water,
      toilet_type, toilet_type_code, improved_sanitation,
      sample_weight, psu, strata
    )

  cat("Processed observations:", nrow(data_processed), "\n")
  cat("Children with hemoglobin data:", sum(!is.na(data_processed$hb_gdl)), "\n")
  cat("Missingness rate:",
      round(mean(data_processed$hb_missing) * 100, 1), "%\n")

  if(sum(!is.na(data_processed$anemia)) > 0) {
    cat("Anemia prevalence (observed):",
        round(mean(data_processed$anemia, na.rm = TRUE) * 100, 1), "%\n")
  }

  return(data_processed)
}

# ==============================================================================
# LOAD DATA FROM THREE COUNTRIES
# ==============================================================================
# Update file paths to match your local DHS data directory

ghana <- load_dhs_country(
  country_name = "Ghana",
  country_code = "GH",
  kr_file      = "path/to/ghana/GHKR8CFL.DTA"  # Update to your local DHS data path,
  survey_year  = 2022,
  dhs_phase    = "DHS-8"
)

nigeria <- load_dhs_country(
  country_name = "Nigeria",
  country_code = "NG",
  kr_file      = "path/to/nigeria/NGKR8AFL.dta"  # Update to your local DHS data path,
  survey_year  = 2024,
  dhs_phase    = "DHS-8"
)

tanzania <- load_dhs_country(
  country_name = "Tanzania",
  country_code = "TZ",
  kr_file      = "path/to/tanzania/TZKR82FL.DTA"  # Update to your local DHS data path,
  survey_year  = 2022,
  dhs_phase    = "DHS-8"
)

# ==============================================================================
# COMBINE THREE COUNTRIES AND APPLY ELIGIBILITY CRITERIA
# ==============================================================================

all_countries <- bind_rows(ghana, nigeria, tanzania) %>%
  mutate(
    country_factor = factor(country, levels = c("Ghana", "Nigeria", "Tanzania"))
  ) %>%
  # Eligibility: children aged 6-59 months
  # (children < 6 months excluded per WHO hemoglobin assessment guidelines)
  filter(child_age_months >= 6 & child_age_months <= 59)

cat("\n==============================================================================\n")
cat("COMBINED DATASET SUMMARY\n")
cat("==============================================================================\n\n")

cat("Total children (6-59 months):", nrow(all_countries), "\n")
cat("Clusters:", length(unique(all_countries$unique_cluster_id)), "\n")
cat("Households:", length(unique(all_countries$unique_household_id)), "\n\n")

cat("By country:\n")
print(table(all_countries$country))

cat("\nHemoglobin missingness by country:\n")
all_countries %>%
  group_by(country) %>%
  summarise(
    n = n(),
    n_observed = sum(!is.na(hb_gdl)),
    pct_missing = round(mean(hb_missing) * 100, 1)
  ) %>%
  print()

cat("\nAnemia prevalence (observed only):\n")
all_countries %>%
  filter(!is.na(anemia)) %>%
  group_by(country) %>%
  summarise(
    n_observed = n(),
    anemia_pct = round(mean(anemia) * 100, 1),
    mean_hb    = round(mean(hb_gdl, na.rm = TRUE), 1)
  ) %>%
  print()

# ==============================================================================
# SAVE PROCESSED DATASET
# ==============================================================================

saveRDS(all_countries, "data/processed/three_countries_full.rds")

cat("\n==============================================================================\n")
cat("SCRIPT 01 COMPLETE\n")
cat("Saved: data/processed/three_countries_full.rds\n")
cat("Next: Run 02_missing_data_characterization.R\n")
cat("==============================================================================\n")
