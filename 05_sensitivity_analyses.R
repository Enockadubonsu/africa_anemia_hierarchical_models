# ==============================================================================
# SCRIPT 05: SENSITIVITY ANALYSES (SA1-SA6)
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Purpose: Six pre-specified sensitivity analyses to assess robustness of
#          the primary GEE estimate (Table 4)
# Input:  data/processed/twostage_imputations_FIXED.rds
#         data/processed/three_countries_full.rds
# Output: output/tables/table4_sensitivity_analyses.csv
# ==============================================================================
# SENSITIVITY ANALYSES:
# SA1 — GLMM cluster-specific models (full MI + Rubin's rules)
# SA2 — Survey-weighted complete-case analysis
# SA3 — Weighted multiple imputation (post-imputation weighting)
# SA4 — Country-stratified GEE (full MI + Rubin's rules per country) [CORRECTED]
# SA5 — Continuous hemoglobin outcome (linear GEE)
# SA6 — MNAR delta-adjustment sensitivity
# ==============================================================================
# NOTE: All sensitivity analyses are secondary and exploratory.
# They assess robustness of the primary estimate, not independent hypotheses.
# No correction for multiple comparisons is applied.
# ==============================================================================

source("00_packages_and_setup.R")
library(glmmTMB)

final_imputations <- readRDS("data/processed/twostage_imputations_FIXED.rds")
all_countries     <- readRDS("data/processed/three_countries_full.rds")
primary_results   <- readRDS("data/processed/primary_gee_pooled_results.rds")

cat("==============================================================================\n")
cat("SENSITIVITY ANALYSES\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# RUBIN'S RULES POOLING FUNCTION (reused across SA1, SA3, SA4)
# ------------------------------------------------------------------------------

pool_rubins <- function(results_list, term_col = "term",
                        est_col = "estimate", se_col = "std.error") {

  param_names <- unique(results_list[[1]][[term_col]])

  pooled <- lapply(param_names, function(param) {

    estimates <- sapply(results_list, function(r)
      r[[est_col]][r[[term_col]] == param])
    ses <- sapply(results_list, function(r)
      r[[se_col]][r[[term_col]] == param])

    m     <- length(estimates)
    Q_bar <- mean(estimates)
    U_bar <- mean(ses^2)
    B     <- var(estimates)
    T_var <- U_bar + (1 + 1/m) * B
    FMI   <- (1 + 1/m) * B / T_var

    # Large-sample normal approximation (m=100 imputations)
    t_crit   <- qnorm(0.975)
    p_value  <- 2 * pnorm(-abs(Q_bar / sqrt(T_var)))
    df       <- Inf

    data.frame(
      term     = param,
      OR       = exp(Q_bar),
      OR_lower = exp(Q_bar - t_crit * sqrt(T_var)),
      OR_upper = exp(Q_bar + t_crit * sqrt(T_var)),
      p_value  = p_value,
      FMI      = round(FMI * 100, 1),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(pooled)
}

# ==============================================================================
# SA1: GLMM CLUSTER-SPECIFIC MODELS WITH FULL MI
# ==============================================================================

cat("SA1: GLMM Cluster-Specific Models\n")
cat("-----------------------------------\n\n")

# Prepare data — need unique_cluster_id in imputed datasets
for(i in 1:length(final_imputations)) {
  final_imputations[[i]]$unique_cluster_id <-
    as.factor(all_countries$unique_cluster_id)
}

sa1_list <- list()

for(i in 1:length(final_imputations)) {

  if(i %% 20 == 0) cat("  SA1 imputation", i, "\n")

  result <- tryCatch({

    model <- glmmTMB(
      anemia ~ media_score + child_age_months + child_sex_binary +
        birth_order + maternal_age + maternal_education_years +
        wealth_index + urban + country_factor +
        (1 | unique_cluster_id),
      data   = final_imputations[[i]],
      family = binomial(link = "logit"),
      control = glmmTMBControl(
        optimizer = optim,
        optArgs   = list(method = "BFGS")
      )
    )

    broom.mixed::tidy(model, effects = "fixed", component = "cond")

  }, error = function(e) NULL)

  if(!is.null(result)) sa1_list[[i]] <- result
}

sa1_list <- Filter(Negate(is.null), sa1_list)
cat("SA1 successful models:", length(sa1_list), "\n")

sa1_pooled <- pool_rubins(sa1_list)
sa1_media  <- sa1_pooled[sa1_pooled$term == "media_score", ]

cat("SA1 media score: OR =", round(sa1_media$OR, 3),
    "(", round(sa1_media$OR_lower, 3), "-", round(sa1_media$OR_upper, 3), ")",
    "p =", round(sa1_media$p_value, 3), "\n\n")

# ==============================================================================
# SA2: SURVEY-WEIGHTED COMPLETE-CASE ANALYSIS
# ==============================================================================

cat("SA2: Survey-Weighted Complete-Case Analysis\n")
cat("--------------------------------------------\n\n")

complete_data <- all_countries %>%
  filter(
    !is.na(anemia), !is.na(child_age_months), !is.na(child_sex_binary),
    !is.na(birth_order), !is.na(maternal_age), !is.na(maternal_education_years),
    !is.na(wealth_index), !is.na(media_score), !is.na(urban),
    !is.na(country_factor), !is.na(sample_weight), !is.na(unique_cluster_id)
  ) %>%
  mutate(strata_var = interaction(urban, country_factor, drop = TRUE))

cat("SA2 complete-case sample size:", nrow(complete_data), "\n")

dhs_design <- svydesign(
  ids     = ~unique_cluster_id,
  strata  = ~strata_var,
  weights = ~sample_weight,
  data    = complete_data,
  nest    = TRUE
)

sa2_model <- svyglm(
  anemia ~ media_score + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + urban + country_factor,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

sa2_results <- broom::tidy(sa2_model, exponentiate = TRUE, conf.int = TRUE)
sa2_media   <- sa2_results[sa2_results$term == "media_score", ]

cat("SA2 media score: OR =", round(sa2_media$estimate, 3),
    "(", round(sa2_media$conf.low, 3), "-", round(sa2_media$conf.high, 3), ")",
    "p =", round(sa2_media$p.value, 3), "\n\n")

# ==============================================================================
# SA3: WEIGHTED MULTIPLE IMPUTATION (POST-IMPUTATION WEIGHTING)
# ==============================================================================

cat("SA3: Weighted Multiple Imputation\n")
cat("-----------------------------------\n\n")

# Restore cluster and weight info to imputed datasets
for(i in 1:length(final_imputations)) {
  final_imputations[[i]]$sample_weight    <- all_countries$sample_weight
  final_imputations[[i]]$unique_cluster_id <- all_countries$unique_cluster_id
  final_imputations[[i]]$strata_var <- paste0(
    all_countries$urban, ".", all_countries$country
  )
}

sa3_list <- list()

for(i in 1:length(final_imputations)) {

  if(i %% 20 == 0) cat("  SA3 imputation", i, "\n")

  result <- tryCatch({

    design <- svydesign(
      ids     = ~unique_cluster_id,
      strata  = ~strata_var,
      weights = ~sample_weight,
      data    = final_imputations[[i]],
      nest    = TRUE
    )

    model <- svyglm(
      anemia ~ media_score + child_age_months + child_sex_binary +
        birth_order + maternal_age + maternal_education_years +
        wealth_index + urban + country_factor,
      design = design,
      family = quasibinomial(link = "logit")
    )

    broom::tidy(model, conf.int = TRUE)

  }, error = function(e) NULL)

  if(!is.null(result)) sa3_list[[i]] <- result
}

sa3_list   <- Filter(Negate(is.null), sa3_list)
cat("SA3 successful models:", length(sa3_list), "\n")

sa3_pooled <- pool_rubins(sa3_list)
sa3_media  <- sa3_pooled[sa3_pooled$term == "media_score", ]

cat("SA3 media score: OR =", round(sa3_media$OR, 3),
    "(", round(sa3_media$OR_lower, 3), "-", round(sa3_media$OR_upper, 3), ")",
    "p =", round(sa3_media$p_value, 3), "\n\n")

# ==============================================================================
# SA4: COUNTRY-STRATIFIED GEE WITH FULL MI (CORRECTED VERSION)
# ==============================================================================
# IMPORTANT: Previous analysis used only one imputed dataset with glmer.
# This corrected version applies the full two-stage MI + GEE + Rubin's rules
# procedure within each country, matching the primary analysis methodology.

cat("SA4: Country-Stratified GEE with Full MI (Corrected)\n")
cat("------------------------------------------------------\n\n")

# Ensure country labels are character (not numeric codes)
# country_factor was stored as factor with numeric codes during pre-processing
for(i in 1:length(final_imputations)) {
  final_imputations[[i]]$country_factor <- as.character(all_countries$country)
}

countries <- c("Ghana", "Nigeria", "Tanzania")

sa4_results <- lapply(countries, function(ctry) {

  cat("Processing:", ctry, "\n")

  country_gee_list <- lapply(1:length(final_imputations), function(i) {

    if(i %% 20 == 0) cat("  Imputation", i, "of",
                          length(final_imputations), "\n")

    country_data <- final_imputations[[i]] %>%
      filter(country_factor == ctry) %>%
      arrange(cluster_id)

    tryCatch({
      gee_model <- geeglm(
        anemia ~ media_score + child_age_months + child_sex_binary +
          birth_order + maternal_age + maternal_education_years +
          wealth_index + urban,
        data   = country_data,
        id     = cluster_id,
        family = binomial(link = "logit"),
        corstr = "exchangeable"
      )
      broom::tidy(gee_model, conf.int = TRUE)
    }, error = function(e) {
      cat("    Failed imputation", i, ":", e$message, "\n")
      NULL
    })
  })

  country_gee_list <- Filter(Negate(is.null), country_gee_list)
  cat("  Successful models:", length(country_gee_list), "\n")

  if(length(country_gee_list) < 10) {
    cat("  WARNING: Too few models for", ctry, "\n")
    return(NULL)
  }

  param_names <- unique(country_gee_list[[1]]$term)

  pooled <- lapply(param_names, function(param) {

    estimates <- sapply(country_gee_list, function(r)
      r$estimate[r$term == param])
    ses <- sapply(country_gee_list, function(r)
      r$std.error[r$term == param])

    m     <- length(estimates)
    Q_bar <- mean(estimates)
    U_bar <- mean(ses^2)
    B     <- var(estimates)
    T_var <- U_bar + (1 + 1/m) * B
    FMI   <- (1 + 1/m) * B / T_var

    # Large-sample normal approximation (justified with m=100 imputations)
    t_crit  <- qnorm(0.975)
    p_value <- 2 * pnorm(-abs(Q_bar / sqrt(T_var)))
    df      <- Inf

    data.frame(
      country  = ctry,
      term     = param,
      OR       = exp(Q_bar),
      OR_lower = exp(Q_bar - t_crit * sqrt(T_var)),
      OR_upper = exp(Q_bar + t_crit * sqrt(T_var)),
      p_value  = p_value,
      FMI      = round(FMI * 100, 1),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(pooled)
})

sa4_results  <- Filter(Negate(is.null), sa4_results)
sa4_pooled   <- bind_rows(sa4_results)
sa4_media    <- sa4_pooled[sa4_pooled$term == "media_score", ]

cat("\nSA4 media score by country:\n")
print(sa4_media[, c("country", "OR", "OR_lower", "OR_upper", "p_value", "FMI")])

# Heterogeneity test
log_ors  <- log(sa4_media$OR)
ses_sa4  <- (log(sa4_media$OR_upper) - log(sa4_media$OR_lower)) / (2 * 1.96)
weights  <- 1 / ses_sa4^2
grand_mean <- sum(weights * log_ors) / sum(weights)
Q        <- sum(weights * (log_ors - grand_mean)^2)
p_het    <- pchisq(Q, df = 2, lower.tail = FALSE)
I2       <- max(0, (Q - 2) / Q * 100)

cat("\nHeterogeneity: Q =", round(Q, 3), "p =", round(p_het, 3),
    "I2 =", round(I2, 1), "%\n\n")

# ==============================================================================
# SA5: CONTINUOUS HEMOGLOBIN OUTCOME (LINEAR GEE)
# ==============================================================================

cat("SA5: Continuous Hemoglobin Outcome\n")
cat("------------------------------------\n\n")

sa5_list <- list()

for(i in 1:length(final_imputations)) {

  if(i %% 20 == 0) cat("  SA5 imputation", i, "\n")

  result <- tryCatch({

    data_i <- final_imputations[[i]]
    data_i$cluster_id <- as.factor(data_i$unique_cluster_id)
    data_i <- data_i[order(data_i$cluster_id), ]

    gee_model <- geeglm(
      hb_gdl ~ media_score + child_age_months + child_sex_binary +
        birth_order + maternal_age + maternal_education_years +
        wealth_index + urban + country_factor,
      data   = data_i,
      id     = cluster_id,
      family = gaussian(link = "identity"),
      corstr = "exchangeable"
    )

    broom::tidy(gee_model, conf.int = TRUE)

  }, error = function(e) NULL)

  if(!is.null(result)) sa5_list[[i]] <- result
}

sa5_list   <- Filter(Negate(is.null), sa5_list)
cat("SA5 successful models:", length(sa5_list), "\n")

# Pool SA5 (linear scale — no exponentiation)
sa5_param_names <- unique(sa5_list[[1]]$term)

sa5_pooled_list <- lapply(sa5_param_names, function(param) {

  estimates <- sapply(sa5_list, function(r) r$estimate[r$term == param])
  ses       <- sapply(sa5_list, function(r) r$std.error[r$term == param])

  m     <- length(estimates)
  Q_bar <- mean(estimates)
  U_bar <- mean(ses^2)
  B     <- var(estimates)
  T_var <- U_bar + (1 + 1/m) * B
  FMI   <- (1 + 1/m) * B / T_var

  # Large-sample normal approximation (m=100 imputations)
  t_crit  <- qnorm(0.975)
  p_value <- 2 * pnorm(-abs(Q_bar / sqrt(T_var)))
  df      <- Inf

  data.frame(
    term     = param,
    beta     = Q_bar,
    ci_lower = Q_bar - t_crit * sqrt(T_var),
    ci_upper = Q_bar + t_crit * sqrt(T_var),
    p_value  = p_value,
    FMI      = round(FMI * 100, 1),
    stringsAsFactors = FALSE
  )
})

sa5_pooled <- bind_rows(sa5_pooled_list)
sa5_media  <- sa5_pooled[sa5_pooled$term == "media_score", ]

cat("SA5 media score: beta =", round(sa5_media$beta, 4),
    "(", round(sa5_media$ci_lower, 4), "-", round(sa5_media$ci_upper, 4), ")",
    "p =", round(sa5_media$p_value, 3), "g/dL\n\n")

# ==============================================================================
# SA6: MNAR DELTA-ADJUSTMENT SENSITIVITY
# ==============================================================================

cat("SA6: MNAR Delta-Adjustment Sensitivity\n")
cat("----------------------------------------\n\n")

# SD of observed altitude-adjusted hemoglobin
# Using hb_adjusted to match the variable imputed in Stage 2 of Script 03
hb_sd <- sd(all_countries$hb_adjusted, na.rm = TRUE)
cat("SD of observed altitude-adjusted hemoglobin:", round(hb_sd, 2), "g/dL\n\n")

deltas <- seq(-1.0, 1.0, by = 0.25)
sa6_results <- list()

for(delta in deltas) {

  cat("  Testing delta =", delta, "SD\n")

  # Apply delta shift to imputed (previously missing) hemoglobin
  # was_missing defined on hb_adjusted to match Script 03 imputation basis
  delta_imputations <- lapply(final_imputations, function(imp) {
    imp_shifted <- imp
    was_missing  <- is.na(all_countries$hb_adjusted)
    imp_shifted$hb_gdl[was_missing] <- imp$hb_gdl[was_missing] + delta * hb_sd
    imp_shifted$anemia <- ifelse(imp_shifted$hb_gdl < 11.0, 1, 0)
    imp_shifted
  })

  # Fit GEE on shifted datasets (use first 20 for speed)
  delta_gee_list <- lapply(1:min(20, length(delta_imputations)), function(i) {

    data_i <- delta_imputations[[i]]
    data_i$cluster_id <- as.factor(data_i$unique_cluster_id)
    data_i <- data_i[order(data_i$cluster_id), ]

    tryCatch({
      gee_model <- geeglm(
        anemia ~ media_score + child_age_months + child_sex_binary +
          birth_order + maternal_age + maternal_education_years +
          wealth_index + urban + country_factor,
        data   = data_i,
        id     = cluster_id,
        family = binomial(link = "logit"),
        corstr = "exchangeable"
      )
      broom::tidy(gee_model, conf.int = TRUE)
    }, error = function(e) NULL)
  })

  delta_gee_list <- Filter(Negate(is.null), delta_gee_list)

  if(length(delta_gee_list) >= 5) {
    delta_pooled <- pool_rubins(delta_gee_list)
    delta_media  <- delta_pooled[delta_pooled$term == "media_score", ]

    sa6_results[[as.character(delta)]] <- data.frame(
      delta    = delta,
      delta_SD = paste0(delta, " SD"),
      OR       = round(delta_media$OR, 3),
      OR_lower = round(delta_media$OR_lower, 3),
      OR_upper = round(delta_media$OR_upper, 3),
      p_value  = round(delta_media$p_value, 3),
      direction_protective = delta_media$OR < 1
    )
  }
}

sa6_df <- bind_rows(sa6_results)

cat("\nMNAR Sensitivity Results:\n")
print(sa6_df)

# Identify tipping point
tipping <- sa6_df %>%
  filter(!direction_protective) %>%
  slice(1)

if(nrow(tipping) > 0) {
  cat("\nTipping point: delta =", tipping$delta, "SD\n")
  cat("(Conclusion reverses at this shift in imputed hemoglobin)\n")
} else {
  cat("\nNo tipping point found within tested range (-1 to +1 SD)\n")
}

# ==============================================================================
# COMPILE TABLE 4
# ==============================================================================

cat("\n==============================================================================\n")
cat("TABLE 4: SENSITIVITY ANALYSES SUMMARY\n")
cat("==============================================================================\n\n")

primary_media <- primary_results[primary_results$term == "media_score", ]

table4 <- data.frame(
  Analysis = c(
    "PRIMARY: GEE Population-Averaged MI",
    "SA1: GLMM Cluster-Specific MI",
    "SA2: Survey-Weighted Complete-Case",
    "SA3: Weighted MI",
    "SA4: Ghana only",
    "SA4: Nigeria only",
    "SA4: Tanzania only",
    "SA4: Heterogeneity (I²)",
    "SA5: Continuous Hemoglobin",
    "SA6: MNAR Sensitivity"
  ),
  n = c(
    47919, 47919, nrow(complete_data), 47919,
    9353, 27783, 10783, NA, 47919, 47919
  ),
  OR_CI = c(
    sprintf("%.3f (%.3f-%.3f)", primary_media$OR,
            primary_media$OR_lower, primary_media$OR_upper),
    sprintf("%.3f (%.3f-%.3f)", sa1_media$OR,
            sa1_media$OR_lower, sa1_media$OR_upper),
    sprintf("%.3f (%.3f-%.3f)", sa2_media$estimate,
            sa2_media$conf.low, sa2_media$conf.high),
    sprintf("%.3f (%.3f-%.3f)", sa3_media$OR,
            sa3_media$OR_lower, sa3_media$OR_upper),
    sprintf("%.3f (%.3f-%.3f)", sa4_media$OR[1],
            sa4_media$OR_lower[1], sa4_media$OR_upper[1]),
    sprintf("%.3f (%.3f-%.3f)", sa4_media$OR[2],
            sa4_media$OR_lower[2], sa4_media$OR_upper[2]),
    sprintf("%.3f (%.3f-%.3f)", sa4_media$OR[3],
            sa4_media$OR_lower[3], sa4_media$OR_upper[3]),
    paste0("I2=", round(I2, 0), "%, p=", round(p_het, 3)),
    sprintf("beta=%.3f (%.3f to %.3f)", sa5_media$beta,
            sa5_media$ci_lower, sa5_media$ci_upper),
    paste0("Tipping point: delta>",
           ifelse(nrow(tipping)>0, tipping$delta, ">1.0"), " SD")
  ),
  p_value = c(
    ifelse(primary_media$p_value < 0.001, "<0.001",
           round(primary_media$p_value, 3)),
    ifelse(sa1_media$p_value < 0.001, "<0.001",
           round(sa1_media$p_value, 3)),
    ifelse(sa2_media$p.value < 0.001, "<0.001",
           round(sa2_media$p.value, 3)),
    ifelse(sa3_media$p_value < 0.001, "<0.001",
           round(sa3_media$p_value, 3)),
    round(sa4_media$p_value[1], 3),
    round(sa4_media$p_value[2], 3),
    round(sa4_media$p_value[3], 3),
    round(p_het, 3),
    round(sa5_media$p_value, 3),
    "—"
  ),
  FMI = c(
    primary_media$FMI,
    sa1_media$FMI,
    "—",
    sa3_media$FMI,
    sa4_media$FMI[1],
    sa4_media$FMI[2],
    sa4_media$FMI[3],
    "—",
    sa5_media$FMI,
    "—"
  )
)

print(table4)

# ==============================================================================
# SAVE ALL RESULTS
# ==============================================================================

write.csv(table4, "output/tables/table4_sensitivity_analyses.csv",
          row.names = FALSE)

saveRDS(list(
  sa1 = sa1_pooled,
  sa2 = sa2_results,
  sa3 = sa3_pooled,
  sa4 = sa4_pooled,
  sa4_heterogeneity = data.frame(Q=Q, df=2, p_het=p_het, I2=I2),
  sa5 = sa5_pooled,
  sa6 = sa6_df
), "data/processed/all_sensitivity_results.rds")

cat("\n==============================================================================\n")
cat("SCRIPT 05 COMPLETE\n")
cat("Saved: output/tables/table4_sensitivity_analyses.csv\n")
cat("Saved: data/processed/all_sensitivity_results.rds\n")
cat("Next: Run 06_mediation_analysis.R\n")
cat("==============================================================================\n")
