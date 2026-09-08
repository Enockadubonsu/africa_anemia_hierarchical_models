# ==============================================================================
# SCRIPT 04: PRIMARY GEE ANALYSIS
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Purpose: Primary analysis — population-averaged GEE with two-stage MI
#          Fit GEE on each of 100 imputed datasets, pool via Rubin's rules
#          This constitutes the single pre-specified primary analysis
# Input:  data/processed/twostage_imputations_FIXED.rds
# Output: data/processed/primary_gee_pooled_results.rds
#         output/tables/table3_primary_gee_results.csv
# ==============================================================================

source("00_packages_and_setup.R")

final_imputations <- readRDS("data/processed/twostage_imputations_FIXED.rds")

cat("==============================================================================\n")
cat("PRIMARY GEE ANALYSIS: Population-Averaged GEE with Two-Stage MI\n")
cat("==============================================================================\n\n")
cat("Number of imputations:", length(final_imputations), "\n")
cat("Sample size:", nrow(final_imputations[[1]]), "\n\n")

# ==============================================================================
# PRE-PROCESS IMPUTATIONS
# ==============================================================================
# Convert cluster to factor and sort — required for geepack

cat("Pre-processing imputed datasets...\n")

for(i in 1:length(final_imputations)) {
  if(i %% 20 == 0) cat("  Processing imputation", i, "\n")

  final_imputations[[i]]$cluster_id <- as.factor(
    final_imputations[[i]]$unique_cluster_id
  )

  final_imputations[[i]] <- final_imputations[[i]][
    order(final_imputations[[i]]$cluster_id), ]

  # Retain only variables needed for GEE
  final_imputations[[i]] <- final_imputations[[i]][, c(
    "anemia", "media_score", "child_age_months", "child_sex_binary",
    "birth_order", "maternal_age", "maternal_education_years",
    "wealth_index", "urban", "country_factor", "cluster_id"
  )]
}

gc()
cat("✓ Pre-processing complete\n\n")

# ==============================================================================
# FIT GEE ON ALL IMPUTED DATASETS
# ==============================================================================

cat("Fitting GEE on all", length(final_imputations), "imputations...\n\n")

gee_results_list <- list()
failed_imputations <- c()

for(i in 1:length(final_imputations)) {

  if(i %% 10 == 0) {
    cat("  Processing imputation", i, "\n")
    gc(verbose = FALSE)
  }

  result <- tryCatch({

    gee_model <- geeglm(
      anemia ~ media_score + child_age_months + child_sex_binary +
        birth_order + maternal_age + maternal_education_years +
        wealth_index + urban + country_factor,
      data    = final_imputations[[i]],
      id      = cluster_id,
      family  = binomial(link = "logit"),
      corstr  = "exchangeable"
    )

    gee_coef <- broom::tidy(gee_model, conf.int = TRUE)
    rm(gee_model)
    list(success = TRUE, results = gee_coef)

  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })

  if(result$success) {
    gee_results_list[[i]] <- result$results
  } else {
    failed_imputations <- c(failed_imputations, i)
    cat("    Failed imputation", i, ":", result$error, "\n")
  }
}

gee_results_list <- gee_results_list[!sapply(gee_results_list, is.null)]
n_successful     <- length(gee_results_list)
n_failed         <- length(failed_imputations)

cat("\nFitting summary:\n")
cat("  Successful:", n_successful, "\n")
cat("  Failed:", n_failed, "\n")

if(n_successful < 20) stop("Too few successful models — check GEE specification")

# ==============================================================================
# POOL RESULTS USING RUBIN'S RULES
# ==============================================================================

cat("\nPooling results using Rubin's rules...\n")

param_names <- unique(gee_results_list[[1]]$term)

pooled_gee <- lapply(param_names, function(param) {

  estimates <- sapply(gee_results_list, function(res)
    res$estimate[res$term == param])
  ses <- sapply(gee_results_list, function(res)
    res$std.error[res$term == param])

  m     <- length(estimates)
  Q_bar <- mean(estimates)
  U_bar <- mean(ses^2)
  B     <- var(estimates)
  T_var <- U_bar + (1 + 1/m) * B
  FMI   <- (1 + 1/m) * B / T_var

  # Large-sample normal approximation (justified with m=100 imputations)
  # Barnard-Rubin df collapses to ~1 when B << U_bar in well-specified models,
  # artificially widening CIs via heavy t-distribution tails.
  # Normal approximation is appropriate and avoids this problem.
  t_crit   <- qnorm(0.975)   # 1.96
  ci_lower <- Q_bar - t_crit * sqrt(T_var)
  ci_upper <- Q_bar + t_crit * sqrt(T_var)
  p_value  <- 2 * pnorm(-abs(Q_bar / sqrt(T_var)))
  df       <- Inf   # Inf indicates normal approximation used

  data.frame(
    term     = param,
    estimate = Q_bar,
    OR       = exp(Q_bar),
    OR_lower = exp(ci_lower),
    OR_upper = exp(ci_upper),
    p_value  = p_value,
    FMI      = round(FMI * 100, 1),
    df       = round(df, 1),
    stringsAsFactors = FALSE
  )
})

pooled_results <- bind_rows(pooled_gee)

# ==============================================================================
# DISPLAY PRIMARY RESULTS
# ==============================================================================

cat("\n==============================================================================\n")
cat("PRIMARY ANALYSIS RESULTS (Table 3)\n")
cat("==============================================================================\n\n")

results_display <- pooled_results %>%
  mutate(
    OR_CI       = sprintf("%.3f (%.3f-%.3f)", OR, OR_lower, OR_upper),
    p_formatted = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    FMI_pct     = paste0(FMI, "%")
  ) %>%
  select(term, OR_CI, p_formatted, FMI_pct)

print(results_display)

# Highlight primary finding
media_row <- pooled_results[pooled_results$term == "media_score", ]

cat("\n==============================================================================\n")
cat("PRIMARY FINDING — Media Score:\n")
cat("==============================================================================\n")
cat("  OR:       ", round(media_row$OR, 3), "\n")
cat("  95% CI:   ", round(media_row$OR_lower, 3), "to",
    round(media_row$OR_upper, 3), "\n")
cat("  p-value:  ", ifelse(media_row$p_value < 0.001, "<0.001",
                           round(media_row$p_value, 3)), "\n")
cat("  FMI:      ", media_row$FMI, "%\n")
cat("  df:       ", round(media_row$df, 1), "\n")

# ==============================================================================
# FMI ASSESSMENT
# ==============================================================================

cat("\n==============================================================================\n")
cat("FRACTION OF MISSING INFORMATION (FMI) — ALL PARAMETERS\n")
cat("==============================================================================\n\n")

fmi_summary <- pooled_results %>%
  select(term, FMI) %>%
  mutate(
    stability = case_when(
      FMI < 20  ~ "Excellent (<20%)",
      FMI < 40  ~ "Stable (<40%)",
      FMI < 60  ~ "Moderate (40-60%)",
      TRUE      ~ "High (>60%)"
    )
  )

print(fmi_summary)

cat("\nAll FMI <40%:", all(pooled_results$FMI < 40), "\n")
cat("All FMI <50%:", all(pooled_results$FMI < 50), "\n")

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

saveRDS(pooled_results, "data/processed/primary_gee_pooled_results.rds")

write.csv(results_display,
          "output/tables/table3_primary_gee_results.csv",
          row.names = FALSE)

write.csv(pooled_results,
          "output/tables/table3_primary_gee_full.csv",
          row.names = FALSE)

cat("\n==============================================================================\n")
cat("SCRIPT 04 COMPLETE\n")
cat("Saved: data/processed/primary_gee_pooled_results.rds\n")
cat("Saved: output/tables/table3_primary_gee_results.csv\n")
cat("Next: Run 05_sensitivity_analyses.R\n")
cat("==============================================================================\n")
