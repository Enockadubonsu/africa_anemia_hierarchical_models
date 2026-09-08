# ==============================================================================
# SCRIPT 06: MEDIATION ANALYSIS
# ==============================================================================
# Manuscript: Maternal Media Exposure and Childhood Anemia in Sub-Saharan Africa
# Purpose: Exploratory mediation analysis examining whether the association
#          between maternal media exposure and childhood anemia operates through
#          two hypothesized mediators:
#          (1) Adequate antenatal care attendance (>=4 visits)
#          (2) Iron supplementation during pregnancy
# Input:  data/processed/twostage_imputations_FIXED.rds
# Output: output/tables/table5_mediation_analysis.csv
# ==============================================================================
# IMPORTANT LIMITATION NOTE (disclosed in manuscript):
# The mediation package bootstrap estimation does not natively accommodate
# GEE-based clustered variance or pooling across multiply imputed datasets.
# Therefore mediation models are fit on the first imputed dataset only,
# using non-clustered GLMs. This means:
# (1) Total effect estimates here are NOT directly comparable to the primary
#     GEE analysis in terms of statistical significance
# (2) The nearly identical point estimates (OR ~0.967 vs 0.965) are consistent
#     but CIs differ due to the different variance structure
# (3) These findings are EXPLORATORY and hypothesis-generating only
# (4) p-values for total and direct effects are NOT reported to avoid
#     implying an independent significance test of the primary association
# ==============================================================================

source("00_packages_and_setup.R")

final_imputations <- readRDS("data/processed/twostage_imputations_FIXED.rds")
all_countries     <- readRDS("data/processed/three_countries_full.rds")

cat("==============================================================================\n")
cat("MEDIATION ANALYSIS: Media Exposure → Health Behaviors → Anemia\n")
cat("==============================================================================\n\n")
cat("NOTE: This is an exploratory analysis using the first imputed dataset.\n")
cat("See manuscript for full disclosure of this limitation.\n\n")

# ==============================================================================
# PREPARE DATA
# ==============================================================================

# Use first imputed dataset
data_med <- final_imputations[[1]]

# Restore health behavior variables from original data
data_med$anc_visits <- all_countries$anc_visits
data_med$iron_supp  <- all_countries$iron_supp

# Create binary mediators
data_med <- data_med %>%
  mutate(
    anc_4plus = as.numeric(anc_visits >= 4),
    iron_any  = as.numeric(iron_supp == 1)
  )

cat("Sample size:", nrow(data_med), "\n")
cat("Anemia prevalence:", round(mean(data_med$anemia) * 100, 1), "%\n\n")

cat("MEDIATOR DISTRIBUTIONS:\n")
cat("  ANC 4+ visits:",
    round(mean(data_med$anc_4plus, na.rm = TRUE) * 100, 1), "%\n")
cat("  Iron supplementation:",
    round(mean(data_med$iron_any, na.rm = TRUE) * 100, 1), "%\n\n")

# Covariates for all mediation models
covariates <- c(
  "child_age_months", "child_sex_binary", "birth_order",
  "maternal_age", "maternal_education_years",
  "wealth_index", "country_factor"
)

# ==============================================================================
# PATHWAY 1: ANTENATAL CARE
# ==============================================================================

cat("PATHWAY 1: Media Exposure → ANC Attendance → Anemia\n")
cat("------------------------------------------------------\n\n")

# a path: Media → ANC 4+ visits
med_anc_model <- glm(
  anc_4plus ~ media_score + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + country_factor,
  data   = data_med,
  family = binomial(link = "logit")
)

# b path / total: Media → Anemia (without mediator)
total_model <- glm(
  anemia ~ media_score + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + country_factor,
  data   = data_med,
  family = binomial(link = "logit")
)

# c' path: Media → Anemia (with mediator)
direct_anc_model <- glm(
  anemia ~ media_score + anc_4plus + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + country_factor,
  data   = data_med,
  family = binomial(link = "logit")
)

# Formal mediation using mediation package
cat("Running bootstrap mediation (1,000 simulations)...\n")
set.seed(123456)

med_anc <- mediate(
  model.m  = med_anc_model,
  model.y  = direct_anc_model,
  treat    = "media_score",
  mediator = "anc_4plus",
  boot     = TRUE,
  sims     = 1000
)

# Extract results
total_coef <- broom::tidy(total_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "media_score")

a_coef <- broom::tidy(med_anc_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "media_score")

b_coef <- broom::tidy(direct_anc_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "anc_4plus")

direct_coef <- broom::tidy(direct_anc_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "media_score")

cat("\nANC Pathway Results:\n")
cat("  a path (Media → ANC): OR =", round(a_coef$estimate, 3),
    "(", round(a_coef$conf.low, 3), "-", round(a_coef$conf.high, 3), ")",
    "p =", round(a_coef$p.value, 3), "\n")
cat("  b path (ANC → Anemia): OR =", round(b_coef$estimate, 3),
    "(", round(b_coef$conf.low, 3), "-", round(b_coef$conf.high, 3), ")",
    "p =", round(b_coef$p.value, 3), "\n")
cat("  Total effect OR:", round(total_coef$estimate, 3), "\n")
cat("  Direct effect OR:", round(direct_coef$estimate, 3), "\n")
cat("  ACME (indirect):", round(med_anc$d.avg, 6), "\n")
cat("  ACME 95% CI: (", round(med_anc$d.avg.ci[1], 6), "-",
    round(med_anc$d.avg.ci[2], 6), ")\n")
cat("  Proportion mediated:", round(med_anc$n.avg * 100, 1), "%\n")
cat("  ACME p-value:", round(med_anc$d.avg.p, 3), "\n\n")

# ==============================================================================
# PATHWAY 2: IRON SUPPLEMENTATION
# ==============================================================================

cat("PATHWAY 2: Media Exposure → Iron Supplementation → Anemia\n")
cat("-----------------------------------------------------------\n\n")

# a path: Media → Iron supplementation
med_iron_model <- glm(
  iron_any ~ media_score + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + country_factor,
  data   = data_med,
  family = binomial(link = "logit")
)

# c' path: Media → Anemia (with iron mediator)
direct_iron_model <- glm(
  anemia ~ media_score + iron_any + child_age_months + child_sex_binary +
    birth_order + maternal_age + maternal_education_years +
    wealth_index + country_factor,
  data   = data_med,
  family = binomial(link = "logit")
)

cat("Running bootstrap mediation (1,000 simulations)...\n")
set.seed(123456)

med_iron <- mediate(
  model.m  = med_iron_model,
  model.y  = direct_iron_model,
  treat    = "media_score",
  mediator = "iron_any",
  boot     = TRUE,
  sims     = 1000
)

a_iron <- broom::tidy(med_iron_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "media_score")

b_iron <- broom::tidy(direct_iron_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "iron_any")

direct_iron_coef <- broom::tidy(direct_iron_model, conf.int = TRUE,
                                 exponentiate = TRUE) %>%
  filter(term == "media_score")

cat("\nIron Supplementation Pathway Results:\n")
cat("  a path (Media → Iron): OR =", round(a_iron$estimate, 3),
    "(", round(a_iron$conf.low, 3), "-", round(a_iron$conf.high, 3), ")",
    "p =", round(a_iron$p.value, 3), "\n")
cat("  b path (Iron → Anemia): OR =", round(b_iron$estimate, 3),
    "(", round(b_iron$conf.low, 3), "-", round(b_iron$conf.high, 3), ")",
    "p =", round(b_iron$p.value, 3), "\n")
cat("  Direct effect OR:", round(direct_iron_coef$estimate, 3), "\n")
cat("  ACME (indirect):", round(med_iron$d.avg, 6), "\n")
cat("  ACME 95% CI: (", round(med_iron$d.avg.ci[1], 6), "-",
    round(med_iron$d.avg.ci[2], 6), ")\n")
cat("  Proportion mediated:", round(med_iron$n.avg * 100, 1), "%\n")
cat("  ACME p-value:", round(med_iron$d.avg.p, 3), "\n\n")

# ==============================================================================
# COMPILE TABLE 5
# ==============================================================================

cat("==============================================================================\n")
cat("TABLE 5: MEDIATION ANALYSIS RESULTS\n")
cat("==============================================================================\n\n")

table5 <- data.frame(
  Pathway = c(
    "Antenatal Care Pathway",
    "  Media → ANC ≥4 visits (a path)",
    "  ANC ≥4 visits → Anemia (b path)",
    "  Media → Anemia (total effect, c)",
    "  Media → Anemia (direct effect, c')",
    "  Indirect effect (ACME)",
    "  Proportion mediated",
    "",
    "Iron Supplementation Pathway",
    "  Media → Iron Supplementation (a path)",
    "  Iron Supplementation → Anemia (b path)",
    "  Media → Anemia (direct effect, c')",
    "  Indirect effect (ACME)",
    "  Proportion mediated"
  ),
  Effect_Type = c(
    "", "Exposure-Mediator", "Mediator-Outcome",
    "Total Effect", "Direct Effect", "Mediation", "",
    "",
    "", "Exposure-Mediator", "Mediator-Outcome",
    "Direct Effect", "Mediation", ""
  ),
  OR_or_Coefficient = c(
    "", round(a_coef$estimate, 3), round(b_coef$estimate, 3),
    round(total_coef$estimate, 3), round(direct_coef$estimate, 3),
    round(med_anc$d.avg, 6), paste0(round(med_anc$n.avg * 100, 1), "%"),
    "",
    "", round(a_iron$estimate, 3), round(b_iron$estimate, 3),
    round(direct_iron_coef$estimate, 3),
    round(med_iron$d.avg, 6), paste0(round(med_iron$n.avg * 100, 1), "%")
  ),
  CI_95 = c(
    "",
    sprintf("(%.3f-%.3f)", a_coef$conf.low, a_coef$conf.high),
    sprintf("(%.3f-%.3f)", b_coef$conf.low, b_coef$conf.high),
    sprintf("(%.3f-%.3f)", total_coef$conf.low, total_coef$conf.high),
    sprintf("(%.3f-%.3f)", direct_coef$conf.low, direct_coef$conf.high),
    sprintf("(%.6f to %.6f)", med_anc$d.avg.ci[1], med_anc$d.avg.ci[2]),
    sprintf("(%.1f%% to %.1f%%)", med_anc$n.avg.ci[1]*100, med_anc$n.avg.ci[2]*100),
    "",
    "",
    sprintf("(%.3f-%.3f)", a_iron$conf.low, a_iron$conf.high),
    sprintf("(%.3f-%.3f)", b_iron$conf.low, b_iron$conf.high),
    sprintf("(%.3f-%.3f)", direct_iron_coef$conf.low, direct_iron_coef$conf.high),
    sprintf("(%.6f to %.6f)", med_iron$d.avg.ci[1], med_iron$d.avg.ci[2]),
    sprintf("(%.1f%% to %.1f%%)", med_iron$n.avg.ci[1]*100, med_iron$n.avg.ci[2]*100)
  ),
  p_value = c(
    "",
    ifelse(a_coef$p.value < 0.001, "<0.001", round(a_coef$p.value, 3)),
    round(b_coef$p.value, 3),
    "—",   # Not reported — see manuscript note
    "—",   # Not reported — see manuscript note
    round(med_anc$d.avg.p, 3),
    round(med_anc$n.avg.p, 3),
    "",
    "",
    ifelse(a_iron$p.value < 0.001, "<0.001", round(a_iron$p.value, 3)),
    round(b_iron$p.value, 3),
    "—",   # Not reported
    round(med_iron$d.avg.p, 3),
    round(med_iron$n.avg.p, 3)
  )
)

print(table5)

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

write.csv(table5, "output/tables/table5_mediation_analysis.csv",
          row.names = FALSE)

saveRDS(list(
  anc_pathway  = list(mediation = med_anc,  a = a_coef, b = b_coef,
                      total = total_coef, direct = direct_coef),
  iron_pathway = list(mediation = med_iron, a = a_iron, b = b_iron,
                      direct = direct_iron_coef)
), "data/processed/mediation_results.rds")

cat("\n==============================================================================\n")
cat("SCRIPT 06 COMPLETE\n")
cat("Saved: output/tables/table5_mediation_analysis.csv\n")
cat("Saved: data/processed/mediation_results.rds\n")
cat("\nAll analyses complete.\n")
cat("==============================================================================\n")
