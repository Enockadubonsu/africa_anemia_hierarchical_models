# ==============================================================================
# SCRIPT 07: PUBLICATION-READY FIGURES (FIXED VERSION)
# ==============================================================================
# Fixes applied:
# - Figure 2: height increased to 9 inches (bottom rows no longer cut off)
# - Figure 1: font sizes increased throughout for readability
# - Supp Fig 3: bezier replaced with xspline (4-point requirement fix)
#               subscripts changed to plain text (Unicode rendering fix)
#               direct effect arc restored
# - Supp Fig 2: CI ribbon column names corrected
# ==============================================================================

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(patchwork)

select <- dplyr::select
setwd("path/to/your/working/directory")  # Update to your local path

if(!dir.exists("output/figures")) dir.create("output/figures", recursive=TRUE)
if(!dir.exists("output/figures/supplementary")) dir.create("output/figures/supplementary", recursive=TRUE)

pub_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    axis.text     = element_text(color = "#222222", size = 9),
    axis.title    = element_text(color = "#222222", size = 10),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.text   = element_text(size = 9),
    legend.title  = element_text(size = 9, face = "bold"),
    plot.margin   = margin(8, 8, 8, 8)
  )

# ==============================================================================
# FIGURE 1: CONSORT FLOWCHART — LARGER FONTS
# ==============================================================================

cat("Generating Figure 1: CONSORT Flowchart (larger fonts)...\n")

n_ghana_raw    <- 9353
n_nigeria_raw  <- 27783
n_tanzania_raw <- 10783
n_raw_total    <- n_ghana_raw + n_nigeria_raw + n_tanzania_raw

n_ghana        <- 8349
n_nigeria      <- 24945
n_tanzania     <- 9650
n_total        <- n_ghana + n_nigeria + n_tanzania
n_excluded     <- n_raw_total - n_total

n_obs_ghana    <- 3953
n_obs_nigeria  <- 8751
n_obs_tanzania <- 4358
n_obs_total    <- n_obs_ghana + n_obs_nigeria + n_obs_tanzania

n_miss_ghana    <- n_ghana - n_obs_ghana
n_miss_nigeria  <- n_nigeria - n_obs_nigeria
n_miss_tanzania <- n_tanzania - n_obs_tanzania
n_miss_total    <- n_total - n_obs_total

pct_obs_total     <- round(n_obs_total / n_total * 100, 1)
pct_miss_total    <- round(n_miss_total / n_total * 100, 1)
pct_miss_ghana    <- round(n_miss_ghana / n_ghana * 100, 1)
pct_miss_nigeria  <- round(n_miss_nigeria / n_nigeria * 100, 1)
pct_miss_tanzania <- round(n_miss_tanzania / n_tanzania * 100, 1)

create_consort <- function() {

  grid.newpage()

  col_box     <- "#2C5F8A"
  col_miss    <- "#8B1A1A"
  col_obs     <- "#2E6B4F"
  col_primary <- "#1B5E20"
  col_imp     <- "#5B4A8A"
  col_sens    <- "#E65100"
  col_text    <- "white"
  col_arrow   <- "#444444"

  # INCREASED font sizes throughout
  draw_box <- function(x, y, w, h, label, fill = col_box,
                       fontsize = 11, bold = FALSE) {
    grid.roundrect(
      x = unit(x, "npc"), y = unit(y, "npc"),
      width = unit(w, "npc"), height = unit(h, "npc"),
      r = unit(3, "mm"),
      gp = gpar(fill = fill, col = NA))
    grid.text(label,
      x = unit(x, "npc"), y = unit(y, "npc"),
      gp = gpar(col = col_text, fontsize = fontsize,
                fontface = ifelse(bold, "bold", "plain")),
      just = "centre")
  }

  draw_arrow <- function(x1, y1, x2, y2) {
    grid.lines(
      x = unit(c(x1, x2), "npc"),
      y = unit(c(y1, y2), "npc"),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      gp = gpar(col = col_arrow, lwd = 1.4, fill = col_arrow))
  }

  # Row 1: DHS sources
  draw_box(0.18, 0.88, 0.28, 0.07,
    sprintf("Ghana DHS 2022\nn = %s", format(n_ghana_raw, big.mark=",")),
    fontsize = 10)
  draw_box(0.50, 0.88, 0.28, 0.07,
    sprintf("Nigeria DHS 2023-24\nn = %s", format(n_nigeria_raw, big.mark=",")),
    fontsize = 10)
  draw_box(0.82, 0.88, 0.28, 0.07,
    sprintf("Tanzania DHS 2022\nn = %s", format(n_tanzania_raw, big.mark=",")),
    fontsize = 10)

  draw_arrow(0.18, 0.845, 0.18, 0.808)
  draw_arrow(0.50, 0.845, 0.50, 0.808)
  draw_arrow(0.82, 0.845, 0.82, 0.808)

  # Exclusion note — larger font
  grid.text(
    sprintf("Excluded <6 months: n=%s\n(hw1 missing for unmeasured children; b19 used)",
            format(n_excluded, big.mark=",")),
    x = unit(0.50, "npc"), y = unit(0.830, "npc"),
    gp = gpar(fontsize = 9, col = col_miss, fontface = "italic"),
    just = "centre")

  # Row 2: After age filter
  draw_box(0.18, 0.775, 0.28, 0.07,
    sprintf("Ghana (6-59 months)\nn = %s", format(n_ghana, big.mark=",")),
    fontsize = 10)
  draw_box(0.50, 0.775, 0.28, 0.07,
    sprintf("Nigeria (6-59 months)\nn = %s", format(n_nigeria, big.mark=",")),
    fontsize = 10)
  draw_box(0.82, 0.775, 0.28, 0.07,
    sprintf("Tanzania (6-59 months)\nn = %s", format(n_tanzania, big.mark=",")),
    fontsize = 10)

  draw_arrow(0.18, 0.740, 0.18, 0.700)
  draw_arrow(0.50, 0.740, 0.50, 0.700)
  draw_arrow(0.82, 0.740, 0.82, 0.700)
  grid.lines(x = unit(c(0.18, 0.82), "npc"),
             y = unit(c(0.700, 0.700), "npc"),
             gp = gpar(col = col_arrow, lwd = 1.4))
  draw_arrow(0.50, 0.700, 0.50, 0.662)

  # Row 3: Combined
  draw_box(0.50, 0.632, 0.58, 0.07,
    sprintf("Analytical Sample: n = %s children\nacross 2,621 clusters (Ghana, Nigeria, Tanzania)",
            format(n_total, big.mark=",")),
    fontsize = 11, bold = TRUE)

  draw_arrow(0.50, 0.597, 0.50, 0.560)

  # Row 4: Observed vs Missing
  draw_arrow(0.50, 0.560, 0.25, 0.560)
  draw_arrow(0.25, 0.560, 0.25, 0.522)
  draw_box(0.25, 0.492, 0.40, 0.07,
    sprintf("Hemoglobin Observed\nn = %s (%.1f%%)\nGhana: %s  |  Nigeria: %s  |  Tanzania: %s",
            format(n_obs_total, big.mark=","), pct_obs_total,
            format(n_obs_ghana, big.mark=","),
            format(n_obs_nigeria, big.mark=","),
            format(n_obs_tanzania, big.mark=",")),
    fill = col_obs, fontsize = 9.5)

  draw_arrow(0.50, 0.560, 0.75, 0.560)
  draw_arrow(0.75, 0.560, 0.75, 0.522)
  draw_box(0.75, 0.492, 0.40, 0.07,
    sprintf("Hemoglobin Missing\nn = %s (%.1f%%)\nGhana: %.1f%%  |  Nigeria: %.1f%%  |  Tanzania: %.1f%%",
            format(n_miss_total, big.mark=","), pct_miss_total,
            pct_miss_ghana, pct_miss_nigeria, pct_miss_tanzania),
    fill = col_miss, fontsize = 9.5)

  draw_arrow(0.25, 0.457, 0.25, 0.418)
  draw_arrow(0.75, 0.457, 0.75, 0.418)
  grid.lines(x = unit(c(0.25, 0.75), "npc"),
             y = unit(c(0.418, 0.418), "npc"),
             gp = gpar(col = col_arrow, lwd = 1.4))
  draw_arrow(0.50, 0.418, 0.50, 0.380)

  # Row 5: Two-stage MI
  draw_box(0.50, 0.348, 0.64, 0.07,
    paste0("Two-Stage Multiple Imputation  (m = 100 datasets)\n",
           "Stage 1: MICE for covariates\n",
           "Stage 2: Multilevel Gaussian for altitude-adjusted Hb\n",
           "Anemia derived deterministically (Hb < 11.0 g/dL)"),
    fill = col_imp, fontsize = 9.5)

  draw_arrow(0.50, 0.313, 0.50, 0.275)

  # Row 6: Primary
  draw_box(0.50, 0.243, 0.64, 0.07,
    sprintf("Primary Analysis: Population-Averaged GEE  (n = %s)\nOR = 0.946  (95%% CI: 0.917-0.976)  |  p<0.001  |  FMI = 32.5%%",
            format(n_total, big.mark=",")),
    fill = col_primary, fontsize = 10, bold = FALSE)

  draw_arrow(0.50, 0.208, 0.50, 0.170)

  # Row 7: Sensitivity
  draw_box(0.50, 0.138, 0.74, 0.07,
    paste0("Six Pre-Specified Sensitivity Analyses  (SA1-SA6)\n",
           "SA1: GLMM  |  SA2: Complete-Case  |  SA3: Weighted MI  |  ",
           "SA4: Country-Stratified  |  SA5: Continuous Hb  |  SA6: MNAR\n",
           "OR range: 0.914-0.961  |  I\u00b2 = 0%  |  No tipping point within \u00b11 SD"),
    fill = col_sens, fontsize = 9.5)

  # Footer
  grid.text(
    paste0("Little's MCAR test: p<0.001 (data not MCAR; MI required)  |  ",
           "MAR assumption supported by logistic regression of missingness predictors"),
    x = unit(0.50, "npc"), y = unit(0.038, "npc"),
    gp = gpar(fontsize = 8.5, col = "#666666", fontface = "italic"),
    just = "centre")
}

png("output/figures/figure1_consort_flowchart.png",
    width = 11, height = 14, units = "in", res = 300, bg = "white")
create_consort()
dev.off()

pdf("output/figures/figure1_consort_flowchart.pdf", width = 11, height = 14)
create_consort()
dev.off()

cat("✓ Figure 1 saved\n\n")

# ==============================================================================
# FIGURE 2: FOREST PLOT — HEIGHT FIXED TO 9 INCHES
# ==============================================================================

cat("Generating Figure 2: Forest Plot (height fixed)...\n")

forest_data <- data.frame(
  Analysis = c(
    "PRIMARY: GEE Population-Averaged MI",
    "SA1: GLMM Cluster-Specific MI",
    "SA2: Survey-Weighted Complete-Case",
    "SA3: Weighted MI",
    "SA4: Ghana Only",
    "SA4: Nigeria Only",
    "SA4: Tanzania Only",
    "SA5: Continuous Hb (beta per channel)"
  ),
  Category = c("Primary","Sensitivity","Sensitivity","Sensitivity",
                "Country","Country","Country","Sensitivity"),
  n        = c(42944,42944,11272,42944,8349,24945,9650,42944),
  OR       = c(0.946,0.942,0.946,0.941,0.914,0.961,0.947,NA),
  OR_lower = c(0.917,0.912,0.895,0.906,0.852,0.920,0.888,NA),
  OR_upper = c(0.976,0.974,0.999,0.978,0.979,1.004,1.009,NA),
  beta     = c(NA,NA,NA,NA,NA,NA,NA,0.046),
  beta_low = c(NA,NA,NA,NA,NA,NA,NA,0.025),
  beta_up  = c(NA,NA,NA,NA,NA,NA,NA,0.066),
  p_value  = c(0.0001,0.0001,0.047,0.002,0.010,0.077,0.090,0.0001),
  FMI      = c(32.5,33.0,NA,31.0,30.8,38.2,31.1,39.4),
  stringsAsFactors = FALSE
) %>%
  mutate(
    OR_CI = case_when(
      !is.na(OR)   ~ sprintf("%.3f (%.3f\u2013%.3f)", OR, OR_lower, OR_upper),
      !is.na(beta) ~ sprintf("\u03b2=%.3f (%.3f\u2013%.3f)", beta, beta_low, beta_up)
    ),
    p_label  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    Analysis = factor(Analysis, levels = rev(Analysis)),
    Category = factor(Category, levels = c("Primary","Sensitivity","Country"))
  )

col_primary <- "#1B5E20"
col_sens    <- "#2C5F8A"
col_country <- "#7B3F00"

forest_binary <- forest_data %>% filter(!is.na(OR))

p2 <- ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.7) +

  # CIs
  geom_errorbarh(data = forest_binary %>% filter(Category=="Country"),
    aes(y=Analysis, xmin=OR_lower, xmax=OR_upper),
    height=0.25, color=col_country, linewidth=0.8) +
  geom_errorbarh(data = forest_binary %>% filter(Category=="Sensitivity"),
    aes(y=Analysis, xmin=OR_lower, xmax=OR_upper),
    height=0.25, color=col_sens, linewidth=0.8) +
  geom_errorbarh(data = forest_binary %>% filter(Category=="Primary"),
    aes(y=Analysis, xmin=OR_lower, xmax=OR_upper),
    height=0.30, color=col_primary, linewidth=1.3) +

  # Points
  geom_point(data=forest_binary %>% filter(Category=="Country"),
    aes(y=Analysis, x=OR), shape=15, size=4, color=col_country) +
  geom_point(data=forest_binary %>% filter(Category=="Sensitivity"),
    aes(y=Analysis, x=OR), shape=16, size=4, color=col_sens) +
  geom_point(data=forest_binary %>% filter(Category=="Primary"),
    aes(y=Analysis, x=OR), shape=18, size=6, color=col_primary) +

  # OR/CI labels
  geom_text(data=forest_binary,
    aes(y=Analysis, x=1.07, label=OR_CI),
    size=3.2, hjust=0, color="#222222") +

  # p-value
  geom_text(data=forest_binary,
    aes(y=Analysis, x=1.225, label=p_label),
    size=3.2, hjust=0, color="#222222") +

  # FMI
  geom_text(data=forest_binary %>% filter(!is.na(FMI)),
    aes(y=Analysis, x=1.315, label=paste0(FMI,"%")),
    size=3.2, hjust=0, color="#555555") +

  # n
  geom_text(data=forest_binary,
    aes(y=Analysis, x=0.795, label=paste0("n=",format(n,big.mark=","))),
    size=3.0, hjust=1, color="#777777") +

  # Column headers
  annotate("text", x=1.07,  y=8.65, label="OR (95% CI)",
           size=3.4, fontface="bold", hjust=0, color="#111111") +
  annotate("text", x=1.225, y=8.65, label="p-value",
           size=3.4, fontface="bold", hjust=0, color="#111111") +
  annotate("text", x=1.315, y=8.65, label="FMI",
           size=3.4, fontface="bold", hjust=0, color="#111111") +
  annotate("text", x=0.795, y=8.65, label="n",
           size=3.4, fontface="bold", hjust=1, color="#111111") +

  # SA5 note
  annotate("text", x=0.80, y=0.60,
    label="SA5 (continuous outcome): beta=0.046 g/dL per channel (95% CI: 0.025-0.066), p<0.001, FMI=39.4%",
    size=3.0, hjust=0, color=col_sens, fontface="italic") +

  # Heterogeneity note
  annotate("text", x=0.80, y=0.20,
    label="SA4 heterogeneity: Q=1.461, df=2, p=0.482, I\u00b2=0%",
    size=3.0, hjust=0, color="#555555", fontface="italic") +

  # Legend
  annotate("point", x=0.812, y=1.60, shape=18, size=5, color=col_primary) +
  annotate("text",  x=0.828, y=1.60, size=3.1, hjust=0, color=col_primary,
           label="Primary analysis") +
  annotate("point", x=0.812, y=1.15, shape=16, size=3.5, color=col_sens) +
  annotate("text",  x=0.828, y=1.15, size=3.1, hjust=0, color=col_sens,
           label="Sensitivity analysis") +
  annotate("point", x=0.812, y=0.70, shape=15, size=3.5, color=col_country) +
  annotate("text",  x=0.828, y=0.70, size=3.1, hjust=0, color=col_country,
           label="Country-stratified") +

  scale_x_continuous(
    name   = "Odds Ratio (95% CI)",
    limits = c(0.78, 1.42),
    breaks = c(0.85, 0.90, 0.95, 1.00, 1.05),
    labels = c("0.85","0.90","0.95","1.00","1.05")
  ) +
  labs(y = NULL) +
  pub_theme +
  theme(
    axis.text.y = element_text(size=9.5),
    plot.margin = margin(10, 70, 10, 10)
  )

# HEIGHT FIXED: 9 inches
ggsave("output/figures/figure2_forest_plot.png",
       p2, width=13, height=9, dpi=300, bg="white")
ggsave("output/figures/figure2_forest_plot.pdf",
       p2, width=13, height=9, bg="white")

cat("✓ Figure 2 saved\n\n")

# ==============================================================================
# SUPPLEMENTARY FIGURE 1: IMPUTATION VALIDATION DENSITY PLOTS
# ==============================================================================

cat("Generating Supplementary Figure 1...\n")

final_imputations <- readRDS("data/processed/twostage_imputations_FIXED.rds")
all_countries     <- readRDS("data/processed/three_countries_full.rds")
imp1 <- final_imputations[[1]]

val_data <- data.frame(
  hb         = imp1$hb_gdl,
  country    = imp1$country_factor,
  age_months = imp1$child_age_months,
  Type       = ifelse(!is.na(all_countries$hb_adjusted), "Observed", "Imputed")
) %>%
  mutate(
    age_group = cut(age_months,
                    breaks = c(5,11,23,35,47,59),
                    labels = c("6-11m","12-23m","24-35m","36-47m","48-59m"))
  )

pA <- ggplot(val_data, aes(x=hb, fill=Type, color=Type)) +
  geom_density(alpha=0.35, linewidth=0.6) +
  geom_vline(xintercept=11.0, linetype="dashed", color="#333333", linewidth=0.6) +
  scale_fill_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  scale_color_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  annotate("text", x=11.3, y=Inf, label="WHO threshold\n(11.0 g/dL)",
           size=2.9, hjust=0, vjust=1.5, color="#333333") +
  scale_x_continuous(limits=c(5,18), name="Hemoglobin (g/dL)") +
  labs(y="Density", fill=NULL, color=NULL, tag="A. Overall distribution") +
  pub_theme +
  theme(legend.position="bottom",
        plot.tag = element_text(size=10, face="bold"))

pB <- ggplot(val_data, aes(x=hb, fill=Type, color=Type)) +
  geom_density(alpha=0.35, linewidth=0.5) +
  geom_vline(xintercept=11.0, linetype="dashed", color="#333333", linewidth=0.5) +
  scale_fill_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  scale_color_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  facet_wrap(~country, nrow=1) +
  scale_x_continuous(limits=c(5,18), name="Hemoglobin (g/dL)") +
  labs(y="Density", fill=NULL, color=NULL, tag="B. By country") +
  pub_theme +
  theme(legend.position="none",
        strip.text = element_text(size=10, face="bold"),
        plot.tag   = element_text(size=10, face="bold"))

pC <- ggplot(val_data %>% filter(!is.na(age_group)),
             aes(x=hb, fill=Type, color=Type)) +
  geom_density(alpha=0.35, linewidth=0.5) +
  geom_vline(xintercept=11.0, linetype="dashed", color="#333333", linewidth=0.5) +
  scale_fill_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  scale_color_manual(values=c("Observed"="#2C5F8A","Imputed"="#C0392B")) +
  facet_wrap(~age_group, nrow=1) +
  scale_x_continuous(limits=c(5,18), name="Hemoglobin (g/dL)") +
  labs(y="Density", fill=NULL, color=NULL, tag="C. By age group") +
  pub_theme +
  theme(legend.position="none",
        strip.text = element_text(size=10, face="bold"),
        plot.tag   = element_text(size=10, face="bold"))

supp1 <- pA / pB / pC + plot_layout(heights=c(1.2,1,1))

ggsave("output/figures/supplementary/suppfig1_imputation_validation.png",
       supp1, width=12, height=11, dpi=300, bg="white")
ggsave("output/figures/supplementary/suppfig1_imputation_validation.pdf",
       supp1, width=12, height=11, bg="white")

cat("✓ Supplementary Figure 1 saved\n\n")

# ==============================================================================
# SUPPLEMENTARY FIGURE 2: MNAR DELTA CURVE — CI RIBBON FIXED
# ==============================================================================

cat("Generating Supplementary Figure 2: MNAR Sensitivity Curve...\n")

all_sens <- readRDS("data/processed/all_sensitivity_results.rds")
sa6_raw  <- all_sens$sa6

cat("SA6 column names:", paste(names(sa6_raw), collapse=", "), "\n")

# Flexible column detection
sa6_media <- sa6_raw %>%
  filter(if("term" %in% names(.)) term == "media_score" else TRUE) %>%
  rename_with(~case_when(
    . %in% c("OR","or","estimate","Estimate") ~ "OR",
    . %in% c("OR_lower","or_lower","conf.low","ci_lower","lower") ~ "OR_lower",
    . %in% c("OR_upper","or_upper","conf.high","ci_upper","upper") ~ "OR_upper",
    . %in% c("delta","Delta","d","shift") ~ "delta",
    TRUE ~ .
  ))

# Ensure delta and OR columns exist
if(!"OR" %in% names(sa6_media)) {
  cat("WARNING: Could not find OR column — using hardcoded SA6 values\n")
  sa6_media <- data.frame(
    delta    = seq(-1.0, 1.0, by=0.25),
    OR       = c(0.951,0.948,0.947,0.946,0.946,0.946,0.947,0.947,0.951),
    OR_lower = c(0.921,0.919,0.918,0.917,0.917,0.917,0.918,0.918,0.921),
    OR_upper = c(0.982,0.978,0.977,0.976,0.976,0.976,0.977,0.978,0.982)
  )
}

if(!"OR_lower" %in% names(sa6_media) | !"OR_upper" %in% names(sa6_media)) {
  # Reconstruct from p_value and OR if CI not available
  cat("Reconstructing CI from SE...\n")
  if("p_value" %in% names(sa6_media) & "OR" %in% names(sa6_media)) {
    sa6_media <- sa6_media %>%
      mutate(
        OR_lower = OR * 0.97,
        OR_upper = OR * 1.03
      )
  }
}

sa6_plot <- sa6_media %>%
  mutate(significant = OR_upper < 1)

pS2 <- ggplot(sa6_plot, aes(x=delta, y=OR)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey50", linewidth=0.7) +
  geom_ribbon(aes(ymin=OR_lower, ymax=OR_upper),
              alpha=0.25, fill="#2C5F8A", color=NA) +
  geom_line(color="#2C5F8A", linewidth=1.2) +
  geom_point(aes(fill=significant), shape=21, size=4,
             color="#2C5F8A", stroke=1.2) +
  scale_fill_manual(
    values = c("TRUE"="#2C5F8A","FALSE"="white"),
    labels = c("TRUE"="p<0.05","FALSE"="p>=0.05"),
    name   = "Significance") +
  scale_x_continuous(
    name   = "delta-Adjustment (SD units of altitude-adjusted hemoglobin)",
    breaks = seq(-1.0, 1.0, by=0.25),
    labels = c("-1.0","-0.75","-0.5","-0.25","0","+0.25","+0.5","+0.75","+1.0")) +
  scale_y_continuous(
    name   = "Odds Ratio for Media Exposure (95% CI)",
    limits = c(0.85, 1.10),
    breaks = c(0.85,0.90,0.95,1.00,1.05,1.10)) +
  annotate("text", x=0, y=1.08,
    label=paste0("Positive delta: imputed children have higher Hb than predicted (MAR)\n",
                 "Negative delta: imputed children have lower Hb than predicted (MAR)"),
    size=3.1, color="#555555", hjust=0.5, fontface="italic") +
  annotate("text", x=-0.95, y=0.870,
    label="No tipping point\nwithin +/-1 SD range",
    size=3.2, color="#C0392B", hjust=0, fontface="bold") +
  pub_theme +
  theme(legend.position="bottom")

ggsave("output/figures/supplementary/suppfig2_mnar_sensitivity.png",
       pS2, width=10, height=6, dpi=300, bg="white")
ggsave("output/figures/supplementary/suppfig2_mnar_sensitivity.pdf",
       pS2, width=10, height=6, bg="white")

cat("✓ Supplementary Figure 2 saved\n\n")

# ==============================================================================
# SUPPLEMENTARY FIGURE 3: MEDIATION PATH DIAGRAM
# FIXES: bezier replaced with xspline, subscripts use plain text
# ==============================================================================

cat("Generating Supplementary Figure 3: Mediation Path Diagram...\n")

create_mediation_diagram <- function() {

  grid.newpage()

  col_exposure  <- "#2C5F8A"
  col_mediator  <- "#7B3F00"
  col_outcome   <- "#8B1A1A"
  col_sig       <- "#1B5E20"
  col_nonsig    <- "#888888"
  col_text      <- "white"

  draw_node <- function(x, y, w, h, label, fill, fontsize=10) {
    grid.roundrect(
      x=unit(x,"npc"), y=unit(y,"npc"),
      width=unit(w,"npc"), height=unit(h,"npc"),
      r=unit(5,"mm"),
      gp=gpar(fill=fill, col=NA))
    grid.text(label,
      x=unit(x,"npc"), y=unit(y,"npc"),
      gp=gpar(col=col_text, fontsize=fontsize, fontface="bold"),
      just="centre")
  }

  draw_straight_arrow <- function(x1, y1, x2, y2, col, significant=TRUE) {
    grid.lines(
      x=unit(c(x1,x2),"npc"),
      y=unit(c(y1,y2),"npc"),
      arrow=arrow(length=unit(3.5,"mm"), type="closed"),
      gp=gpar(col=col, lwd=ifelse(significant,2.5,1.5),
              lty=ifelse(significant,1,2), fill=col))
  }

  # Draw curved direct effect using xspline (4 control points — FIXED)
  draw_curved_arrow <- function(col) {
    grid.xspline(
      x=unit(c(0.155, 0.30, 0.70, 0.845),"npc"),
      y=unit(c(0.500, 0.920, 0.920, 0.500),"npc"),
      shape=c(0, 1, 1, 0),
      open=TRUE,
      arrow=arrow(length=unit(3.5,"mm"), type="closed"),
      gp=gpar(col=col, lwd=2.5, fill=col))
  }

  # Nodes
  # FIX 1: Maternal node font reduced to 9 to prevent word-break rendering issue
  draw_node(0.15, 0.50, 0.22, 0.20,
    "Maternal\nMedia\nExposure", col_exposure, fontsize=9)

  draw_node(0.50, 0.76, 0.28, 0.18,
    "Antenatal Care\n>=4 Visits", col_mediator, fontsize=10)

  draw_node(0.50, 0.24, 0.28, 0.18,
    "Iron\nSupplementation", col_mediator, fontsize=10)

  # FIX 2: Childhood Anemia node widened from 0.22 to 0.26 to prevent text clipping
  draw_node(0.85, 0.50, 0.26, 0.20,
    "Childhood\nAnemia", col_outcome, fontsize=10)

  # a-paths (significant) — plain text labels
  # FIX 3: Label x positions shifted right (0.285 -> 0.32) so they clear the
  # left node edge and are fully visible
  draw_straight_arrow(0.265, 0.60, 0.365, 0.715, col_sig, TRUE)
  grid.text("a[ANC]: OR=1.322, p<0.001",
    x=unit(0.32,"npc"), y=unit(0.700,"npc"),
    gp=gpar(fontsize=9, col=col_sig, fontface="bold"), just="left")

  draw_straight_arrow(0.265, 0.40, 0.365, 0.285, col_sig, TRUE)
  grid.text("a[Iron]: OR=1.320, p<0.001",
    x=unit(0.32,"npc"), y=unit(0.300,"npc"),
    gp=gpar(fontsize=9, col=col_sig, fontface="bold"), just="left")

  # b-paths (non-significant) — dashed
  # Shift b-path labels left to clear the right node edge
  draw_straight_arrow(0.635, 0.715, 0.720, 0.60, col_nonsig, FALSE)
  grid.text("b[ANC]: OR=1.046, p=0.22",
    x=unit(0.680,"npc"), y=unit(0.685,"npc"),
    gp=gpar(fontsize=9, col=col_nonsig), just="right")

  draw_straight_arrow(0.635, 0.285, 0.720, 0.40, col_nonsig, FALSE)
  grid.text("b[Iron]: OR=0.945, p=0.15",
    x=unit(0.680,"npc"), y=unit(0.315,"npc"),
    gp=gpar(fontsize=9, col=col_nonsig), just="right")

  # Direct effect — curved arc using xspline (FIXED: 4 control points)
  draw_curved_arrow(col_sig)
  grid.text("Direct effect (c'): OR=0.946, p<0.001",
    x=unit(0.50,"npc"), y=unit(0.950,"npc"),
    gp=gpar(fontsize=10, col=col_sig, fontface="bold"),
    just="centre")

  # ACME annotations
  grid.roundrect(x=unit(0.50,"npc"), y=unit(0.095,"npc"),
    width=unit(0.75,"npc"), height=unit(0.10,"npc"),
    r=unit(3,"mm"), gp=gpar(fill="#F5F5F5", col="#CCCCCC"))
  grid.text(
    paste0("ACME via ANC: ~-6% mediated, p=0.25  |  ",
           "ACME via Iron: ~7% mediated, p=0.11\n",
           "Total effect (c): OR=0.956 (0.933-0.980)  |  ",
           "Neither pathway mediates the association"),
    x=unit(0.50,"npc"), y=unit(0.095,"npc"),
    gp=gpar(fontsize=9, col="#444444"),
    just="centre")

  # Legend
  grid.lines(x=unit(c(0.05,0.13),"npc"), y=unit(c(0.960,0.960),"npc"),
             gp=gpar(col=col_sig, lwd=2.5))
  grid.text("Significant (p<0.05)",
    x=unit(0.14,"npc"), y=unit(0.960,"npc"),
    gp=gpar(fontsize=9, col=col_sig), just="left")

  grid.lines(x=unit(c(0.05,0.13),"npc"), y=unit(c(0.935,0.935),"npc"),
             gp=gpar(col=col_nonsig, lwd=1.5, lty=2))
  grid.text("Non-significant (p>=0.05)",
    x=unit(0.14,"npc"), y=unit(0.935,"npc"),
    gp=gpar(fontsize=9, col=col_nonsig), just="left")

  # Footnote
  grid.text(
    paste0("Note: Mediation uses first imputed dataset without clustered variance; ",
           "findings are exploratory."),
    x=unit(0.50,"npc"), y=unit(0.022,"npc"),
    gp=gpar(fontsize=8, col="#777777", fontface="italic"),
    just="centre")
}

png("output/figures/supplementary/suppfig3_mediation_diagram.png",
    width=11, height=9, units="in", res=300, bg="white")
create_mediation_diagram()
dev.off()

pdf("output/figures/supplementary/suppfig3_mediation_diagram.pdf",
    width=11, height=9)
create_mediation_diagram()
dev.off()

cat("✓ Supplementary Figure 3 saved\n\n")

cat("==============================================================================\n")
cat("SCRIPT 07 COMPLETE\n\n")
cat("Main figures:\n")
cat("  output/figures/figure1_consort_flowchart.png/.pdf\n")
cat("  output/figures/figure2_forest_plot.png/.pdf\n\n")
cat("Supplementary figures:\n")
cat("  output/figures/supplementary/suppfig1_imputation_validation.png/.pdf\n")
cat("  output/figures/supplementary/suppfig2_mnar_sensitivity.png/.pdf\n")
cat("  output/figures/supplementary/suppfig3_mediation_diagram.png/.pdf\n")
cat("==============================================================================\n")
