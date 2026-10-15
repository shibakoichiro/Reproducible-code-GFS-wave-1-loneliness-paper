# =============================================================================
# 04_distribution_convergent_validity.R
# Analyses added in response to peer review (complete cases, non-imputed data)
#   - Table S27  : survey-weighted distribution of frequency of loneliness by country
#                  (output/tbl_RR2_1_loneliness_distribution.csv)
#   - Figure S65 : survey-weighted Spearman correlations of frequency of loneliness
#                  with life satisfaction and depressive symptoms, with 95% CIs
#                  (Fisher z); output/fig_RR2_3_forest_combined.pdf
#   - Results text: random-effects pooled correlations
# =============================================================================
source("00_config.R")


packages <- c("survey", "tidyverse", "haven", "flextable", "officer", "metafor")
invisible(lapply(packages, library, character.only = TRUE))

# Import data
df.original <- haven::read_dta(file.path(DATA_DIR, "gfs_all_countries_wave1.dta"))

# Get country labels
countrynumbers <- attr(df.original$COUNTRY, "labels") %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(country.num = 2, COUNTRY = 1)

# Data cleaning
df <- df.original %>%
  select(LONELY, LIFE_SAT, DEPRESSED, COUNTRY, ANNUAL_WEIGHT1, STRATA, PSU) %>%
  mutate(across(c(LONELY, LIFE_SAT, DEPRESSED),
                ~ ifelse(.x %in% c(-98, 98, 99), NA, .x))) %>%
  mutate(LONELY = 10 - LONELY) %>%
  rename(country.num = COUNTRY) %>%
  left_join(countrynumbers, by = "country.num") %>%
  relocate(COUNTRY, .before = country.num)

# Complete case dataset (all three outcome variables non-missing)
df.cc <- df %>%
  filter(complete.cases(LONELY, LIFE_SAT, DEPRESSED))

cat("Total N (complete cases):", nrow(df.cc), "\n")
cat("Number of countries:", n_distinct(df.cc$COUNTRY), "\n")
cat("Countries:", paste(sort(unique(df.cc$COUNTRY)), collapse = ", "), "\n")

# Weighted skewness and kurtosis
weighted_skewness <- function(x, w) {
  wm <- weighted.mean(x, w)
  sw <- sum(w)
  wvar <- sum(w * (x - wm)^2) / sw
  wsd <- sqrt(wvar)
  sum(w * ((x - wm) / wsd)^3) / sw
}

weighted_kurtosis <- function(x, w) {
  wm <- weighted.mean(x, w)
  sw <- sum(w)
  wvar <- sum(w * (x - wm)^2) / sw
  wsd <- sqrt(wvar)
  sum(w * ((x - wm) / wsd)^4) / sw
}

# Compute survey-weighted stats by country
dist_stats <- df.cc %>%
  group_nest(COUNTRY) %>%
  mutate(stats = map(data, function(d) {
    svy <- svydesign(ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA, data = d)

    m  <- svymean(~LONELY, svy)
    v  <- svyvar(~LONELY, svy)
    q  <- svyquantile(~LONELY, svy, quantiles = c(0.25, 0.5, 0.75))
    q_vals <- as.numeric(coef(q))

    tibble(
      N        = nrow(d),
      Mean     = as.numeric(coef(m)),
      SD       = sqrt(as.numeric(coef(v))),
      Median   = q_vals[2],
      Q1       = q_vals[1],
      Q3       = q_vals[3],
      Min      = min(d$LONELY),
      Max      = max(d$LONELY),
      Skewness = weighted_skewness(d$LONELY, d$ANNUAL_WEIGHT1),
      Kurtosis = weighted_kurtosis(d$LONELY, d$ANNUAL_WEIGHT1)
    )
  })) %>%
  select(COUNTRY, stats) %>%
  unnest(stats) %>%
  arrange(desc(Mean))

# Display table
dist_tbl <- dist_stats %>%
  mutate(across(where(is.numeric) & !c(N, Min, Max), ~ round(.x, 2))) %>%
  rename(Country = COUNTRY) %>%
  flextable() %>%
  set_caption("Table RR2-1. Survey-weighted distribution of loneliness (reversed) by country") %>%
  padding(padding = 0, part = "all") %>%
  autofit() %>%
  align_text_col(align = "left") %>%
  align_nottext_col(align = "left")

dist_tbl

# Save CSV
write_csv(dist_stats %>% rename(Country = COUNTRY),
          file.path(OUT_DIR, "tbl_RR2_1_loneliness_distribution.csv"))

# Country order from main table (imputed data, descending mean)
main_table_order <- c("Turkey", "Japan", "Brazil", "India", "Egypt",
                      "South Africa", "Argentina", "Kenya", "Philippines",
                      "United Kingdom", "Tanzania", "Australia", "Spain",
                      "Mexico", "United States", "Hong Kong", "Nigeria",
                      "Sweden", "Germany", "Indonesia", "Poland", "Israel")

# Weighted proportions by country and response value
bar_data <- df.cc %>%
  group_by(COUNTRY, LONELY) %>%
  summarise(wt_n = sum(ANNUAL_WEIGHT1), .groups = "drop") %>%
  group_by(COUNTRY) %>%
  mutate(pct = wt_n / sum(wt_n) * 100) %>%
  ungroup() %>%
  mutate(COUNTRY = factor(COUNTRY, levels = main_table_order))

fig1 <- bar_data %>%
  ggplot(aes(x = LONELY, y = pct)) +
  geom_col(fill = "steelblue", width = 0.8) +
  facet_wrap(~COUNTRY, ncol = 5) +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Loneliness (reversed; higher = more lonely)",
       y = "Weighted %",
       title = "Distribution of Loneliness by Country") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

fig1

ggsave(file.path(OUT_DIR, "fig_RR2_1_bar_chart.png"), fig1, width = 14, height = 14, dpi = 300)
ggsave(file.path(OUT_DIR, "fig_RR2_1_bar_chart.pdf"), fig1, width = 14, height = 14)

fig2 <- bar_data %>%
  mutate(COUNTRY = factor(COUNTRY, levels = rev(main_table_order))) %>%
  ggplot(aes(x = factor(LONELY), y = COUNTRY, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "C", name = "Weighted %") +
  labs(x = "Loneliness (reversed; higher = more lonely)",
       y = NULL,
       title = "Distribution of Loneliness by Country") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.position = "right")

fig2

ggsave(file.path(OUT_DIR, "fig_RR2_2_heatmap.png"), fig2, width = 10, height = 8, dpi = 300)
ggsave(file.path(OUT_DIR, "fig_RR2_2_heatmap.pdf"), fig2, width = 10, height = 8)

compute_country_correlations <- function(data, var1, var2, var1_label, var2_label) {
  # Survey-weighted correlations by country
  results <- data %>%
    group_nest(COUNTRY) %>%
    mutate(result = map(data, function(d) {
      # Weighted Spearman correlation: rank variables, then weighted Pearson on ranks
      d_ranked <- data.frame(
        r1 = rank(d[[var1]], ties.method = "average"),
        r2 = rank(d[[var2]], ties.method = "average")
      )
      wt_cor <- cov.wt(d_ranked, wt = d$ANNUAL_WEIGHT1, cor = TRUE)
      r <- wt_cor$cor[1, 2]
      n <- nrow(d)
      tibble(r = r, n = n)
    })) %>%
    select(COUNTRY, result) %>%
    unnest(result) %>%
    mutate(
      z      = 0.5 * log((1 + r) / (1 - r)),
      se_z   = 1 / sqrt(n - 3),
      ci_low = tanh(z - 1.96 * se_z),
      ci_up  = tanh(z + 1.96 * se_z)
    ) %>%
    arrange(r)

  # Random-effects meta-analysis (REML) on Fisher z scale
  meta <- rma(yi = z, sei = se_z, data = results, method = "REML")

  pooled_r      <- tanh(coef(meta))
  pooled_ci_low <- tanh(meta$ci.lb)
  pooled_ci_up  <- tanh(meta$ci.ub)

  list(
    country_results = results,
    meta   = meta,
    pooled = tibble(
      r      = pooled_r,
      ci_low = pooled_ci_low,
      ci_up  = pooled_ci_up,
      I2     = meta$I2,
      tau2   = meta$tau2,
      k      = meta$k
    ),
    labels = list(var1 = var1_label, var2 = var2_label)
  )
}


corr_lifesat <- compute_country_correlations(
  df.cc, "LONELY", "LIFE_SAT",
  "Loneliness (reversed)", "Life Satisfaction"
)

# Country-level table
corr_lifesat$country_results %>%
  mutate(across(c(r, ci_low, ci_up), ~ round(.x, 3))) %>%
  select(COUNTRY, r, ci_low, ci_up, n) %>%
  rename(Country = COUNTRY, `95% CI Lower` = ci_low,
         `95% CI Upper` = ci_up, N = n) %>%
  flextable() %>%
  set_caption("Table RR2-2. Weighted Spearman correlations: Loneliness (reversed) \u00d7 Life Satisfaction") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

cat("=== Meta-Analysis: Loneliness (reversed) \u00d7 Life Satisfaction ===\n")
cat("Pooled r:", round(corr_lifesat$pooled$r, 3), "\n")
cat("95% CI: [", round(corr_lifesat$pooled$ci_low, 3), ",",
    round(corr_lifesat$pooled$ci_up, 3), "]\n")
cat("I\u00b2:", round(corr_lifesat$pooled$I2, 1), "%\n")
cat("\u03c4\u00b2:", round(corr_lifesat$pooled$tau2, 4), "\n")
cat("k:", corr_lifesat$pooled$k, "\n\n")
summary(corr_lifesat$meta)

write_csv(
  corr_lifesat$country_results %>%
    select(COUNTRY, r, ci_low, ci_up, n, z, se_z) %>%
    rename(Country = COUNTRY, N = n),
  file.path(OUT_DIR, "tbl_RR2_2_corr_lifesat.csv")
)

corr_depressed <- compute_country_correlations(
  df.cc, "LONELY", "DEPRESSED",
  "Loneliness (reversed)", "Depression"
)

# Country-level table
corr_depressed$country_results %>%
  mutate(across(c(r, ci_low, ci_up), ~ round(.x, 3))) %>%
  select(COUNTRY, r, ci_low, ci_up, n) %>%
  rename(Country = COUNTRY, `95% CI Lower` = ci_low,
         `95% CI Upper` = ci_up, N = n) %>%
  flextable() %>%
  set_caption("Table RR2-3. Weighted Spearman correlations: Loneliness (reversed) x Depression") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

cat("=== Meta-Analysis: Loneliness (reversed) x Depression ===\n")
cat("Pooled r:", round(corr_depressed$pooled$r, 3), "\n")
cat("95% CI: [", round(corr_depressed$pooled$ci_low, 3), ",",
    round(corr_depressed$pooled$ci_up, 3), "]\n")
cat("I-squared:", round(corr_depressed$pooled$I2, 1), "%\n")
cat("tau-squared:", round(corr_depressed$pooled$tau2, 4), "\n")
cat("k:", corr_depressed$pooled$k, "\n\n")
summary(corr_depressed$meta)

write_csv(
  corr_depressed$country_results %>%
    select(COUNTRY, r, ci_low, ci_up, n, z, se_z) %>%
    rename(Country = COUNTRY, N = n),
  file.path(OUT_DIR, "tbl_RR2_3_corr_depressed.csv")
)

# Multi-panel forest plot — each panel sorted by correlation strength
# Depressive symptoms: flip sign so positive = more lonely → more depressed
# (original DEPRESSED scale: 1=nearly every day, 4=not at all; higher=less depressed)
forest_df <- bind_rows(
  corr_lifesat$country_results %>%
    select(COUNTRY, r, ci_low, ci_up) %>%
    mutate(Measure = "Life Satisfaction"),
  corr_depressed$country_results %>%
    select(COUNTRY, r, ci_low, ci_up) %>%
    mutate(r = -r, ci_low_new = -ci_up, ci_up_new = -ci_low) %>%
    mutate(ci_low = ci_low_new, ci_up = ci_up_new) %>%
    select(COUNTRY, r, ci_low, ci_up) %>%
    mutate(Measure = "Depressive Symptoms")
) %>%
  mutate(
    Measure = factor(Measure, levels = c("Life Satisfaction",
                                         "Depressive Symptoms")),
    country_panel = paste(COUNTRY, Measure, sep = "___"),
    country_panel = reorder(country_panel, -r)
  )

fig3 <- ggplot(forest_df, aes(x = r, y = country_panel,
                               xmin = ci_low, xmax = ci_up)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(height = 0.2, linewidth = 0.4, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~Measure, scales = "free_y") +
  scale_y_discrete(labels = function(x) gsub("___.*", "", x)) +
  labs(x = "Spearman \u03c1",
       y = NULL,
       title = "Convergent Validity: Loneliness Correlations by Country") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

fig3

ggsave(file.path(OUT_DIR, "fig_RR2_3_forest_combined.png"), fig3, width = 12, height = 8, dpi = 300)
ggsave(file.path(OUT_DIR, "fig_RR2_3_forest_combined.pdf"), fig3, width = 12, height = 8)

# Distribution table
tbl1_flex <- dist_stats %>%
  mutate(across(where(is.numeric) & !c(N, Min, Max), ~ round(.x, 2))) %>%
  rename(Country = COUNTRY) %>%
  flextable() %>%
  set_caption("Table RR2-1. Survey-weighted distribution of loneliness (reversed) by country") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

# Life satisfaction correlation table
tbl2_flex <- corr_lifesat$country_results %>%
  mutate(across(c(r, ci_low, ci_up), ~ round(.x, 3))) %>%
  select(COUNTRY, r, ci_low, ci_up, n) %>%
  rename(Country = COUNTRY, `95% CI Lower` = ci_low,
         `95% CI Upper` = ci_up, N = n) %>%
  flextable() %>%
  set_caption("Table RR2-2. Weighted Spearman correlations: Loneliness (reversed) x Life Satisfaction") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

# Depression correlation table
tbl3_flex <- corr_depressed$country_results %>%
  mutate(across(c(r, ci_low, ci_up), ~ round(.x, 3))) %>%
  select(COUNTRY, r, ci_low, ci_up, n) %>%
  rename(Country = COUNTRY, `95% CI Lower` = ci_low,
         `95% CI Upper` = ci_up, N = n) %>%
  flextable() %>%
  set_caption("Table RR2-3. Weighted Spearman correlations: Loneliness (reversed) x Depression") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

# Meta-analysis summary table
meta_summary <- tibble(
  Analysis = c("Loneliness x Life Satisfaction",
               "Loneliness x Depression"),
  `Pooled r` = c(round(corr_lifesat$pooled$r, 3),
                 round(corr_depressed$pooled$r, 3)),
  `95% CI Lower` = c(round(corr_lifesat$pooled$ci_low, 3),
                      round(corr_depressed$pooled$ci_low, 3)),
  `95% CI Upper` = c(round(corr_lifesat$pooled$ci_up, 3),
                      round(corr_depressed$pooled$ci_up, 3)),
  I2_pct = c(round(corr_lifesat$pooled$I2, 1),
             round(corr_depressed$pooled$I2, 1)),
  tau2 = c(round(corr_lifesat$pooled$tau2, 4),
           round(corr_depressed$pooled$tau2, 4)),
  k = c(corr_lifesat$pooled$k, corr_depressed$pooled$k)
)

meta_flex <- meta_summary %>%
  rename("I-squared (%)" = I2_pct, "tau-squared" = tau2) %>%
  flextable() %>%
  set_caption("Table RR2-4. Random-effects meta-analysis summary") %>%
  padding(padding = 0, part = "all") %>%
  autofit()

read_docx() %>%
  body_add_flextable(value = tbl1_flex) %>%
  body_add_break() %>%
  body_add_flextable(value = tbl2_flex) %>%
  body_add_break() %>%
  body_add_flextable(value = tbl3_flex) %>%
  body_add_break() %>%
  body_add_flextable(value = meta_flex) %>%
  print(target = file.path(OUT_DIR, "GFS_loneliness_RR2_tables.docx"))

cat("=== Export Complete ===\n")
cat("Word: GFS_loneliness_RR2_tables.docx\n")
cat("CSVs: tbl_RR2_1/2/3_*.csv\n")
cat("Figures: fig_RR2_1-5_*.png/pdf\n")
