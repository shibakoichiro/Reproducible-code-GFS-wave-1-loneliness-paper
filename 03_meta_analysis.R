# =============================================================================
# 03_meta_analysis.R
# Meta-analyses of the country-specific estimates from 01 and 02
#   - Table 3 note : random-effects pooled mean frequency of loneliness
#   - Table 4      : random-effects meta-analysis of means by sociodemographic category
#   - Table 5      : random-effects meta-analysis of childhood-factor coefficients
#   - Table 6      : E-values for the meta-analysed childhood-factor coefficients
#   - Tables S23-S25 : population-weighted meta-analyses (and E-values)
# The procedures follow the Global Flourishing Study (GFS) Wave 1 analysis code
# (Rglobalflourishing R package; refs. 63, 64 and 71 of the paper):
#   * random effects: metafor::rma(method = "PM") (Paule-Mandel); Wald-type 95% CI
#   * prediction interval / proportion of effects: calibrated (empirical Bayes)
#     country estimates; empirical minimum/maximum and proportion < -0.10 / > 0.10
#   * global P value: harmonic mean P value (harmonicmeanp::p.hmp) of the
#     country-specific global Wald-test P values
#   * population-weighted: fixed-effect model with weights proportional to the
#     2023 population size of each country
#   * E-values: EValue::evalues.OLS with the pooled SD of the outcome
#     (square root of the mean of the country-specific variances)
# =============================================================================
source("00_config.R")
suppressMessages({
  library(tidyverse); library(metafor); library(harmonicmeanp); library(EValue)
})

# ---- helper functions (as in Rglobalflourishing) ---------------------------------
compute_calibrated <- function(fit) {
  theta <- as.numeric(fit$b); tau2 <- fit$tau2
  if (tau2 == 0) tau2 <- 0.01^2
  theta + (fit$yi - theta) * sqrt(tau2 / (tau2 + fit$vi))
}
compute_global_pvalue <- function(p, p.min = 1e-16) {
  p <- p[!is.na(p)]; p <- ifelse(p < p.min, p.min, p)
  as.numeric(p.hmp(p, L = length(p)))
}
pop2023 <- c(Australia = 20329009, "Hong Kong" = 6097316, India = 964761394,
             Indonesia = 193828725, Japan = 107139250, Philippines = 73216028,
             Egypt = 56059669, Germany = 69392175, Israel = 6191774, Kenya = 25639336,
             Nigeria = 109534481, Poland = 30917886, "South Africa" = 38728302,
             Spain = 39144781, Sweden = 8296398, Tanzania = 31482707, Turkey = 62703653,
             "United Kingdom" = 53129081, "United States" = 259759435,
             Argentina = 33085830, Brazil = 156216636, Mexico = 89518608,
             China = 1112122780) # China is listed in the GFS code but not in the data
clean_country <- function(x) {
  x <- str_trim(str_replace_all(x, "_", " "))
  if_else(str_detect(x, "rkiye|rkey"), "Turkey", x)
}
run_meta <- function(d) {
  d <- d %>% filter(!is.na(sei), sei > 0)   # drops a category with a single observation (SE = 0)
  fit <- rma(yi = yi, sei = sei, data = d, method = "PM")
  cy  <- compute_calibrated(fit)
  pw  <- rma(yi = yi, sei = sei, weights = pop2023[d$Country] / sum(pop2023), data = d, method = "FE")
  tibble(k = fit$k,
         est = as.numeric(fit$b), se = fit$se,
         ci.low = est - qnorm(0.975) * se, ci.up = est + qnorm(0.975) * se,
         pi.low = min(cy), pi.up = max(cy),
         prop.below.m0.10 = mean(cy < -0.10), prop.above.0.10 = mean(cy > 0.10),
         tau = sqrt(fit$tau2), I2 = fit$I2,
         global.p = compute_global_pvalue(d$p),
         pw.est = as.numeric(pw$b), pw.se = pw$se,
         pw.ci.low = pw.est - qnorm(0.975) * pw.se, pw.ci.up = pw.est + qnorm(0.975) * pw.se)
}

# ---- Table 3 note: pooled mean frequency of loneliness ---------------------------
cm <- read_csv(file.path(OUT_DIR, "table3_country_means_unrounded.csv"), show_col_types = FALSE)
fit.mean <- rma(yi = mean.pooled, sei = SE.combined, data = cm, method = "PM")
table3.pooled <- tibble(est = as.numeric(fit.mean$b),
                        ci.low = est - qnorm(0.975) * fit.mean$se,
                        ci.up  = est + qnorm(0.975) * fit.mean$se)
write_csv(table3.pooled, file.path(OUT_DIR, "table3_note_pooled_mean.csv"))

# ---- Table 4 / Table S23: sociodemographic categories ----------------------------
d1 <- read_csv(file.path(OUT_DIR, "gfs_paper1_demographic_variation_continuous_combined_.csv"),
               show_col_types = FALSE) %>%
  transmute(Country = clean_country(Country), Variable, Category,
            yi = Mean, sei = SE, p = `Global p-value`)
meta.demo <- d1 %>% group_by(Variable, Category) %>% group_modify(~run_meta(.x)) %>% ungroup()
write_csv(meta.demo %>% select(Variable, Category, k, est, ci.low, ci.up, se, pi.low, pi.up, tau, I2, global.p),
          file.path(OUT_DIR, "table4_meta_sociodemographic.csv"))
write_csv(meta.demo %>% select(Variable, Category, k, pw.est, pw.ci.low, pw.ci.up, pw.se),
          file.path(OUT_DIR, "tableS23_popweighted_sociodemographic.csv"))

# ---- Table 5 / Table S24: childhood factors -------------------------------------
d2 <- read_csv(file.path(OUT_DIR, "gfs_paper2_childhood_predictors_continuous_combined_.csv"),
               show_col_types = FALSE) %>%
  transmute(Country = clean_country(Country), Variable, Category,
            yi = Est, sei = SE, p = pvalue)
meta.child <- d2 %>% group_by(Variable, Category) %>% group_modify(~run_meta(.x)) %>% ungroup()

# ---- Table 6 / Table S25: E-values -------------------------------------------------
sd.country <- read_csv(file.path(OUT_DIR, "country_outcome_sd.csv"), show_col_types = FALSE)
sd.pooled  <- sqrt(mean(sd.country$sd^2))
evalue <- function(est, se, which) {
  x <- evalues.OLS(est, se = se, sd = sd.pooled)
  if (which == "est") x[2] else ifelse(is.na(x[4]), x[6], x[4])
}
meta.child <- meta.child %>%
  mutate(E.est    = map2_dbl(est, se, ~evalue(.x, .y, "est")),
         E.ci     = map2_dbl(est, se, ~evalue(.x, .y, "ci")),
         pw.E.est = map2_dbl(pw.est, pw.se, ~evalue(.x, .y, "est")),
         pw.E.ci  = map2_dbl(pw.est, pw.se, ~evalue(.x, .y, "ci")))
write_csv(meta.child %>% select(Variable, Category, k, est, ci.low, ci.up, se, prop.below.m0.10, prop.above.0.10, tau, I2, global.p),
          file.path(OUT_DIR, "table5_meta_childhood.csv"))
write_csv(meta.child %>% select(Variable, Category, E.est, E.ci),
          file.path(OUT_DIR, "table6_evalues_childhood.csv"))
write_csv(meta.child %>% select(Variable, Category, k, pw.est, pw.ci.low, pw.ci.up, pw.se),
          file.path(OUT_DIR, "tableS24_popweighted_childhood.csv"))
write_csv(meta.child %>% select(Variable, Category, pw.E.est, pw.E.ci),
          file.path(OUT_DIR, "tableS25_evalues_popweighted.csv"))
cat("Meta-analyses written to", OUT_DIR, "\n")
