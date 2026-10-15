# =============================================================================
# 00_config.R -- shared settings for all analysis scripts
# Run every script from the reproducible_code/ folder (the working directory).
# =============================================================================
DATA_DIR <- "data"     # folder with the GFS Wave 1 data and the imputed datasets
OUT_DIR  <- "output"   # all tables (csv/docx) and figures are written here
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Multiple imputation (5 datasets per country, mice + futuremice, seed 123) takes
# several hours. The imputed datasets used in the paper are read from DATA_DIR by
# default; set RUN_IMPUTATION <- TRUE to regenerate them from the raw data.
RUN_IMPUTATION <- FALSE
nimp <- 5

# gtsummary is only needed for the descriptive tables (Tables 1-2, Supplementary
# Tables S1a-S22a), which reproduce descriptive statistics from ref. 61.
HAS_GTSUMMARY <- requireNamespace("gtsummary", quietly = TRUE)

options(survey.lonely.psu = "certainty") # adjust for single PSUs

# Labels used in the Supplementary Tables/Figures (applied to the country-specific
# output files; the estimates are unchanged)
relabel_demo <- function(d) {
  d %>% dplyr::mutate(
    Category = dplyr::case_when(
      Variable == "MARITAL_STATUS" & Category == "Never" ~ "Never married",
      Variable == "MARITAL_STATUS" & Category == "Domestic Partner" ~ "Domestic partner",
      Variable == "ATTEND_SVCS" ~ sub("/", "x/", Category, fixed = TRUE),
      Variable == "EDUCATION_3" & Category == "up to 8 years" ~ "Up to 8 years",
      Category == "No religion/Atheist/Agnostic" ~ "No religion/atheist/agnostic",
      TRUE ~ as.character(Category)),
    Variable = dplyr::recode(Variable, age.group = "Age group", GENDER = "Gender",
      MARITAL_STATUS = "Marital status", EMPLOYMENT = "Employment",
      ATTEND_SVCS = "Religious service attendance", EDUCATION_3 = "Education",
      BORN_COUNTRY = "Immigration status", REL2 = "Religious affiliation",
      race = "Race and ethnicity"))
}
relabel_child <- function(d) {
  d %>% dplyr::mutate(
    Category = dplyr::case_when(
      Category == "No, divorced" ~ "Parents were divorced",
      Category == "Never" & grepl("^Parent marital", Variable) ~ "Parents were never married",
      Category == "No, one or both of them had died" ~ "One or both of parents had died",
      grepl("^Age 12 religious service", Variable) ~ sub("/", "x/", Category, fixed = TRUE),
      Category == "Minority" ~ "Minority group",
      TRUE ~ as.character(Category)),
    Variable = Variable |>
      sub(pattern = "(Ref: Parents married)", replacement = "(Ref: Parents were married)", fixed = TRUE) |>
      sub(pattern = "Race and ethnicity plurality (Ref: Majority)", replacement = "Race and ethnicity (Ref: Plurality group)", fixed = TRUE) |>
      sub(pattern = "No religion/Atheist/Agnostic", replacement = "No religion/atheist/agnostic", fixed = TRUE) |>
      sub(pattern = "Somewhat bad))", replacement = "Somewhat bad)", fixed = TRUE))
}
