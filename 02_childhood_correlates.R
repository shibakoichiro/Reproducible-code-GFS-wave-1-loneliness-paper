# =============================================================================
# 02_childhood_correlates.R
# Childhood correlates of frequency of loneliness (country-specific analyses)
#   - Tables S1c-S22c : survey-weighted linear regression of frequency of loneliness
#                       on all childhood factors simultaneously, by country
#   - Tables S1d-S22d : E-values for the country-specific coefficients
#   - output/gfs_paper2_childhood_predictors_continuous_combined_.csv : input for
#     the meta-analyses in 03_meta_analysis.R (Tables 5-6, Tables S24-S25)
#   - Table 2 and childhood part of Tables S1a-S22a (descriptive; requires gtsummary)
# Outcome: LONELY reverse-coded (10 - LONELY) so that higher = more frequent loneliness.
# =============================================================================
source("00_config.R")
packages <- c("survey", "tidyverse", "mice", "future", "forcats", "convey", "knitr",
              "flextable", "officer", "fastDummies", "EValue", "broom")
invisible(lapply(packages, library, character.only = TRUE))
if (HAS_GTSUMMARY) library(gtsummary)

set_flextable_defaults(font.family = "Times New Roman", font.size = 10) # table format setting

# Import data
df.original <- haven::read_dta(file.path(DATA_DIR, "gfs_all_countries_wave1.dta"))

df.original$outcome = df.original$LONELY # Change here
outcome = "loneliness" # Change here

# get race labels then remove country names and whitespaces
race.lvls <- 
  attr(df.original$SELFID1, "labels") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(SELFID1 = 2) %>% 
  separate(rowname, c(NA,"race"), sep = ":  ", remove = F) %>% 
  select(race,SELFID1) %>% 
  mutate(race = ifelse(SELFID1 == 9996,"Other",race))

# get country labels
countrynumbers = attr(df.original$COUNTRY, "labels") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(country.num = 2,
         COUNTRY = 1) %>% 
  mutate(country.num2 = 1:nrow(.))

# get countries with missing race
withrace.countries = df.original %>% 
  group_by(COUNTRY) %>% 
  summarize(n = n(),
            n.na = sum(is.na(SELFID1))) %>% 
  filter(n!=n.na) %>% 
  mutate(COUNTRY = as.numeric(COUNTRY)) %>% 
  pull(COUNTRY)

# get REL1 labels
relaff.labels = attr(df.original$REL1, "labels") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(REL1 = 2,
         REL1.labels = 1) %>% 
  mutate(REL1 = as.factor(REL1))

# Data cleaning
## Including other parental variables that were omitted in the main analysis due to collinearity because they can still be useful for predicting missing data.
df <- df.original %>% 
  select(MOTHER_RELATN,MOTHER_LOVED,FATHER_RELATN,FATHER_LOVED,PARENTS_12YRS,INCOME_12YRS,ABUSED,OUTSIDER,HEALTH_GROWUP,BORN_COUNTRY,SVCS_12YRS,SVCS_MOTHER,SVCS_FATHER,COUNTRY,AGE,GENDER,REL1,SELFID1, outcome,ANNUAL_WEIGHT1,STRATA,PSU) %>% 
  mutate(across(c(1:COUNTRY,GENDER:outcome), function(x) ifelse(x %in% c(98,-98,99),NA,x)),
         PARENTS_12YRS = ifelse(PARENTS_12YRS == 5, NA, PARENTS_12YRS),
         OUTSIDER = ifelse(OUTSIDER == 97, NA, OUTSIDER), # Unlike parental variables, we treat "not applicable" resopnse as missing
         GENDER = ifelse(GENDER == 4, NA, GENDER),
         AGE = ifelse(AGE %in% c(-998,998,999),NA,AGE)) %>% 
  mutate(
    SELFID1 = case_when(
      SELFID1 %in% c(9995, 9997, 9998, 9999, -9998) ~ NA,
      .default = SELFID1
    )
  ) 

# Missing data imputation
set.seed(123)

if (RUN_IMPUTATION) {
  df.imputed = df %>%
    mutate(SELFID1 = ifelse(is.na(SELFID1),-1,SELFID1)) %>% # don't impute missing race
    group_nest(COUNTRY) %>%
    mutate(data = map(data, ~.x %>% mutate(across(c(1:SVCS_FATHER,GENDER,REL1,SELFID1), function(x) as.factor(x))))) %>%
    mutate(impdf = map(data, ~futuremice(.x, m = nimp, pred = quickpred(.x, exclude = c("COUNTRY","ANNUAL_WEIGHT1","STRATA","PSU")))))
  df.imputed.long = df.imputed %>%
    mutate(impdf.long = map(impdf, ~complete(.x, action = "long"))) %>%
    select(COUNTRY,impdf.long) %>%
    unnest(impdf.long)
  write.csv(df.imputed.long, file.path(DATA_DIR, "GFSsample.df.imputed.CP.long.wave1.csv"))
}


# Create a long imputed dataset
df.imputed.long =
  read_csv(file.path(DATA_DIR, "GFSsample.df.imputed.CP.long.wave1.csv")) %>% select(-1) %>%
  mutate(outcome = 10 - outcome) %>% # reverse code loneliness such that higher scores
  mutate(across(c(1:COUNTRY,GENDER:REL1), function(x) as.factor(x))) %>% 
  mutate(age.group = ifelse(AGE <25, "1998-2005; current age: 18-24",ifelse(AGE < 30, "1993-1998; age: 25-29",ifelse(AGE<40,"1983-1993; age: 30-39",ifelse(AGE<50,"1973-1983; age 40-49",ifelse(AGE<60,"1963-1973; age 50-59",ifelse(AGE<70,"1953-1963; age 60-69",ifelse(AGE<80,"1943-1953; age 70-79","1943 or earlier; age 80+"))))))),.before = AGE) %>% 
  select(-AGE) %>% 
  mutate(id = rownames(.),.before = age.group) %>% 
  dummy_cols(select_columns = c("SVCS_MOTHER","SVCS_FATHER","MOTHER_RELATN","MOTHER_LOVED","FATHER_RELATN","FATHER_LOVED"),
             ignore_na = T) %>% 
  mutate(mother.NA = case_when(SVCS_MOTHER_97 == 1 | MOTHER_RELATN_97 == 1 | MOTHER_LOVED_97 == 1 ~ 1,TRUE ~ 0),
         father.NA = case_when(SVCS_FATHER_97 == 1 | FATHER_RELATN_97 == 1 | FATHER_LOVED_97 == 1 ~ 1,TRUE ~ 0)) %>% 
  mutate(
    age.group = factor(age.group) %>% fct_rev(.),
    GENDER = factor(GENDER, labels = c("Male","Female","Other")),
    PARENTS_12YRS = factor(PARENTS_12YRS, labels = c("Yes, married","No, divorced","Never","No, one or both of them had died")),
    SVCS_12YRS = factor(SVCS_12YRS, labels = c("At least 1/week","1-3/month","<1/month","Never")),
    OUTSIDER = factor(OUTSIDER, labels = c("Yes","No")),
    ABUSED = factor(ABUSED, labels = c("Yes","No")),
    HEALTH_GROWUP = factor(HEALTH_GROWUP, labels = c("Excellent","Very good","Good","Fair","Poor")),
    INCOME_12YRS = factor(INCOME_12YRS, labels = c("Lived comfortably","Got by","Found it difficult","Found it very difficult")),
    BORN_COUNTRY = factor(BORN_COUNTRY, labels = c("Born in this country","Born in another country"))
  ) %>% 
  left_join(relaff.labels,by = "REL1") %>% 
  left_join(race.lvls,by="SELFID1") %>% 
  mutate(SVCS_12YRS = relevel(SVCS_12YRS, ref = "Never"),
         OUTSIDER = relevel(OUTSIDER, ref = "No"),
         ABUSED = relevel(ABUSED, ref = "No"),
         HEALTH_GROWUP = relevel(HEALTH_GROWUP, ref = "Good"),
         INCOME_12YRS = relevel(INCOME_12YRS, ref = "Got by"),
         BORN_COUNTRY = relevel(BORN_COUNTRY, ref = "Born in this country"),
         REL1 = factor(REL1.labels),
         REL1 = relevel(REL1, ref  = "No religion/Atheist/Agnostic"),
         race = ifelse(is.na(race), "No race",race)
         ) %>% 
  relocate(REL1,.before = race) %>% 
  mutate(
    MOTHER_RELATN_1or2 = ifelse(MOTHER_RELATN == 1 | MOTHER_RELATN == 2,1,0),
    FATHER_RELATN_1or2 = ifelse(FATHER_RELATN == 1 | FATHER_RELATN == 2,1,0)
  )

# Reference values for religious affiliation
reference.rel = 
  df.imputed.long %>% # list of reference values for the religious affiliation
  group_by(COUNTRY) %>%
  nest() %>% 
  mutate(
    prop.rel = map(data, ~.x %>% group_by(REL1.labels) %>% summarize(n.rel = n()) %>% 
                     mutate(prop.rel = n.rel/sum(n.rel)*100))
  ) %>% 
  select(COUNTRY,prop.rel) %>%
  unnest(cols = prop.rel) %>% 
  mutate(
    prevalent.rel = ifelse(REL1.labels == "No religion/Atheist/Agnostic" & prop.rel > 3, REL1.labels, # use no religion as reference if 3% or more 
                           ifelse(prop.rel == max(prop.rel),REL1.labels,NA)) # otherwise use the most prevalent category as reference
  ) %>% 
  select(COUNTRY,prevalent.rel) %>% 
  unique() %>% 
  na.omit() %>% 
  mutate(n=n()) %>% # n=2 indicates prevalent religion is NOT no religion & no religion is more than 3%
  filter((prevalent.rel == "No religion/Atheist/Agnostic" & n == 2) | n==1) %>% 
  select(-n)


#### create a dummy data with multiple countries
df.tbl1 = df %>% 
  mutate(age.group = ifelse(AGE <25, "1998-2005; current age: 18-24",ifelse(AGE < 30, "1993-1998; age: 25-29",ifelse(AGE<40,"1983-1993; age: 30-39",ifelse(AGE<50,"1973-1983; age 40-49",ifelse(AGE<60,"1963-1973; age 50-59",ifelse(AGE<70,"1953-1963; age 60-69",ifelse(AGE<80,"1943-1953; age 70-79","1943 or earlier; age 80+"))))))),.before = AGE) %>% 
  mutate(across(c(1:COUNTRY,REL1), function(x) as.factor(x))) %>%
  mutate(
    age.group = factor(age.group) %>% fct_rev(.),
    GENDER = factor(GENDER, labels = c("Male","Female","Other")),
    PARENTS_12YRS = factor(PARENTS_12YRS, labels = c("Yes, married","No, divorced","Never married","No, one or both of them had died")),
    SVCS_12YRS = factor(SVCS_12YRS, labels = c("At least 1/week","1-3/month","<1/month","Never")),
    MOTHER_RELATN = factor(MOTHER_RELATN, labels = c("Very good","Somewhat good","Somewhat bad","Very bad","Does not apply")),
    FATHER_RELATN = factor(FATHER_RELATN, labels = c("Very good","Somewhat good","Somewhat bad","Very bad","Does not apply")),
    OUTSIDER = factor(OUTSIDER, labels = c("Yes","No")),
    ABUSED = factor(ABUSED, labels = c("Yes","No")),
    HEALTH_GROWUP = factor(HEALTH_GROWUP, labels = c("Excellent","Very good","Good","Fair","Poor")),
    INCOME_12YRS = factor(INCOME_12YRS, labels = c("Lived comfortably","Got by","Found it difficult","Found it very difficult")),
    BORN_COUNTRY = factor(BORN_COUNTRY, labels = c("Born in this country","Born in another country")),
    COUNTRY = factor(COUNTRY, labels = c("Argentina", "Australia", "Brazil", "Egypt", "Germany", "India", "Indonesia","Israel", "Japan", "Kenya", "Mexico", "Nigeria", "Philippines", "Poland", 
                                         "South Africa", "Spain", "Tanzania", "Turkey", "United Kingdom", "United States", "Sweden", "Hong Kong"))
  ) %>% 
  select(-AGE, -MOTHER_LOVED, -FATHER_LOVED, -SVCS_MOTHER, -SVCS_FATHER) %>% 
  relocate(COUNTRY,.after=GENDER) %>% 
  mutate(across(c(1:COUNTRY), function(x) fct_explicit_na(x, na_level = "Missing"))) # Making missing as an explicit level
  

if (HAS_GTSUMMARY) {
# Create Table 1

tbl1 = svydesign(data = df.tbl1,ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA) %>%  
  tbl_svysummary(
    include = c(MOTHER_RELATN:COUNTRY),
    missing_text = "Missing",
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      age.group ~ "Age group",
      GENDER ~ "Gender",
      COUNTRY ~ "Country",
      PARENTS_12YRS ~ "Parent marital status",
      SVCS_12YRS ~ "Age 12 religious service attendance",
      MOTHER_RELATN ~ "Relationship with mother",
      FATHER_RELATN ~ "Relationship with father",
      OUTSIDER ~ "Outsider growing up",
      ABUSED ~ "Abuse",
      HEALTH_GROWUP ~ "Self-rated health growing up",
      BORN_COUNTRY ~ "Immigration status",
      INCOME_12YRS ~ "Subjective financial status of family growing up"
    )
  ) %>% 
  bold_labels()


tbl1.forprint = tbl1 %>% 
  modify_caption(paste0("Table 1. Nationally representative childhood descriptive statistics of the observed sample")) %>% 
  as_flex_table()  %>% 
  padding(padding = 0, part = "all") %>% 
  autofit()

read_docx() %>% 
  body_add_flextable(value = tbl1.forprint) %>% 
  print(., target = file.path(OUT_DIR, "gfs_paper2_childhood_predictors_continuous_Main Table 2.docx"))
}

#########################
### Regression output ###
#########################

# List of predictors
predictor_names <- c(
  "MOTHER_RELATN_1or2",
  "FATHER_RELATN_1or2",
  "PARENTS_12YRS",
  "INCOME_12YRS",
  "ABUSED",
  "OUTSIDER",
  "HEALTH_GROWUP",
  "BORN_COUNTRY",
  "SVCS_12YRS",
  "mother.NA",
  "father.NA",
  "age.group",
  "GENDER",
  "REL1",
  "race"
)

# Create a function to omit completely missing variables (e.g., ABUSED in Israel)
not_all_na <- function(x) any(!is.na(x))

# Fit model
svy.data.imp = df.imputed.long %>% 
  select(all_of(predictor_names),outcome,REL1.labels,.imp,COUNTRY, PSU, ANNUAL_WEIGHT1, STRATA) %>% # re-order columns
  group_by(.imp,COUNTRY) %>%
  nest() %>% 
  left_join(reference.rel, by = "COUNTRY") %>% 
  mutate(
    cat.count.rel1 = map(data, ~.x %>% group_by(REL1) %>% summarize(n.rel1=n()) %>%
                           mutate(prop.rel1 = n.rel1/sum(n.rel1)*100)), 
    prevalent.race = map(data, ~.x %>% group_by(race) %>% summarize(n.race = n()) %>% 
                           mutate(prop.race = n.race/sum(n.race)*100) %>% 
                           filter(prop.race == max(prop.race)) %>% pull(race)),
    data      = map2(data, cat.count.rel1, ~.x %>% left_join(.y)),
    data      = map(data, ~.x %>% mutate(REL1 = ifelse(prop.rel1 <= 3, "Some other religion", REL1.labels))),
    data      = map(data, ~.x %>% mutate(race = ifelse(race == "No race",NA,race))),
    data      = map2(data,prevalent.race, ~.x %>% mutate(race = ifelse(race == .y, "Majority","Minority")) %>% mutate(race = factor(race, levels = c("Majority","Minority")))),
    data      = map2(data, prevalent.rel, ~.x %>% mutate(REL1 = relevel(factor(.x$REL1),ref = .y))),
    data = map(data, ~.x %>% select(where(not_all_na))), # omit completely missing variables
    available.predictors = map(data, ~intersect(names(.x),predictor_names)), # find available predictors in each country
    data = map2(data,available.predictors, ~.x %>% 
                 select(outcome,all_of(.y),PSU,STRATA,ANNUAL_WEIGHT1)),
    svy.data = map(data, ~svydesign(ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA, data=.))
    ) %>%
  mutate(
    mainformula = map(available.predictors, ~reformulate(termlabels = .x, response = "outcome")), # create formulas using predictors available in each country
    fit.dof = map2(data, mainformula, ~glm(.y, data = .x)),
    vcom    = map(fit.dof, ~.x$df.residual), ## needed for the dof correction based on the instructions here: https://stefvanbuuren.name/fimd/sec-whyandwhen.html
    fit = map2(svy.data, mainformula, ~svyglm(.y, design=.x)),
    tidied = map(fit, ~tidy(.))
  ) %>% 
  ungroup()

# Create a output table
tbl2.output = svy.data.imp %>% 
  select(.imp, COUNTRY,tidied,vcom) %>% 
  unnest(c(tidied, vcom)) %>% 
  group_nest(COUNTRY) %>% 
  arrange(COUNTRY) %>% 
  mutate(
    term.order = map(data, ~.x %>% filter(.imp==1) %>% pull(term)),
    data = map2(data,term.order, ~.x %>% mutate(term = factor(term, levels = .y))),
    data = map(data, ~.x %>% group_by(term) %>% 
                 mutate(Var = std.error^2)%>% 
                 summarise(estimate.pooled = mean(estimate),
                           Vw = sum(Var)/nimp,
                           Vb = sum((estimate - mean(estimate))^2/(nimp-1)),
                           Vt = Vw + (1 + 1/nimp)*Vb,
                           SE.combined = sqrt(Vt),
                           lambda = (Vb + (Vb/nimp))/Vt,
                           vm = (nimp-1)/lambda^2,
                           vm.obs = ((vcom) + 1)/((vcom)+3)*(vcom)*(1-lambda),
                           vm.adj = (vm*vm.obs)/(vm + vm.obs),
                           ci.low = ifelse(Vb == 0, estimate.pooled - qt(0.975, vm.obs)*SE.combined,estimate.pooled - qt(0.975, vm.adj)*SE.combined),
                           ci.up = ifelse(Vb == 0, estimate.pooled + qt(0.975, vm.obs)*SE.combined,estimate.pooled + qt(0.975, vm.adj)*SE.combined),
                           p.value = ifelse(Vb == 0, 2*pt(-abs(estimate.pooled/SE.combined),vm.obs),2*pt(-abs(estimate.pooled/SE.combined),vm.adj))
                 ) %>% 
                 select(term,estimate.pooled,SE.combined,ci.low, ci.up,p.value) %>%
                 unique() %>% 
                 mutate(
                   CI = paste0("(",format(round(ci.low,2),nsmall=2),", ",format(round(ci.up,2),nsmall=2),")"),
                   estimate = round(estimate.pooled,2)
                 ) %>% 
                 rename(
                   Category = term,
                   Estimate = estimate
                 ))
    ) %>%
  unnest(cols = c(data)) %>% 
  filter(!grepl(".NA",Category)) %>% # remove parent missing indicators
  filter(!grepl("Intercept",Category)) 


#######################
### Global p-values ###
#######################

mycovariates <- list("age.group", "GENDER", "BORN_COUNTRY", "PARENTS_12YRS", "SVCS_12YRS",
                  "MOTHER_RELATN_1or2","FATHER_RELATN_1or2",
                  "OUTSIDER", "ABUSED", "HEALTH_GROWUP", "INCOME_12YRS", "REL1","race")

globalps = svy.data.imp %>% 
  group_by(.imp,COUNTRY) %>% 
  mutate(
    vars.regtest = map(available.predictors, ~intersect(mycovariates,.x) %>% 
                         append(.,
                                list(
                                  "MOTHER_RELATN_1or2","FATHER_RELATN_1or2"
                                  )
                                )
                       ),
    test = map2(fit, vars.regtest, ~sapply(.y,  function(z) regTermTest(.x, z))),
    test.tidied = map(test, ~.x %>% 
                        as.data.frame() %>% rownames_to_column() %>% 
                        filter(rowname %in% c("test.terms","p")) %>% 
                        pivot_longer(-rowname, names_to = "Variable", values_to = "Value") %>% 
                        pivot_wider(names_from = rowname, values_from = Value) %>% 
                        rename(
                          "term" = test.terms,
                          "p-value" = p
                        ) %>% 
                        unnest(cols = c(term, `p-value`))
                      ),
    test.tidied = map(test.tidied, ~.x %>% 
                        distinct(Variable,.keep_all = T) %>% 
                        mutate(
                          term = case_when(
                            term == "MOTHER_RELATN_1or2" ~ "MOTHER_RELATN",
                            term == "FATHER_RELATN_1or2" ~ "FATHER_RELATN",
                            .default = term
                            )
                          ) %>% 
                        select(-Variable)
                      )
  ) %>% 
  select(.imp,COUNTRY,test.tidied) %>% 
  unnest(cols = test.tidied) %>% 
  rename(
    Country = COUNTRY,
    Variable = term
  ) %>% 
  group_by(Country,Variable) %>% 
  summarise(
    p.transformed = -2*sum(log(`p-value`)),
    global.p = pchisq(p.transformed, nimp*2, lower.tail = F)
  ) %>% 
  select(Country, Variable, global.p) %>% 
  ungroup()


####################
## Relabel output ##
####################
fit.dof = df.imputed.long %>%
  filter(.imp==1 & COUNTRY == 1) %>% 
  glm(outcome ~ age.group + GENDER + PARENTS_12YRS + SVCS_12YRS + OUTSIDER + 
    ABUSED + HEALTH_GROWUP + INCOME_12YRS + BORN_COUNTRY +  
    MOTHER_RELATN_1or2 + FATHER_RELATN_1or2 + mother.NA + father.NA, data=.)

varlist  = all.vars(fit.dof$call)[2:14]  
termlist =fit.dof %>% tidy() %>% slice(-1) %>% pull(term)

base_variable = sapply(termlist, function(b) {
  match <- sapply(varlist, function(a) {
    startsWith(b, a)
  })
  varlist[which(match)]
}) 
base_variable = base_variable %>% as.data.frame() %>% pull(.)
levels = gsub(paste(unlist(base_variable),collapse = "|"),"",termlist)
termlabels = data.frame(original = termlist, Variable1 = base_variable, Variable2 = levels)

termlabels = termlabels %>% 
  mutate(
    Variable1 = case_when(
      Variable1 == "MOTHER_RELATN_1or2" ~ "MOTHER_RELATN",
      Variable1 == "FATHER_RELATN_1or2" ~ "FATHER_RELATN",
      .default = Variable1
    )
  ) %>% 
  mutate(
    Variable2 = case_when(
      original == "MOTHER_RELATN_1or2" ~ "Very good/Somewhat good",
      original == "FATHER_RELATN_1or2" ~ "Very good/Somewhat good",
      .default = Variable2
    )
  )

tbl2.output.labeled <- tbl2.output %>% 
  left_join(termlabels, by = c("Category" = "original")) %>% 
  mutate(
    Variable1 = ifelse(grepl("REL1",Category),"REL1",Variable1), #paste0("Religious affiliation at age 12 (ref=",prevalent.rel,")")
    Variable2 = ifelse(grepl("REL1",Category),str_split_i(Category,"REL1",2),Variable2)
  ) %>%
  mutate(
    Variable1 = ifelse(grepl("race",Category),"race",Variable1),
    Variable2 = ifelse(Category == "raceMinority","Minority",Variable2)
  ) %>% 
  left_join(reference.rel) %>% 
  rename(
    original.label = Category,
    Country = COUNTRY,
    Variable = Variable1,
    Category = Variable2,
    Est = estimate.pooled,
    SE = SE.combined
  ) %>% 
  left_join(globalps)  %>% 
  mutate(
    Variable = case_when(
      Variable == "age.group" ~ "Age group (Ref: 1998-2005; current age: 18-24)",
      Variable == "GENDER" ~ "Gender (Ref: Male)",
      Variable == "PARENTS_12YRS" ~ "Parent marital status (Ref: Parents married)",
      Variable == "SVCS_12YRS" ~ "Age 12 religious service attendance (Ref: Never)",
      Variable == "MOTHER_RELATN" ~ "Relationship with mother (Ref: Very bad/Somewhat bad)",
      Variable == "FATHER_RELATN" ~ "Relationship with father (Ref: Very bad/Somewhat bad)",
      Variable == "OUTSIDER" ~ "Outsider growing up (Ref: No)",
      Variable == "ABUSED" ~ "Abuse (Ref: No)",
      Variable == "HEALTH_GROWUP" ~ "Self-rated health growing up (Ref: Good)",
      Variable == "BORN_COUNTRY" ~ "Immigration status (Ref: Born in this country)",
      Variable == "INCOME_12YRS" ~ "Subjective financial status of family growing up (Ref: Got by)",
      Variable == "REL1" ~ paste0("Religious affiliation at age 12 (Ref: ",prevalent.rel,")"),
      Variable == "race" ~ "Race and ethnicity plurality (Ref: Majority)",
      .default = Variable
    )
  ) %>% 
  mutate(Country = factor(Country, labels = countrynumbers$COUNTRY))



## Meta analysis input
tbl2.metainput <- tbl2.output.labeled %>% 
  select(Country, Variable, Category, Est, SE, global.p) %>% 
  rename(pvalue = global.p) %>% 
  filter(!grepl("Race and ethnicity plurality",Variable)) %>% 
  filter(!grepl("Religious affiliation at age 12", Variable))

write_csv(tbl2.metainput, file.path(OUT_DIR, "gfs_paper2_childhood_predictors_continuous_combined_.csv"))
tbl2.output.labeled <- relabel_child(tbl2.output.labeled)
write_csv(tbl2.output.labeled, file.path(OUT_DIR, "tbl2.output.csv")) # country-specific coefficients (Tables S1c-S22c, Figure 2, Figures S35-S64)

##################################################
### Table S#as (country-specific demographics) ###
##################################################

if (HAS_GTSUMMARY) {
tbl1.groupby = function(data){
  svydesign(data = data,ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA) %>% 
    tbl_svysummary(
      include = c(MOTHER_RELATN:race),
      missing_text = "Missing",
      statistic = all_continuous() ~ "{mean} ({sd})",
      label = list(
        age.group ~ "Age group",
        GENDER ~ "Gender",
        PARENTS_12YRS ~ "Parent marital status",
        SVCS_12YRS ~ "Age 12 religious service attendance",
        MOTHER_RELATN ~ "Relationship with mother",
        FATHER_RELATN ~ "Relationship with father",
        OUTSIDER ~ "Outsider growing up",
        ABUSED ~ "Abuse",
        HEALTH_GROWUP ~ "Self-rated health growing up",
        BORN_COUNTRY ~ "Immigration status",
        INCOME_12YRS ~ "Subjective financial status of family growing up",
        REL1 ~ "Religious affiliation at age 12",
        race ~ "Race and ethnicity"
      )
    ) %>% 
    bold_labels()
}


tblSas = df.tbl1 %>%
  group_nest(COUNTRY) %>% 
  arrange(COUNTRY) %>% 
  mutate(country.num = 1:nrow(.)) %>% 
  mutate(data      = map(data, ~.x %>% left_join(race.lvls, by = "SELFID1") %>% relocate(race, .before = SELFID1)),
         data      = map(data, ~.x %>% left_join(relaff.labels, by = "REL1")),  
         data      = map(data, ~.x %>% mutate(REL1 = REL1.labels)),
         data      = map(data, ~.x %>% mutate(across(REL1:race, function(x) fct_explicit_na(x, na_level = "Missing"))))
         ) %>% 
  mutate(tblSas = map(data, tbl1.groupby)) %>% 
  mutate(tblSas = pmap(list(x = country.num, y = COUNTRY, z = tblSas), \(x,y,z) z %>% 
                         modify_caption(paste0("Table S",x,"a. Nationally representative descriptive statistics for ", y)) %>% 
                         as_flex_table()  %>% 
                         padding(padding = 0, part = "all") %>% 
                         autofit()
                       )) %>% 
  select(COUNTRY, tblSas)
}

################################################
### Table S#bs (country-specific regression) ###
################################################

## Country-specific output
collapse_rows_df.variable <- function(df, variable){
  group_var <- enquo(variable)
  df %>%
    group_by(!! group_var,Country) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}


tblSbs = tbl2.output.labeled %>% 
  mutate(SE       = round(SE,2)) %>%
  select(Country, Variable, Category,Estimate,SE,CI,global.p) %>% 
  collapse_rows_df.variable(.,Variable) %>%
  collapse_rows_df.variable(.,global.p) %>% 
  mutate(country.num2 = as.numeric(Country),
         global.p = as.numeric(global.p),
         global.p = ifelse(global.p < 0.001, "<0.001", formatC(global.p, format = "f", digits = 3))) %>%
  select(-Country) %>% 
  rename('95% CI' = CI,
         'Global p-value' = global.p) %>%
  group_nest(country.num2) %>% 
  left_join(countrynumbers) %>% 
  mutate(tblSbs = pmap(list(x=data,y=COUNTRY,z=country.num2), \(x,y,z) x %>%
                         flextable() %>% 
                         align_nottext_col(align = "left") %>% 
                         padding(padding = 0, part = "all") %>% 
                         set_caption(paste0("Table S",z,"b. Regression of ", outcome, " on childhood predictors for ", y)) %>% 
                         autofit()
                        )) %>% 
  select(-data)

#############################
### Table S#cs (E-values) ###
#############################

collapse_rows_df.variable2 <- function(df, variable){
  group_var <- enquo(variable)
  df %>%
    group_by(!! group_var,COUNTRY) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}


countrynumbers2 = countrynumbers %>% mutate(country.label = as.character(COUNTRY)) %>% select(country.label,country.num2)
  
# country-specific outcome SDs
sd.pooled = df.imputed.long %>% 
  group_by(.imp,COUNTRY) %>% 
  summarize(sd = sd(outcome)) %>% 
  group_by(COUNTRY) %>% summarize(sd = mean(sd))
write_csv(sd.pooled %>% mutate(Country = factor(COUNTRY, labels = countrynumbers$COUNTRY)), file.path(OUT_DIR, "country_outcome_sd.csv")) # used for the meta-analytic E-values
# E-values for the country-specific coefficients (Tables S1d-S22d), saved as csv
evalues.country = tbl2.output %>% 
  left_join(sd.pooled, by = "COUNTRY") %>% 
  mutate(ev = pmap(list(estimate.pooled, sd, SE.combined), \(e, s, se) evalues.OLS(e, sd = s, se = se)),
         Evalue = map_dbl(ev, ~.x[2]),
         Evalue.limit = map_dbl(ev, ~ifelse(is.na(.x[4]), .x[6], .x[4])),
         Country = factor(COUNTRY, labels = countrynumbers$COUNTRY)) %>% 
  select(Country, Category, Evalue, Evalue.limit)
write_csv(evalues.country, file.path(OUT_DIR, "evalues_country_specific.csv"))

# compute E-values
tblScs = tbl2.output %>% 
  left_join(sd.pooled, by = "COUNTRY") %>%
  left_join(reference.rel) %>% 
  mutate(country.num2 = as.numeric(COUNTRY)) %>%  
  left_join(countrynumbers2, by = "country.num2") %>% 
  mutate(COUNTRY = country.label) %>% select(-country.num2,-country.label) %>% 
  group_nest(Category,COUNTRY) %>% 
  mutate(
    evalues = map(data, function(x) evalues.OLS(x$estimate.pooled,sd = x$sd, se = x$SE.combined)),
    Evalue  = map(evalues,function(x) x[2]),
    Evalue.limit = map(evalues, function(x) ifelse(is.na(x[4]),x[6],x[4])),
    prevalent.rel = map(data,~.x %>% pull(prevalent.rel))
  ) %>% 
  select(COUNTRY, Category,Evalue,Evalue.limit,prevalent.rel) %>% 
  unnest() %>% 
  left_join(termlabels, by = c("Category" = "original")) %>% 
  mutate(Evalue = round(Evalue,2),
         Evalue.limit = round(Evalue.limit,2)) %>% 
  mutate(
    Variable1 = ifelse(grepl("REL1",Category),"REL1",Variable1), #paste0("Religious affiliation at age 12 (ref=",prevalent.rel,")")
    Variable2 = ifelse(grepl("REL1",Category),str_split_i(Category,"REL1",2),Variable2)
  ) %>%
  mutate(
    Variable1 = ifelse(grepl("race",Category),"race",Variable1),
    Variable2 = ifelse(Category == "raceMinority","Minority",Variable2)
  ) %>% 
  select(COUNTRY,Variable1,Variable2,Evalue,Evalue.limit,prevalent.rel) %>% 
  rename(
    Variable = Variable1,
    Category = Variable2
  ) %>% 
  mutate(
    Variable = case_when(
      Variable == "age.group" ~ "Age group (Ref: 1998-2005; current age: 18-24)",
      Variable == "GENDER" ~ "Gender (Ref: Male)",
      Variable == "PARENTS_12YRS" ~ "Parent marital status (Ref: Parents married)",
      Variable == "SVCS_12YRS" ~ "Age 12 religious service attendance (Ref: Never)",
      Variable == "SVCS_MOTHER" ~ "Religious service attendance of mother (Ref: Never)",
      Variable == "SVCS_FATHER" ~ "Religious service attendance of father (Ref: Never)",
      Variable == "MOTHER_RELATN" ~ "Relationship with mother (Ref: Very bad/Somewhat bad))",
      Variable == "FATHER_RELATN" ~ "Relationship with father (Ref: Very bad/Somewhat bad))",
      Variable == "MOTHER_LOVED" ~ "Love from mother (Ref: No)",
      Variable == "FATHER_LOVED" ~ "Love from father (Ref: No)",
      Variable == "OUTSIDER" ~ "Outsider growing up (Ref: No)",
      Variable == "ABUSED" ~ "Abuse (Ref: No)",
      Variable == "HEALTH_GROWUP" ~ "Self-rated health growing up (Ref: Good)",
      Variable == "BORN_COUNTRY" ~ "Immigration status (Ref: Born in this country)",
      Variable == "INCOME_12YRS" ~ "Subjective financial status of family growing up (Ref: Got by)",
      Variable == "REL1" ~ paste0("Religious affiliation at age 12 (Ref: ",prevalent.rel,")"),
      Variable == "race" ~ "Race and ethnicity plurality (Ref: Majority)",
      .default = Variable
    )) %>%   
  select(-prevalent.rel) %>%
  relabel_child() %>%
  collapse_rows_df.variable2(.,Variable) %>%
  rename(
    'E-value for Estimate' = Evalue,
    'E-value for 95% CI' = Evalue.limit
  ) %>% 
  group_nest(COUNTRY) %>% 
  left_join(countrynumbers2 %>% rename(COUNTRY = country.label)) %>% 
  arrange(country.num2) %>% 
  mutate(tblScs = pmap(list(x = data, y = COUNTRY, z = country.num2), \(x,y,z) x %>%
                         flextable() %>% 
                         align_nottext_col(align = "left") %>% 
                         padding(padding = 0, part = "all") %>% 
                         set_caption(paste0("Table S",z,"c. Sensitivity to unmeasured confounding of childhood predictors in ", y)) %>%
                         autofit()
                      )
  ) %>% 
  select(-data)

# Supplementary Tables S#a (if gtsummary is available), S#c (regressions) and S#d (E-values) as a MS Word document
tblSs = tblSbs %>% select(COUNTRY, tblSbs) %>% left_join(tblScs %>% select(COUNTRY, tblScs), by = "COUNTRY")
if (HAS_GTSUMMARY) tblSs = tblSas %>% mutate(COUNTRY = as.character(COUNTRY)) %>% left_join(tblSs, by = "COUNTRY")
tblSs = tblSs %>% left_join(countrynumbers %>% select(COUNTRY, country.num2), by = "COUNTRY") %>% arrange(country.num2)
doc <- read_docx()
for (i in seq_len(nrow(tblSs))) {
  for (v in names(tblSs)[startsWith(names(tblSs), "tbl")]) doc <- doc %>% body_add_flextable(value = tblSs[[v]][[i]]) %>% body_add_break()
}
print(doc, target = file.path(OUT_DIR, "gfs_paper2_childhood_predictors_continuous_combined.TableSs.docx"))
