# =============================================================================
# 01_sociodemographic_variation.R
# Sociodemographic variation in frequency of loneliness (country-specific analyses)
#   - Table 3   : ordered country means, SDs and Gini coefficients
#   - Tables S1b-S22b : country-specific means by sociodemographic category
#   - output/gfs_paper1_demographic_variation_continuous_combined_.csv : input for
#     the meta-analyses in 03_meta_analysis.R (Table 4, Table S23)
#   - Table 1 and Tables S1a-S22a (descriptive statistics; requires gtsummary)
# Outcome: LONELY reverse-coded (10 - LONELY) so that higher = more frequent loneliness.
# =============================================================================
source("00_config.R")
packages <- c("survey", "tidyverse", "mice", "future", "forcats", "convey", "knitr",
              "flextable", "officer")
invisible(lapply(packages, library, character.only = TRUE))
if (HAS_GTSUMMARY) library(gtsummary)


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
         COUNTRY = 1) 

# get countries with missing race
withrace.countries = df.original %>% 
  group_by(COUNTRY) %>% 
  summarize(n = n(),
            n.na = sum(is.na(SELFID1))) %>% 
  filter(n!=n.na) %>% 
  mutate(COUNTRY = as.numeric(COUNTRY)) %>% 
  pull(COUNTRY)

# get REL2 labels
relaff.labels = attr(df.original$REL2, "labels") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  rename(REL2 = 2,
         REL2.labels = 1) %>% 
  mutate(REL2 = as.factor(REL2))

# data cleaning 
df <- df.original %>%
  select(AGE,GENDER,MARITAL_STATUS,EMPLOYMENT,ATTEND_SVCS,EDUCATION_3,BORN_COUNTRY,COUNTRY,REL2,SELFID1,
         outcome,ANNUAL_WEIGHT1,STRATA,PSU) %>%
  mutate(across(GENDER:outcome, function(x) ifelse(x %in% c(-98,98,99),NA,x))) %>%
  mutate(GENDER = ifelse(GENDER == 4, NA, GENDER)) %>% 
  mutate(AGE = ifelse(AGE %in% c(-998,998,999),NA,AGE)) %>% 
  mutate(
    SELFID1 = case_when(
      SELFID1 %in% c(9995, 9997, 9998, 9999, -9998) ~ NA,
      .default = SELFID1
    )
  ) 



# Imputation (see 00_config.R)
set.seed(123)
if (RUN_IMPUTATION) {
  df.imputed = df %>%
    mutate(flag = ifelse(COUNTRY %in% withrace.countries,1,0)) %>% # don't impute race for countries with no race measurement
    mutate(SELFID1 = ifelse(flag == 1,SELFID1,-1)) %>%
    group_nest(COUNTRY) %>%
    mutate(data = map(data, ~.x %>% mutate(across(GENDER:SELFID1, function(x) as.factor(x))))) %>%
    mutate(impdf = map(data, ~futuremice(.x, m = nimp, pred = quickpred(.x, exclude = c("COUNTRY","ANNUAL_WEIGHT1","STRATA","PSU")))))
  df.imputed.long = df.imputed %>%
    mutate(impdf.long = map(impdf, ~complete(.x, action = "long"))) %>%
    select(COUNTRY,impdf.long) %>%
    unnest(impdf.long)
  write.csv(df.imputed.long, file.path(DATA_DIR, "GFSsample.df.imputed.long.wave1.csv"))
}


df.imputed.long = 
  read_csv(file.path(DATA_DIR, "GFSsample.df.imputed.long.wave1.csv")) %>% select(-1) %>% 
  mutate(outcome = 10 - outcome) %>% # reverse code loneliness such that higher scores
  mutate(across(GENDER:REL2, function(x) as.factor(x))) %>% 
  mutate(age.group = ifelse(AGE <25, "18-24",ifelse(AGE < 30, "25-29",ifelse(AGE<40,"30-39",ifelse(AGE<50,"40-49",ifelse(AGE<60,"50-59",ifelse(AGE<70,"60-69",ifelse(AGE<80,"70-79","80 or older"))))))),.before = AGE) %>% 
  mutate(
    GENDER = factor(GENDER, labels = c("Male","Female","Other")),
    MARITAL_STATUS = factor(MARITAL_STATUS, levels = c(2,3,4,5,1,6), labels = c("Married","Separated","Divorced","Widowed","Never","Domestic Partner")),
    EMPLOYMENT = factor(EMPLOYMENT, labels = c("Employed for an employer","Self-employed","Retired","Student","Homemaker","Unemployed and looking for a job","None of these/other")),
    ATTEND_SVCS = factor(ATTEND_SVCS, labels = c(">1/week","1/week","1-3/month","A few times a year","Never")),
    EDUCATION_3 = factor(EDUCATION_3, labels = c("up to 8 years", "9-15 years", "16+years")),
    BORN_COUNTRY = factor(BORN_COUNTRY, labels = c("Born in this country","Born in another country")),
    ) %>% 
  left_join(race.lvls,by="SELFID1") %>% 
  select(-AGE,-SELFID1) %>% 
  relocate(race, .before = outcome)   %>% 
  mutate(race = ifelse(is.na(race), "No race",race)) %>% 
  rename(country.num = COUNTRY) %>%
  left_join(countrynumbers, by = "country.num") %>% 
  relocate(COUNTRY, .before = country.num) 

df.tbl1 = df %>% 
  mutate(age.group = ifelse(AGE <25, "18-24",ifelse(AGE < 30, "25-29",ifelse(AGE<40,"30-39",ifelse(AGE<50,"40-49",ifelse(AGE<60,"50-59",ifelse(AGE<70,"60-69",ifelse(AGE<80,"70-79","80 or older"))))))),.before = AGE) %>% 
  mutate(
    GENDER = factor(GENDER, labels = c("Male","Female","Other")),
    MARITAL_STATUS = factor(MARITAL_STATUS, levels = c(2,3,4,5,1,6), labels = c("Married","Separated","Divorced","Widowed","Never","Domestic Partner")),
    EMPLOYMENT = factor(EMPLOYMENT, labels = c("Employed for an employer","Self-employed","Retired","Student","Homemaker","Unemployed and looking for a job","None of these/other")),
    ATTEND_SVCS = factor(ATTEND_SVCS, labels = c(">1/week","1/week","1-3/month","A few times a year","Never")),
    EDUCATION_3 = factor(EDUCATION_3, labels = c("up to 8 years", "9-15 years", "16+years")),
    BORN_COUNTRY = factor(BORN_COUNTRY, labels = c("Born in this country","Born in another country")),
    REL2 = factor(REL2, labels = c("Christianity", "Islam", "Hinduism", "Buddhism", "Judaism", "Sikhism", "Baha’i", "Jainism", "Shinto", "Taoism","Confucianism", "Primal, Animist, or Folk religion","Spiritism","Umbanda, Candomblé, and other African-derived religions","Chinese folk/traditional religion", "Some other religion", "No religion/Atheist/Agnostic")),
    COUNTRY = factor(COUNTRY, labels = c("Argentina", "Australia", "Brazil", "Egypt", "Germany", "India", "Indonesia","Israel", "Japan", "Kenya", "Mexico", "Nigeria", "Philippines", "Poland", 
               "South Africa", "Spain", "Tanzania", "Turkey", "United Kingdom", "United States", "Sweden", "Hong Kong"))
    ) %>% 
  left_join(race.lvls,by="SELFID1") %>% 
  select(-AGE,-SELFID1) %>% 
  relocate(race, .before = outcome) %>% 
  mutate(across(age.group:race, function(x) fct_explicit_na(x, na_level = "Missing"))) # Making missing as an explicit level


df.tbl1.overall = df.tbl1 %>% 
  select(-race,-REL2) # remove race and religion for the main table

if (HAS_GTSUMMARY) {
# Create Table 1
tbl1 = svydesign(data = df.tbl1.overall %>% select(-outcome),ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA) %>% 
  tbl_svysummary(
    include = c(age.group:COUNTRY),
    missing_text = "Missing",
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      age.group ~ "Age group",
      GENDER ~ "Gender",
      MARITAL_STATUS ~ "Marital status",
      EMPLOYMENT ~ "Employment",
      ATTEND_SVCS ~ "Religious service attendance",
      EDUCATION_3 ~ "Education",
      BORN_COUNTRY ~ "Immigration"
      )
  ) %>% 
  bold_labels()


tbl1.forprint = tbl1 %>% 
  modify_caption(paste0("Table 1. Nationally representative descriptive statistics of the observed sample")) %>% 
  as_flex_table()  %>% 
  padding(padding = 0, part = "all") %>% 
  autofit()

tbl1.forprint
}

svy.data.imp <- df.imputed.long %>% 
  group_nest(.imp,COUNTRY) %>%
  mutate(data = map(data, ~.x %>% mutate(across(GENDER:race, function(x) as.factor(x))))) %>% # need to do this after grouping by country due to differing category numbers
  mutate(cat.count.rel2 = map(data, ~.x %>% group_by(REL2) %>% summarize(n.rel2=n()) %>%
                                mutate(prop.rel2 = n.rel2/sum(n.rel2)*100)), 
         data      = map2(data, cat.count.rel2, ~.x %>% left_join(.y)),
         data      = map(data, ~.x %>% left_join(relaff.labels, by = "REL2"))) %>% 
  mutate(svy.data = map(data, ~svydesign(ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA, data=.)))

tbl2.output = svy.data.imp %>% 
  mutate(
    mean = map(svy.data, ~svymean(~outcome, design = .)),
    variance   = map(svy.data, ~svyvar(~outcome, design = .)),
    gini = map(svy.data, ~convey_prep(.) %>% svygini(~outcome, design = .)),
    n_approx = map(svy.data, ~.x %>% nrow(.))
  ) %>% 
  mutate(
    mean.tidied = map(mean, ~as_tibble(.)),
    variance.tidied = map(variance, ~as_tibble(.)),
    gini.tidied = map(gini, ~as_tibble(.)),
    n_approx.tidied = map(n_approx, ~as_tibble(.))
  ) %>% 
  select(COUNTRY, .imp,mean.tidied,variance.tidied,gini.tidied,n_approx.tidied) %>% 
  unnest(mean.tidied,variance.tidied,gini.tidied,n_approx.tidied)  %>% 
  select(1:5,7,9) %>% 
  rename(
    std.error = 4,
    n = value,
  )  %>% 
  ungroup(.) %>% 
  mutate(Var = std.error^2) %>% 
  group_by(COUNTRY) %>% 
  summarise(mean.pooled = mean(mean),
            sd.pooled = sqrt(mean(variance)),
            gini.pooled = mean(gini),
            Vw = sum(Var)/nimp,
            Vb = sum((mean - mean(mean))^2)/(nimp-1),
            Vt = Vw + (1 + 1/nimp)*Vb,
            SE.combined = sqrt(Vt),
            lambda = (Vb + (Vb/nimp))/Vt,
            vm = (nimp-1)/lambda^2,
            k = 1, # to be changed depending on the analysis
            n = mean(n),
            vm.obs = ((n-k) + 1)/((n-k)+3)*(n-k)*(1-lambda),
            vm.adj = (vm*vm.obs)/(vm + vm.obs),
            vm.adj = ifelse(Vb == 0, vm.obs, vm.adj), # if no missing within the country, then use the unadjusted dof
            ci.low = mean.pooled - qt(0.975, vm.adj)*SE.combined,
            ci.up = mean.pooled + qt(0.975, vm.adj)*SE.combined
  )
write_csv(tbl2.output, file.path(OUT_DIR, "table3_country_means_unrounded.csv")) # used for the pooled mean in Table 3 note
tbl2.output = tbl2.output %>% 
  mutate_if(is.numeric,round,2) %>% 
  select(COUNTRY, mean.pooled,ci.low, ci.up, sd.pooled,gini.pooled) 
write_csv(tbl2.output, file.path(OUT_DIR, "table3_ordered_means.csv"))

tbl2.forprint = tbl2.output %>% 
  mutate(CI = paste0("(",format(round(ci.low,2),nsmall=2),", ",format(round(ci.up,2),nsmall=2),")"), .before = ci.low) %>% 
  rename(
    Country = COUNTRY,
    Mean = mean.pooled,
    "95% CI" = CI,
    "Standard Deviation" = sd.pooled,
    "Gini Coefficient" = gini.pooled
  ) %>% 
  select(-ci.low,-ci.up) %>% 
  arrange(desc(Mean)) %>% # descending order
  flextable()  %>% 
  padding(padding = 0, part = "all") %>% 
  set_caption(paste0("Table 2. Ordered means of ",outcome," with standard deviations and Gini coefficients")) %>%
  autofit() %>% 
  align_text_col(align = "left") %>% 
  align_nottext_col(align = "left") 


doc <- read_docx()
if (HAS_GTSUMMARY) doc <- doc %>% body_add_flextable(value = tbl1.forprint) %>% body_add_break()
doc %>% 
  body_add_flextable(tbl2.forprint) %>% 
  print(., target = file.path(OUT_DIR, "gfs_paper1_demographic_variation_continuous_Main Table 1 and Table 3.docx"))

withrace.countries2 = countrynumbers %>% 
  filter(country.num %in% withrace.countries) %>% 
  pull(COUNTRY)

# New function to compute group-specific means
mean.by.group <- function(group){
  svy.data.imp.tbl3 = svy.data.imp %>% 
    mutate(cat.count = map(data, ~.x %>% count((!!as.name(group)))),
           data      = map2(data, cat.count, ~.x %>% left_join(.y) %>% filter(n >= 2)), # remove categories with zero or one observation
           data = map(data, ~.x %>% mutate(across(GENDER:race, function(x) as.factor(x)))),# do this again
           svy.data = map(data, ~svydesign(ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA, data=.))
    )
  
  is.stratified.byrace = function(data,group){
    if(group == "race")
      data %>% filter(COUNTRY %in% withrace.countries2)
    else data
  }
  
  svy.data.imp.tbl3 = svy.data.imp.tbl3 %>% is.stratified.byrace(group)
  
  output <- svy.data.imp.tbl3 %>%
    mutate(
      mean = map(svy.data, ~svyby(~outcome, ~(!!as.name(group)), svymean, design = .,drop.empty.groups = F)), 
      n_approx = map(svy.data, ~svytotal(~(!!as.name(group)), design = .)),
      var = map(svy.data, ~svyby(~outcome, ~(!!as.name(group)), svyvar, design = .,drop.empty.groups = F)),
      fit = map(svy.data, ~svyglm(outcome ~ (!!as.name(group)), design=.)),
      globalps.error = map(fit,~try(.x %>% regTermTest(~(!!as.name(group)), method = "Wald"), silent = T)),
      globalps = map(fit, \(x){ 
        if(is.null(x) == F){
          out = NULL
          try({
            out = x %>% regTermTest(~(!!as.name(group)), method = "Wald")
          }, silent=TRUE)
          if(is.null(out)){
            try({
              out = x %>% regTermTest(~(!!as.name(group)), method = "LRT", lrt.approximation = "satterthwaite")
            }, silent=TRUE)
          }
          if(is.null(out)){
            # if all others fail, use Wald test under simple-random sampling...
            try({
              out = x %>% regTermTest(~(!!as.name(group)), method = "WorkingWald")
            }, silent=TRUE)
          }
          out
        } else {
          NULL
        }
      }),
      globalps = map(globalps, ~ifelse(is.null(.x), NA, .x$p %>% as.numeric))
    ) %>% 
    select(-data,-svy.data,-fit,-globalps.error) %>%
    mutate(
      mean = map(mean, ~.x %>% as_tibble() %>% rename(Category=1, mean=2) %>% filter(!is.na(mean))),
      n_approx = map(n_approx, ~.x %>% as_tibble() %>% select(total) %>% rename(N.sum.weights=1) %>% filter(N.sum.weights != 0)), # remove categories with zero cell
      var = map(var, ~.x %>% as_tibble() %>% rename(Var=2) %>% select(-se) %>% filter(!is.na(Var)))
    ) %>% 
    unnest(c(mean, n_approx,var,globalps), names_repair = "unique") %>% 
    group_by(Category,COUNTRY) %>% 
    mutate(SD = sqrt(Var),
           Var = se^2
    ) %>% 
    summarise(mean.pooled = mean(mean),
              SD = mean(SD),
              N.sum.weights = mean(N.sum.weights),
              Vw = sum(Var)/nimp,
              Vb = sum((mean - mean(mean))^2/(nimp-1)),
              Vt = Vw + (1 + 1/nimp)*Vb,
              SE.combined = sqrt(Vt),
              lambda = (Vb + (Vb/nimp))/Vt,
              vm = (nimp-1)/lambda^2,
              k = 1, # single mean per sample
              vm.obs = ((N.sum.weights-k) + 1)/((N.sum.weights-k)+3)*(N.sum.weights-k)*(1-lambda),
              vm.adj = (vm*vm.obs)/(vm + vm.obs),
              vm.adj = ifelse(Vb == 0, vm.obs, vm.adj), # if no missing within the country, then use the unadjusted dof
              ci.low = mean.pooled - qt(0.975, vm.adj)*SE.combined,
              ci.up = mean.pooled + qt(0.975, vm.adj)*SE.combined,
              p.transformed = -2*sum(log(globalps)),
              global.p = pchisq(p.transformed, nimp*2, lower.tail = F)      
    ) %>% 
    ungroup() %>% 
    mutate(Variable = group) %>%  
    select(COUNTRY, Variable, Category,mean.pooled, SD,SE.combined,ci.low, ci.up,vm.adj,N.sum.weights,global.p) 
  
  output
}


# Apply the function above to each group
age = mean.by.group("age.group")
gender = mean.by.group("GENDER")
marital = mean.by.group("MARITAL_STATUS")
employment = mean.by.group("EMPLOYMENT")
service = mean.by.group("ATTEND_SVCS")
educ = mean.by.group("EDUCATION_3")
immigration = mean.by.group("BORN_COUNTRY")
relaff = mean.by.group("REL2")

relaff = relaff.labels %>% 
  rename(Category = REL2) %>% 
  right_join(relaff, by = "Category")  %>% 
  arrange(COUNTRY) %>% 
  select(-Category) %>% 
  rename(Category = REL2.labels) %>% 
  relocate(Category,.after = "Variable") %>% 
  mutate(SE.combined = ifelse(vm.adj < 1, NA, SE.combined)) # don't report SE if dof < 1

race   = mean.by.group("race")

tbl3.output <-
  rbind(
    age, gender, marital, employment, service, educ, immigration, relaff,race
  )


tbl3.metainput <-
  tbl3.output %>% select(COUNTRY,Variable, Category, mean.pooled,SE.combined,global.p) %>% 
  rename(
    "Country" = COUNTRY,
    "Variable" = Variable,
    "Category" = Category,
    "Mean" = mean.pooled,
    "SE" = SE.combined,
    "Global p-value" = global.p
  ) %>% 
  arrange(Country) %>% 
  filter(!(Variable %in% c("REL2","race"))) # omit these two variables for meta analysis

write_csv(tbl3.metainput, file.path(OUT_DIR, "gfs_paper1_demographic_variation_continuous_combined_.csv"))
tbl3.output <- relabel_demo(tbl3.output)
write_csv(tbl3.output, file.path(OUT_DIR, "tbl3.output.csv")) # country-specific means (Tables S1b-S22b, Figure 1, Figures S1-S34)

##################################################
### Table S#as (country-specific demographics) ###
##################################################
if (HAS_GTSUMMARY) {
tbl1.groupby = function(data){
  data = data %>% 
    mutate(race = factor(race),
           REL2 = factor(REL2))
  
  svydesign(data = data %>% select(-outcome),ids = ~PSU, weights = ~ANNUAL_WEIGHT1, strata = ~STRATA) %>%  # add religion2 and race back in
    tbl_svysummary(
      include = c(age.group:race),
      missing_text = "Missing",
      statistic = all_continuous() ~ "{mean} ({sd})",
      label = list(
        age.group ~ "Age group",
        GENDER ~ "Gender",
        MARITAL_STATUS ~ "Marital status",
        EMPLOYMENT ~ "Employment",
        ATTEND_SVCS ~ "Religious service attendance",
        EDUCATION_3 ~ "Education",
        BORN_COUNTRY ~ "Immigration",
        REL2 ~ "Religious affiliation",
        race ~ "Race and ethnicity"
      )
    ) %>% 
    bold_labels()
}

tblSas = df.tbl1 %>%
  group_nest(COUNTRY) %>% 
  left_join(countrynumbers, by = "COUNTRY") %>% 
  arrange(COUNTRY) %>% 
  mutate(country.num = 1:nrow(.)) %>% 
  mutate(tblSas = map(data, tbl1.groupby)) %>% 
  mutate(tblSas = pmap(list(x = country.num, y = COUNTRY, z = tblSas), \(x,y,z) z %>% 
                         modify_caption(paste0("Table S",x,"a. Nationally representative descriptive statistics for ", y)) %>% 
                         as_flex_table()  %>% 
                         padding(padding = 0, part = "all") %>% 
                         autofit()
                       )) %>%   
  select(COUNTRY, tblSas)
}

#######################################################
### Table S#bs (country-specific means by subgroup) ###
#######################################################

# Country specific demographic group means of outcome
collapse_rows_df.variable <- function(df){
  df %>%
    group_by(Variable,COUNTRY) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(Variable.combined := ifelse(groupRow == 1, as.character(Variable), "")) %>%
    select(-c(groupRow))
}

collapse_rows_df.globalp <- function(df){
  df %>%
    group_by(Variable,COUNTRY) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(global.p := ifelse(groupRow == 1, as.character(global.p), "")) %>%
    select(-c(groupRow))
}

tblSbs = tbl3.output %>% 
  mutate(
    CI = paste0("(",format(round(ci.low,2),nsmall=2,scientific = F,trim = T),", ",format(round(ci.up,2),nsmall=2,scientific = F,trim = T),")"),
    mean.pooled = round(mean.pooled,2),
    SE.combined = round(SE.combined,2),
    global.p = ifelse(global.p < 0.001, "<0.001", format(round(global.p,3),nsmall=3,scientific = F,trim = T))
  ) %>% 
  mutate(
    CI = ifelse(vm.adj <= 1, NA,CI),
    SE.combined = ifelse(vm.adj <= 1, NA, SE.combined)
  ) %>% 
  select(COUNTRY,Variable, Category, mean.pooled, SE.combined, CI,global.p) %>% 
  collapse_rows_df.variable()  %>% 
  collapse_rows_df.globalp() %>% 
  mutate(Variable = Variable.combined) %>% 
  select(-Variable.combined) %>%   
  rename(
    "Category" = Category,
    "Mean" = mean.pooled,
    "SE" = SE.combined,
    "95% CI" = CI,
    "Global p-value" = global.p
  )  %>% 
  mutate(
    Variable = case_when(
      Variable == "age.group" ~ "Age group",
      Variable == "GENDER" ~ "Gender",
      Variable == "MARITAL_STATUS" ~ "Marital status",
      Variable == "EMPLOYMENT" ~ "Employment",
      Variable == "ATTEND_SVCS" ~ "Religious service attendance",
      Variable == "EDUCATION_3" ~ "Education",
      Variable == "BORN_COUNTRY" ~ "Immigration status",
      Variable == "REL2" ~ "Religious affiliation",
      Variable == "race" ~"Race and ethnicity",
      .default = Variable
    )
  ) %>% 
  group_nest(COUNTRY) %>% 
  arrange(COUNTRY) %>% 
  mutate(country.num = 1:nrow(.)) %>% 
  mutate(tblSbs = pmap(list(x=data,y=COUNTRY,z=country.num), \(x,y,z) x %>%
                         flextable() %>% 
                         align_nottext_col(align = "left") %>% 
                         padding(padding = 0, part = "all") %>% 
                         set_caption(paste0("Table S",z,"b. Means by demographic category for ", y)) %>% 
                         autofit()
                        )) %>% 
  select(-data)  


# Export Supplementary Tables S#a (if gtsummary is available) and S#b as a MS Word document
tblSs = if (HAS_GTSUMMARY) tblSas %>% full_join(tblSbs) else tblSbs
add_tables_for_country <- function(countrynum) {
  temp <- tblSs %>% 
    pivot_longer(cols = starts_with("tbl")) %>% 
    filter(country.num == countrynum)
  for (v in temp$value) doc <- doc %>% body_add_flextable(value = v) %>% body_add_break()
  return(doc)
}
doc <- read_docx()
ncountries = countrynumbers %>% nrow()
for (i in 1:ncountries){
  doc <- add_tables_for_country(i)
}
print(doc, target = file.path(OUT_DIR, "gfs_paper1_demographic_variation_continuous_combinedTableSs.docx"))
