##############################################################
# Herramienta digital Análisis de Riesgo SR - risk_eval.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-10-09
# R 4.3.0
##############################################################
# Editorial changes ##########################################
# Editor: Rafael León
# Editor email: leonraf@paho.org
# Edit date: 2024-05-03
# Edit: Modified score_res_rap functions to add maximum 
# score to columns with NA answers
# ####
# Editor: Rafael León
# Editor email: leonraf@paho.org
# Edit date: 2024-12-05
# Edit: Added an output for joined analysis with Polio tool 
# ####
# Editor: Rafael León
# Editor email: leonraf@paho.org
# Edit date: 2025-02-12
# Edit: Modified the tool to include the changes to the 7. Rapid
# response sheet
#############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

library(readxl)
library(sf)
library(tidyverse)

rm(list = ls())

# PATHS ----
PATH_country_data   = "Data/country_data.xlsx"
PATH_risk_cut_offs  = "R/risk_cut_offs.xlsx"
PATH_shapefiles     = "Data/shapefiles/"

# VARS ----
ref_adequate_investigation_delay = 2
ref_adequate_specimen_coll_delay = 30
ref_timely_avail_lab_results_delay = 5
LANG <- as.character(read_excel(PATH_country_data,sheet = 1)[8,2])

# LANG ----
LANG_TLS <- read_excel("R/translations.xlsx",sheet="DASHBOARD") %>% select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

rep_label_admin1_name = lang_label("rep_label_admin1_name")
rep_label_admin1_name_plural = lang_label("rep_label_admin1_name_plural")
rep_label_admin2_name = lang_label("rep_label_admin2_name")
rep_label_admin2_name_plural = lang_label("rep_label_admin2_name_plural")


# UTILS ----

# Not in operator
`%!in%` <- Negate(`%in%`)

# Removes accents and uppercases ADMIN1 and ADMIN2 columns
admin_normalizer <- function(admin_df) {
  # MAYUS
  admin_df$ADMIN1 <- toupper(admin_df$ADMIN1)
  admin_df$ADMIN2 <- toupper(admin_df$ADMIN2)
  
  # ACCENTS
  admin_df <- admin_df %>%
    mutate(
      ADMIN1 = gsub("Á","A", ADMIN1),
      ADMIN1 = gsub("É","E", ADMIN1),
      ADMIN1 = gsub("Í","I", ADMIN1),
      ADMIN1 = gsub("Ó","O", ADMIN1),
      ADMIN1 = gsub("Ú","U", ADMIN1),
      ADMIN1 = gsub("Ñ","N", ADMIN1),
      ADMIN1 = gsub("Ü","U", ADMIN1),
      ADMIN2 = gsub("Á","A", ADMIN2),
      ADMIN2 = gsub("É","E", ADMIN2),
      ADMIN2 = gsub("Í","I", ADMIN2),
      ADMIN2 = gsub("Ó","O", ADMIN2),
      ADMIN2 = gsub("Ú","U", ADMIN2),
      ADMIN2 = gsub("Ñ","N", ADMIN2),
      ADMIN2 = gsub("Ü","U", ADMIN2)
    )
}

var_norm <- function(x) {
  x = toupper(x)
  if (is.character(x)) {
    x = gsub("Á","A",x)
    x = gsub("É","E",x)
    x = gsub("Í","I",x)
    x = gsub("Ó","O",x)
    x = gsub("Ú","U",x)
    x = gsub("Ñ","N",x)
    x = gsub("Ü","U",x)
  }
  return(x)
}

# Checks if n TRUEs are found in an array
check_n_true <- function(arr,n) {
  return(sum(arr == T) >= n)
}

# Calculates de trend (slope) for 5 years of vaccination coverage via linear reegressión
cob_slope <- function(cob_1,cob_2,cob_3,cob_4,cob_5) {
  x = c(1:5)
  slopes_arr <- c()
  for (i in 1:length(cob_1)) {
    y = c(cob_1[i],cob_2[i],cob_3[i],cob_4[i],cob_5[i])
    slopes_arr <- c(slopes_arr,coef(lm(y ~ x))[[2]])
  }
  return(slopes_arr)
}

# Calculates the average PR remove NAs
cob_PR_average <- function(cob_pr_1,cob_pr_2,cob_pr_3,cob_pr_4,cob_pr_5) {
  pr_avgs <- c()
  for (i in 1:length(cob_pr_1)) {
    pr_arr = c(cob_pr_1[i],cob_pr_2[i],cob_pr_3[i],cob_pr_4[i],cob_pr_5[i])
    pr_arr = pr_arr[!is.na(pr_arr)]
    pr_avg <- round(mean(pr_arr),0)
    pr_avgs <- c(pr_avgs,pr_avg)
  }
  return(pr_avgs)
}


# SCORING Functions ----

# MMR1 and MMR2 coverage
score_cob_SRP <- function(cob) {
  if (IS_OUTBREAK) {
    RP = case_when(
      is.na(cob) ~ 8,
      cob >= 95 ~ 0,
      cob >= 90 & cob < 95 ~ 2,
      cob >= 85 & cob < 90 ~ 4,
      cob >= 80 & cob < 85 ~ 6,
      cob < 80 ~ 8
    )
  } else {
    RP = case_when(
      is.na(cob) ~ 10,
      cob >= 95 ~ 0,
      cob >= 90 & cob < 95 ~ 2,
      cob >= 85 & cob < 90 ~ 4,
      cob >= 80 & cob < 85 ~ 6,
      cob < 80 ~ 10
    )
  }
  return(RP)
}

# Percentage of neighboring municipalities with <95% of MMR1 coverage
score_pct_neighbor_mun_cob <- function(pct_mun) {
  pct_mun = as.integer(pct_mun)
  if (IS_OUTBREAK) {
    RP = case_when(
      pct_mun < 25 ~ 0,
      pct_mun >= 25 & pct_mun <= 50 ~ 2,
      pct_mun >= 51 & pct_mun <= 75 ~ 4,
      pct_mun > 75 ~ 8
    )
  } else {
    # This function is only for the outbreak scenario
    RP = NA
  }
  return(RP)
}

# Coverage of last follow-up campaign
score_cobcamp_SRP <- function(cob) {
  if (IS_OUTBREAK) {
    RP = case_when(
      is.na(cob) ~ 8,
      cob >= 95 ~ 0,
      cob >= 90 & cob < 95 ~ 2,
      cob >= 85 & cob < 90 ~ 4,
      cob < 85 ~ 6
    )
  } else {
    RP = case_when(
      is.na(cob) ~ 10,
      cob >= 95 ~ 0,
      cob >= 90 & cob < 95 ~ 2,
      cob >= 85 & cob < 90 ~ 4,
      cob < 85 ~ 6
    )
  }
  return(RP)
}

# Proportion of suspected measles cases who are unvaccinated or have unknown vaccination status
score_sospechosos_novac_SRP <- function(p) {
  if (IS_OUTBREAK) {
    PR = case_when(
      p < 20 ~ 4,
      p >= 20 ~ 8
    )
  } else {
    PR = case_when(
      p < 20 ~ 4,
      p >= 20 ~ 10
    )
  }
  return(PR)
}

# Reporting rate of suspected measles and rubella cases per 100,000 population
# (If area population ≥100,000 population)
score_calidad_notif_above100 <- function(rate) {
  if (IS_OUTBREAK) {
    PR = case_when(
      rate >= 2 ~ 0,
      rate < 2 & rate >= 1 ~ 4,
      rate < 1 ~ 8
    )
  } else {
    PR = case_when(
      rate >= 2 ~ 0,
      rate < 2 & rate >= 1 ~ 4,
      rate < 1 ~ 8
    )
  }
  return(PR)
}

# (If area population <100,000 population)
score_calidad_notif_below100 <- function(cases) {
  if (IS_OUTBREAK) {
    PR = case_when(
      cases >= 1 ~ 0,
      cases < 1 ~ 8 # epidemiologically silent 
    )
  } else {
    PR = case_when(
      cases >= 1 ~ 0,
      cases < 1 ~ 8 # epidemiologically silent 
    )
  }
  return(PR)
}

# Proportion of cases with adequate investigation
# Proportion of cases with adequate specimen collection within 30 days of rash onset
# Proportion of blood specimens received in laboratory in <5 days
score_calidad_p <- function(p) {
  if (IS_OUTBREAK) {
    PR = case_when(
      p < 80 ~ 4,
      p >= 80 ~ 0
    )
  } else {
    PR = case_when(
      p < 80 ~ 4,
      p >= 80 ~ 0
    )
  }
  return(PR)
}

# Trends in MMR1 and MMR2 coverage
score_rendimiento_trend <- function(trend) {
  if (IS_OUTBREAK) {
    PR = case_when(
      trend >= 0 ~ 0,
      trend < 0 & trend >= -10 ~ 2,
      trend < -10 ~ 4
    )
  } else {
    PR = case_when(
      trend >= 0 ~ 0,
      trend < 0 & trend >= -10 ~ 2,
      trend < -10 ~ 4
    )
  }
  return(PR)
}

# MMR1–MMR2 and Penta1–MMR1 drop-out rate
score_rendimiento_des <- function(rate) {
  rate = abs(rate)
  
  if (IS_OUTBREAK) {
    PR = case_when(
      is.na(rate) ~ 4,
      rate <= 5 ~ 0,
      rate > 5 ~ 4
    )
  } else {
    PR = case_when(
      is.na(rate) ~ 4,
      rate <= 5 ~ 0,
      rate > 5 ~ 4
    )
  }
  return(PR)
}

# ≥1 confirmed or measles compatible case reported in a district within the past 12 months among children <5 years
# ≥1 confirmed or measles compatible case(s) reported in a district within the past 12 months among children aged 5–14 years old
# ≥1 confirmed or measles compatible case(s) reported in a district within the past 12 months among young adults ≥15 years
score_confirmed_case <- function(cases) {
  if (IS_OUTBREAK) {
    PR = case_when(
      is.na(cases) ~ 0,
      cases <= 0 ~ 0, # Absence of case
      cases > 0 ~ 2 # Presence of case
    )
  } else {
    PR = case_when(
      is.na(cases) ~ 0,
      cases <= 0 ~ 0, # Absence of case
      cases > 0 ~ 2 # Presence of case
    )
  }
  return(PR)
}

# Population density
score_pop_density <- function(d) {
  if (IS_OUTBREAK) {
    RP = case_when(
      is.na(d) ~ 4,
      d <= pop_density_Q1 ~ 0,
      d > pop_density_Q1 & d <= pop_density_Q2 ~ 1,
      d > pop_density_Q2 & d <= pop_density_Q3 ~ 2,
      d > pop_density_Q3 ~ 3
    )
  } else {
    RP = case_when(
      is.na(d) ~ 4,
      d <= pop_density_Q1 ~ 0,
      d > pop_density_Q1 & d <= pop_density_Q2 ~ 1,
      d > pop_density_Q2 & d <= pop_density_Q3 ~ 2,
      d > pop_density_Q3 ~ 3
    )
  }
  return(RP)
}

# Presence of a trained rapid response team at the subnational level
score_res_rap_equipo <- function(conf) {
  conf = toupper(conf)
  if (IS_OUTBREAK) {
    PR = case_when(
      conf == 1 ~ 0,
      conf == 0 ~ 12,
      is.na(conf) ~ 12
    )
  } else {
    PR = case_when(
      conf == 1 ~ 0,
      conf == 0 ~ 6,
      is.na(conf) ~ 6
    )
  }
  return(PR)
}

# Proportion of subnational d hospitals with staff that are trained to do triage and isolation for measles/rubella highly suspected cases
score_res_rap_hospitales <- function(conf) {
  conf = toupper(conf)
  if (IS_OUTBREAK) {
    PR = case_when(
      conf == 1 ~ 0,
      conf == 0 ~ 12,
      is.na(conf) ~ 12
    )
  } else {
    PR = case_when(
      conf == 1 ~ 0,
      conf == 0 ~ 6,
      is.na(conf) ~ 6
    )
  }
  return(PR)
}

# OPTS ----
OPTS_DF <- read_xlsx(PATH_country_data,sheet = "_ListValues")
final_class_opts <- unique(OPTS_DF$`Final Classification`)
final_class_opts <- final_class_opts[!is.na(final_class_opts)]
sex_opts <- unique(OPTS_DF$Sex)
sex_opts <- sex_opts[!is.na(sex_opts)]
doses_opts <- unique(OPTS_DF$`Number Of Doses`)
doses_opts <- doses_opts[!is.na(doses_opts)]
vac_status_opts <- unique(OPTS_DF$`Vaccination Status`)
vac_status_opts <- vac_status_opts[!is.na(vac_status_opts)]
yes_no_opts <- unique(OPTS_DF$`Yes No`)
yes_no_opts <- yes_no_opts[!is.na(yes_no_opts)]
travel_hist_opts <- unique(OPTS_DF$`Travel History`)
travel_hist_opts <- travel_hist_opts[!is.na(travel_hist_opts)]


# GENERAL ----
id_data <- read_excel(PATH_country_data,sheet = 2) %>% select(1,2,3,4)
colnames(id_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2")
id_data$`ADMIN1 GEO_ID` <- as.character(id_data$`ADMIN1 GEO_ID`)
id_data$GEO_ID <- as.character(id_data$GEO_ID)
id_data <- id_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
config_data <- read_excel(PATH_country_data,sheet = 1)
colnames(config_data) <- c("var","val")
YEAR_EVAL <- as.integer(config_data$val[2])
YEAR_CAMP_SR <- as.integer(config_data$val[4])
YEAR_1 = YEAR_EVAL - 5
YEAR_2 = YEAR_EVAL - 4
YEAR_3 = YEAR_EVAL - 3
YEAR_4 = YEAR_EVAL - 2
YEAR_5 = YEAR_EVAL - 1
COUNTRY_NAME <- config_data$val[1]
REF_MMR1_AGE_MONTHS <- as.integer(config_data$val[5])
REF_MMR2_AGE_MONTHS <- as.integer(config_data$val[6])
# Maintain Is outbreak as false to avoid modifying code
# This variable is now irrelevant, there is no separate scoring
# For outbreaks or not outbreaks only for Rapid Response data
IS_OUTBREAK <- F

#Variable used in RAPID RESPONSE to determine if there is an outbreak or not
OUTBREAK <- case_when(
  config_data$val[7] == yes_no_opts[2] ~ F,
  config_data$val[7] == yes_no_opts[1] ~ T
)
REPORT_FILE_FORMAT <- config_data$val[8]
indicadores_data <- id_data


# Risk cut offs ----
if(IS_OUTBREAK) {sheet_cut_off = "outbreak"} else {sheet_cut_off = "non_outbreak"}
CUT_OFFS <- read_xlsx(PATH_risk_cut_offs,sheet=sheet_cut_off,n_max=6)
CUT_OFFS <- CUT_OFFS %>% pivot_longer(!RV,names_to = "risk_level")

# POP AREA ----
pop_data <- read_excel(PATH_country_data,sheet = 2)
colnames(pop_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2","POB","AREA")
pop_data$`ADMIN1 GEO_ID` <- as.character(pop_data$`ADMIN1 GEO_ID`)
pop_data$GEO_ID <- as.character(pop_data$GEO_ID)
pop_data <- pop_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
pop_data$POB <- as.numeric(pop_data$POB)
pop_data$AREA <- as.numeric(pop_data$AREA)
pop_data$dens_pob <- round(pop_data$POB/pop_data$AREA,1)
pop_data$dens_pob[is.na(pop_data$dens_pob)] = 0
ZERO_POB_LIST <- pop_data %>% filter(POB <= 0) %>% select(GEO_ID)
ZERO_POB_LIST <- ZERO_POB_LIST$GEO_ID
dens_pob_median <- median(pop_data$dens_pob)
pop_density_Q1 <- dens_pob_median/2
pop_density_Q2 <- dens_pob_median
pop_density_Q3 <- dens_pob_median/2*3
pop_density_Q4 <- dens_pob_median/2*4


# CASES ----
cases_data <- read_excel(PATH_country_data,sheet = 6,skip = 12)
colnames(cases_data) <- c("YEAR","GEO_ID","ADMIN1","ADMIN2","CASE_ID","FINAL_CLASS","DATE_BIRTH","SEX","RES_PLACE","PRES_FEVER","DATE_RASH","VAC_STATE","DOSE_NUM","DATE_NOTIF","DATE_INV","DATE_SAMPLE","DATE_LAB","DATE_LAST_VAC","TRAVEL_HIST")
cases_data$GEO_ID <- as.character(cases_data$GEO_ID)
pop_data <- pop_data %>% filter(!is.na(GEO_ID))
cases_data <- cases_data %>% mutate(
  DATE_BIRTH = as.Date(DATE_BIRTH),
  DATE_RASH = as.Date(DATE_RASH),
  DATE_NOTIF = as.Date(DATE_NOTIF),
  DATE_INV = as.Date(DATE_INV),
  DATE_SAMPLE = as.Date(DATE_SAMPLE),
  DATE_LAB = as.Date(DATE_LAB),
  DATE_LAST_VAC = as.Date(DATE_LAST_VAC)
)
cases_data <- cases_data %>% filter(GEO_ID %!in% ZERO_POB_LIST & !is.na(GEO_ID))
cases_data$Suspected_Case <- 1
cases_data$Core_Variables_Ok <- 0
cases_data$DATE_BIRTH[is.na(cases_data$DATE_BIRTH)] = as.Date("1900-01-01")
cases_data$Calc_Age_Months <- as.integer(difftime(cases_data$DATE_RASH,cases_data$DATE_BIRTH, unit="days")/(30.4167))

cases_data <- cases_data %>% mutate(
  Core_Variables_Ok = case_when(
    check_n_true(
      c(!is.na(CASE_ID),
      !is.na(Calc_Age_Months),
      !is.na(SEX),
      !is.na(RES_PLACE),
      !is.na(DATE_RASH),
      !is.na(DATE_NOTIF),
      !is.na(DATE_INV),
      !is.na(PRES_FEVER),
      !is.na(DATE_SAMPLE),
      !is.na(DATE_LAST_VAC),
      !is.na(TRAVEL_HIST))
      ,8) ~ 1,
      T ~ 0
  ),
  MMR_Age_Elegible = case_when(Calc_Age_Months >= REF_MMR1_AGE_MONTHS ~ 1,T ~ 0),
  Unvaccinated_Case = case_when(VAC_STATE == "" ~ 1,(VAC_STATE == yes_no_opts[2] & MMR_Age_Elegible == 1) ~ 1,T ~ 0),
  Unknown_Case = case_when(VAC_STATE == "" ~ 1,(VAC_STATE == vac_status_opts[3] & MMR_Age_Elegible == 1) ~ 1,T ~ 0),
  Unvac_Or_Unknown_Case = case_when(Unvaccinated_Case == 1 ~ 1,Unknown_Case == 1 ~ 1,T ~ 0),
  Discarded_Case = case_when(FINAL_CLASS == final_class_opts[3] ~ 1, T ~ 0),
  Confirmed_Case = case_when(FINAL_CLASS == final_class_opts[1] | FINAL_CLASS == final_class_opts[2] ~ 1, T ~ 0),
  Case_0_5_Years = case_when(Confirmed_Case == 1 & Calc_Age_Months < 60 ~ 1, T ~ 0),
  Case_5_14_Years = case_when(Confirmed_Case == 1 & Calc_Age_Months >= 60 & Calc_Age_Months <= 168 ~ 1, T ~ 0),
  Case_5_15_Years = case_when(Confirmed_Case == 1 & Calc_Age_Months >= 60 & Calc_Age_Months <= 180 ~ 1, T ~ 0),
  Case_Over_15_Years = case_when(Confirmed_Case == 1 & Calc_Age_Months > 180 ~ 1, T ~ 0),
  Adequate_Investigation = case_when(
    DATE_INV>0 & DATE_NOTIF>0 & (difftime(cases_data$DATE_INV,cases_data$DATE_NOTIF, unit="days") <= ref_adequate_investigation_delay) ~ 1*Core_Variables_Ok,
    T ~ 0
  ),
  Specimen_Collected = case_when(DATE_SAMPLE>0 ~ 1, T ~ 0),
  Adequate_Specimen_Coll = case_when(
    DATE_SAMPLE>0 &
      DATE_RASH>0 &
      (difftime(cases_data$DATE_SAMPLE,cases_data$DATE_RASH, unit="days") <= ref_adequate_specimen_coll_delay) ~ 1, T ~ 0
    ),
  Timely_Avail_Of_Lab_Results = case_when(DATE_LAB>0 & (difftime(cases_data$DATE_LAB,cases_data$DATE_SAMPLE, unit="days")<=ref_timely_avail_lab_results_delay) ~ 1, T ~ 0)
)

cases_data <- admin_normalizer(cases_data)

aggregated_cases <- cases_data %>% group_by(GEO_ID) %>% summarise(
  Unvac_Or_Unknown_Case=sum(Unvac_Or_Unknown_Case),
  MMR_Age_Elegible=sum(MMR_Age_Elegible),
  Suspected_Case=sum(Suspected_Case),
  Adequate_Investigation=sum(Adequate_Investigation),
  Adequate_Specimen_Coll=sum(Adequate_Specimen_Coll),
  Specimen_Collected=sum(Specimen_Collected),
  Timely_Avail_Of_Lab_Results=sum(Timely_Avail_Of_Lab_Results)
)
# 2025-02-12: Group by ADMIN1 of cases to use with rapid response data to adjust
# for outbreak
#cases_adm1_rr <- cases_data %>% 
#  select(ADMIN1, ADMIN2, Confirmed_Case) %>% 
#  group_by(ADMIN1) %>% 
#  summarise(Confirmed_Case = sum(Confirmed_Case, na.rm = F))

inm_aggregated_cases <- cases_data %>% 
  filter(MMR_Age_Elegible == 1) %>%
  group_by(GEO_ID) %>% summarise(
    Unvac_Or_Unknown_Case=sum(Unvac_Or_Unknown_Case),
    MMR_Age_Elegible=sum(MMR_Age_Elegible)
  )

# INM_POB ----
inmunidad_data <- read_excel(PATH_country_data,sheet = 3,skip = 1)
colnames(inmunidad_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2",
                              "SRP1_year1","SRP1_year2","SRP1_year3","SRP1_year4","SRP1_year5",
                              "SRP2_year1","SRP2_year2","SRP2_year3","SRP2_year4","SRP2_year5",
                              "cob_last_camp")
inmunidad_data <- inmunidad_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
  mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
inmunidad_data <- inmunidad_data %>% filter(!is.na(GEO_ID) & GEO_ID %!in% ZERO_POB_LIST)
cobs_inmunidad <- inmunidad_data %>% select(-ADMIN1,-ADMIN2,-cob_last_camp)

inmunidad_data <- inmunidad_data %>% mutate(
  SRP1_year1 = round(SRP1_year1,0),
  SRP1_year2 = round(SRP1_year2,0),
  SRP1_year3 = round(SRP1_year3,0),
  SRP1_year4 = round(SRP1_year4,0),
  SRP1_year5 = round(SRP1_year5,0),
  SRP2_year1 = round(SRP2_year1,0),
  SRP2_year2 = round(SRP2_year2,0),
  SRP2_year3 = round(SRP2_year3,0),
  SRP2_year4 = round(SRP2_year4,0),
  SRP2_year5 = round(SRP2_year5,0),
  SRP1_year1_PR = score_cob_SRP(SRP1_year1),
  SRP1_year2_PR = score_cob_SRP(SRP1_year2),
  SRP1_year3_PR = score_cob_SRP(SRP1_year3),
  SRP1_year4_PR = score_cob_SRP(SRP1_year4),
  SRP1_year5_PR = score_cob_SRP(SRP1_year5),
  SRP2_year1_PR = score_cob_SRP(SRP2_year1),
  SRP2_year2_PR = score_cob_SRP(SRP2_year2),
  SRP2_year3_PR = score_cob_SRP(SRP2_year3),
  SRP2_year4_PR = score_cob_SRP(SRP2_year4),
  SRP2_year5_PR = score_cob_SRP(SRP2_year5),
  cob_last_camp = round(as.numeric(cob_last_camp),0)
  )

inmunidad_data <- inmunidad_data %>% mutate(
  SRP1_PR = cob_PR_average(SRP1_year1_PR,SRP1_year2_PR,SRP1_year3_PR,SRP1_year4_PR,SRP1_year5_PR),
  SRP2_PR = cob_PR_average(SRP2_year1_PR,SRP2_year2_PR,SRP2_year3_PR,SRP2_year4_PR,SRP2_year5_PR),
  cob_last_camp_PR = score_cobcamp_SRP(cob_last_camp)
)
inmunidad_data$cob_last_camp_PR[is.na(inmunidad_data$cob_last_camp_PR)] = 8

inmunidad_data <- full_join(inmunidad_data,inm_aggregated_cases,by="GEO_ID")
inmunidad_data$p_sospechosos_novac <- round(inmunidad_data$Unvac_Or_Unknown_Case/inmunidad_data$MMR_Age_Elegible*100,1)
inmunidad_data$Unvac_Or_Unknown_Case[is.na(inmunidad_data$Unvac_Or_Unknown_Case)] = 0
inmunidad_data$MMR_Age_Elegible[is.na(inmunidad_data$MMR_Age_Elegible)] = 0
inmunidad_data$p_sospechosos_novac[is.na(inmunidad_data$p_sospechosos_novac)] = 0
inmunidad_data$p_sospechosos_novac_PR <- score_sospechosos_novac_SRP(as.integer(round(inmunidad_data$p_sospechosos_novac,0)))
inmunidad_data$TOTAL_PR <- inmunidad_data$SRP1_PR + inmunidad_data$SRP2_PR + inmunidad_data$cob_last_camp_PR + inmunidad_data$p_sospechosos_novac_PR

# Join to general risk results
inmunidad_data_join <- inmunidad_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,INMUNIDAD_POB=TOTAL_PR)
indicadores_data <- left_join(indicadores_data,inmunidad_data_join,by=c("ADMIN1 GEO_ID","GEO_ID"))


# SURV_QUAL ----
calidad_data <- id_data
calidad_data <- left_join(calidad_data,pop_data %>% select(-ADMIN1,-ADMIN2,-dens_pob),by=c("ADMIN1 GEO_ID","GEO_ID"))
calidad_data <- calidad_data %>% filter(!is.na(GEO_ID) & GEO_ID %!in% ZERO_POB_LIST)
calidad_data <- left_join(calidad_data,aggregated_cases %>% select("GEO_ID","Suspected_Case","Adequate_Investigation","Adequate_Specimen_Coll","Specimen_Collected","Timely_Avail_Of_Lab_Results"),by="GEO_ID")
calidad_data[is.na(calidad_data)] = 0

calidad_data$tasa_casos <- round(calidad_data$Suspected_Case*100000/calidad_data$POB,1)
calidad_data$p_casos_inv <- round(calidad_data$Adequate_Investigation/calidad_data$Suspected_Case*100,1)
calidad_data$p_casos_muestra <- round(calidad_data$Adequate_Specimen_Coll/calidad_data$Suspected_Case*100,1)
calidad_data$p_muestras_lab <- round(calidad_data$Timely_Avail_Of_Lab_Results/calidad_data$Specimen_Collected*100,1)
calidad_data[is.na(calidad_data)] = 0

# Risk points para tasa de casos, formula compuesta
calidad_data$tasa_casos_PR <- 0 # iniciar en 0
for(i in 1:nrow(calidad_data)) {
  if (calidad_data[i,]$POB >= 100000) { # Si tiene pob >= 100k usar formula above100 con tasa
    calidad_data[i,]$tasa_casos_PR = score_calidad_notif_above100(calidad_data[i,]$tasa_casos)
  } else { # Si tiene pob < 100k usar formula below100 con casos sopechosos
    calidad_data[i,]$tasa_casos_PR = score_calidad_notif_below100(calidad_data[i,]$Suspected_Case)
  }
}

calidad_data <- calidad_data %>% mutate(
  p_casos_inv_PR = score_calidad_p(p_casos_inv),
  p_casos_muestra_PR = score_calidad_p(p_casos_muestra),
  p_muestras_lab_PR = score_calidad_p(p_muestras_lab),
  TOTAL_PR = tasa_casos_PR+p_casos_inv_PR+p_casos_muestra_PR+p_muestras_lab_PR
)

# Join to general risk results
calidad_data_join <- calidad_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,CALIDAD_VIG=TOTAL_PR)
indicadores_data <- left_join(indicadores_data,calidad_data_join,by=c("ADMIN1 GEO_ID","GEO_ID"))


# PROG_DEL ----
rendimiento_data <- read_excel(PATH_country_data,sheet = 4,skip = 1) 
colnames(rendimiento_data) = c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2","d_PENTA1","d_SRP1","d_SRP2","tasa_des_srp1_srp2","tasa_des_penta1_srp1")
rendimiento_data <- rendimiento_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
  mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
rendimiento_data <- rendimiento_data %>% filter(!is.na(GEO_ID) & GEO_ID %!in% ZERO_POB_LIST)
rendimiento_data$tasa_des_srp1_srp2 <- abs(rendimiento_data$tasa_des_srp1_srp2)
rendimiento_data$tasa_des_penta1_srp1 <- abs(rendimiento_data$tasa_des_penta1_srp1)
rendimiento_data <- left_join(rendimiento_data,cobs_inmunidad,by=c("ADMIN1 GEO_ID","GEO_ID"))

rendimiento_data$tendencia_SRP1 <- cob_slope(rendimiento_data$SRP1_year1,rendimiento_data$SRP1_year2,rendimiento_data$SRP1_year3,rendimiento_data$SRP1_year4,rendimiento_data$SRP1_year5)
rendimiento_data$tendencia_SRP2 <- cob_slope(rendimiento_data$SRP2_year1,rendimiento_data$SRP2_year2,rendimiento_data$SRP2_year3,rendimiento_data$SRP2_year4,rendimiento_data$SRP2_year5)
#rendimiento_data[is.na(rendimiento_data)] = 0

rendimiento_data <- rendimiento_data %>% mutate(
  tendencia_SRP1_PR = score_rendimiento_trend(tendencia_SRP1),
  tendencia_SRP2_PR = score_rendimiento_trend(tendencia_SRP2),
  tasa_des_srp1_srp2_PR = score_rendimiento_des(tasa_des_srp1_srp2),
  tasa_des_penta1_srp1_PR = score_rendimiento_des(tasa_des_penta1_srp1),
  TOTAL_PR = tendencia_SRP1_PR+tendencia_SRP2_PR+tasa_des_srp1_srp2_PR+tasa_des_penta1_srp1_PR
)

# Join to general risk results
rendimiento_data_join <- rendimiento_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,RENDIMIENTO_PROG=TOTAL_PR)
indicadores_data <- left_join(indicadores_data,rendimiento_data_join,by=c("ADMIN1 GEO_ID","GEO_ID"))


# VUL GROUP ----
vulnerables_data <- read_excel(PATH_country_data,sheet = 5,skip = 1)
colnames(vulnerables_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2","pres_intercambio_pob","pres_turismo","pres_problemas","pres_calamidades","dif_topo_transporte","pres_comunidades","pres_trafico","pres_eventos","TOTAL_PR")
vulnerables_data <- vulnerables_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
  mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
vulnerables_data <- vulnerables_data %>% filter(!is.na(GEO_ID) & GEO_ID %!in% ZERO_POB_LIST)

# Si o no dicotómico
vulnerables_data[c(5:12)] <- lapply(vulnerables_data[c(5:12)], toupper)
vulnerables_data[c(5:12)] <- lapply(vulnerables_data[c(5:12)], var_norm)

vulnerables_data[vulnerables_data == toupper(yes_no_opts[2])] <- "0"
vulnerables_data[vulnerables_data == toupper(yes_no_opts[1])] <- "1"
vulnerables_data[is.na(vulnerables_data)] <- "2"
vulnerables_data[c(5:12)] <- lapply(vulnerables_data[c(5:12)], as.numeric)
vulnerables_data[is.na(vulnerables_data)] <- 2


vulnerables_data$TOTAL_PR = vulnerables_data$pres_intercambio_pob+
  vulnerables_data$pres_turismo+
  vulnerables_data$pres_problemas+
  vulnerables_data$pres_calamidades+
  vulnerables_data$dif_topo_transporte+
  vulnerables_data$pres_comunidades+
  vulnerables_data$pres_trafico+
  vulnerables_data$pres_eventos

# THRE_ASSE ----
eval_amenaza_data <- vulnerables_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,ADMIN1,ADMIN2,pob_vulnerable=TOTAL_PR)
eval_amenaza_data$pob_vulnerable_PR <- eval_amenaza_data$pob_vulnerable
eval_amenaza_data <- left_join(eval_amenaza_data,pop_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,dens_pob),by=c("ADMIN1 GEO_ID","GEO_ID"))
eval_amenaza_data <- eval_amenaza_data %>% mutate(dens_pob_PR = score_pop_density(dens_pob))
eval_amenaza_data$TOTAL_PR = eval_amenaza_data$dens_pob_PR+eval_amenaza_data$pob_vulnerable_PR

# Join to general risk results
eval_amenaza_data_join <- eval_amenaza_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,EVAL_AMENAZA=TOTAL_PR)
indicadores_data <- left_join(indicadores_data,eval_amenaza_data_join,by=c("ADMIN1 GEO_ID","GEO_ID"))


# RES_RAP ----
respuesta_rapida_data <- read_excel(PATH_country_data,sheet = 7)
colnames(respuesta_rapida_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2","equipo","hospitales_p")
respuesta_rapida_data <- respuesta_rapida_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
  mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
respuesta_rapida_data <- respuesta_rapida_data %>% filter(!is.na(GEO_ID) & GEO_ID %!in% ZERO_POB_LIST)
respuesta_rapida_data$equipo <- toupper(respuesta_rapida_data$equipo)
respuesta_rapida_data$equipo <- var_norm(respuesta_rapida_data$equipo)
respuesta_rapida_data$equipo[respuesta_rapida_data$equipo == var_norm(yes_no_opts[1])] = "1" # Yes
respuesta_rapida_data$equipo[respuesta_rapida_data$equipo == var_norm(yes_no_opts[2])] = "0" # No
respuesta_rapida_data$equipo <- as.numeric(respuesta_rapida_data$equipo)

# 2025-02-12: Hospital question changed to include a different qualitative question
respuesta_rapida_data$hospitales_p <- toupper(respuesta_rapida_data$hospitales_p)
respuesta_rapida_data$hospitales_p <- var_norm(respuesta_rapida_data$hospitales_p)
respuesta_rapida_data$hospitales_p[respuesta_rapida_data$hospitales_p == var_norm(yes_no_opts[1])] = "1" # Yes
respuesta_rapida_data$hospitales_p[respuesta_rapida_data$hospitales_p == var_norm(yes_no_opts[2])] = "0" # No
respuesta_rapida_data$hospitales_p <- as.numeric(respuesta_rapida_data$hospitales_p)

respuesta_rapida_data <- respuesta_rapida_data %>% mutate(
  equipo_PR = score_res_rap_equipo(equipo),
  hospitales_p_PR = score_res_rap_hospitales(hospitales_p)
)
respuesta_rapida_data$TOTAL_PR <- ifelse(respuesta_rapida_data$equipo_PR == 6,12, respuesta_rapida_data$equipo_PR+respuesta_rapida_data$hospitales_p_PR)
respuesta_rapida_data$equipo_PR <- ifelse(respuesta_rapida_data$equipo_PR == 6,6, respuesta_rapida_data$equipo_PR)
respuesta_rapida_data$hospitales_p_PR <- ifelse(respuesta_rapida_data$equipo_PR == 6,6, respuesta_rapida_data$hospitales_p_PR)
respuesta_rapida_data$hospitales_p <- ifelse(is.na(respuesta_rapida_data$equipo),0,ifelse(respuesta_rapida_data$equipo == 0,0, respuesta_rapida_data$hospitales_p))
# Join to general risk results
respuesta_rapida_data_join <- respuesta_rapida_data %>% select(`ADMIN1 GEO_ID`,GEO_ID,RES_RAPIDA=TOTAL_PR)
indicadores_data <- left_join(indicadores_data,respuesta_rapida_data_join,by=c("ADMIN1 GEO_ID","GEO_ID"))

indicadores_data$TOTAL_PR = indicadores_data$INMUNIDAD_POB+
  indicadores_data$CALIDAD_VIG+
  indicadores_data$RENDIMIENTO_PROG+
  indicadores_data$EVAL_AMENAZA+
  indicadores_data$RES_RAPIDA

# shapefiles ----
country_shapes <- st_read(PATH_shapefiles,layer="admin2")
if ("ADMIN1_" %in% colnames(country_shapes)) {
  country_shapes <- country_shapes %>% rename("ADMIN1_GEO_ID"="ADMIN1_")}
country_shapes <- country_shapes %>% 
  mutate(`ADMIN1_GEO_ID` = as.character(`ADMIN1_GEO_ID`),GEO_ID = as.character(GEO_ID))

# DBD vars ----
YEAR_LIST <- c(YEAR_1,YEAR_2,YEAR_3,YEAR_4,YEAR_5)
admin1_list <- c(toupper(lang_label("rep_label_all")),sort(unique(id_data$ADMIN1)))
admin1_geo_id_df <- id_data %>% select(`ADMIN1 GEO_ID`,ADMIN1) %>% unique()
admin1_geo_id_df <- rbind(admin1_geo_id_df,c(0,toupper(lang_label("rep_label_all"))))

# Silent Municipalities ----
# Section that determines which of the geo objects haven't reported a single case
# in the Case by case data section using cases_data
# ------------------------------------------------------------------------------

#Select from pop_data the geo_codes of the country
geo_info <- pop_data %>% 
  select(GEO_ID, ADMIN1, ADMIN2, `ADMIN1 GEO_ID`)
cases_unique <- cases_data %>%
  select(GEO_ID) %>% 
  distinct(GEO_ID)
silent_mun <- anti_join(geo_info,cases_unique) 
silent_data <- geo_info %>% 
  mutate(silent_mun = ifelse(GEO_ID %in% silent_mun$GEO_ID, T,F))
calidad_data <- left_join(calidad_data, silent_data)

# Changes 2024-12-05 ----
# Added save function for the risk eval results for joint analysis
rio::export(indicadores_data, "Data/mmr_results.xlsx")
# SAVE ----
rm(aggregated_cases,cobs_inmunidad,
   calidad_data_join,
   eval_amenaza_data_join,
   inmunidad_data_join,
   rendimiento_data_join,
   respuesta_rapida_data_join,
   i,sheet_cut_off, 
   geo_info, 
   cases_unique,
   silent_mun,
   silent_data)
save.image(file = "SR_BD.RData")

# CLEAN ----
rm(list = ls())
