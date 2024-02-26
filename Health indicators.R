## R script to generate cross tabulations in excel
#calculation of indicators, cross-tabulations and correlations

#------------ packages --------------------------------------------------------

library(tidyverse)
library(readxl)
library(openxlsx)
library(table1)
library(kableExtra)
library(gtsummary)
library(openxlsx)

options(scipen=999)


#------------ data ------------------------------------------------------------

#data used is from UNHCR MSNA 2023 for Europe, available at https://microdata.unhcr.org/index.php/catalog/1028

data_hh <- read_excel("/unhcr_eu_msna_2023_hh_anonym.xlsx")

data_ind <- read_excel("/unhcr_eu_msna_2023_indv_anonym.xlsx")


#------------ Top 3 reported priority needs -----------------------------------

data_hh_clean <- filter(data_hh, AAP_5_SM_TOP_NEEDS_no_needs  != 1 & AAP_5_SM_TOP_NEEDS_dont_know != 1 &
                          AAP_5_SM_TOP_NEEDS_prefer_not_to_answer != 1)

#regional
dt_needs_regional <- data_hh_clean %>%
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "regional") %>% mutate(indicator = "Top 3 reported priority needs")

#Bulgaria
dt_needs_Bulgaria <- data_hh_clean %>% filter(country == "Bulgaria") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Bulgaria") %>% mutate(indicator = "Top 3 reported priority needs")

#Czech Republic
dt_needs_Czechia <- data_hh_clean %>% filter(country == "Czechia") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Czechia") %>% mutate(indicator = "Top 3 reported priority needs")

#Hungary
dt_needs_Czechia <- data_hh_clean %>% filter(country == "Hungary") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%"))  %>% 
  mutate(country = "Hungary") %>% mutate(indicator = "Top 3 reported priority needs")

#Moldova
dt_needs_Moldova <- data_hh_clean %>% filter(country == "Moldova") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Moldova") %>% mutate(indicator = "Top 3 reported priority needs")

#Poland
dt_needs_Poland <- data_hh_clean %>% filter(country == "Poland") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Poland") %>% mutate(indicator = "Top 3 reported priority needs")

#Romania
dt_needs_Romania <- data_hh_clean %>% filter(country == "Romania") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Romania") %>% mutate(indicator = "Top 3 reported priority needs")

#Slovakia
dt_needs_Slovakia <- data_hh_clean %>% filter(country == "Slovakia") %>% 
  select(starts_with("AAP_5_SM_TOP_NEEDS_"), regional_weights) %>%
  pivot_longer(cols = starts_with("AAP_5_SM_TOP_NEEDS_"),
               names_to = "variable",
               names_prefix = "AAP_5_SM_TOP_NEEDS_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Slovakia") %>% mutate(indicator = "Top 3 reported priority needs")

dt_needs <- rbind(dt_needs_regional, dt_needs_Bulgaria, dt_needs_Czechia, dt_needs_Moldova, dt_needs_Poland, dt_needs_Romania, dt_needs_Slovakia)


#----------------- Access to GBV related health services -----------------------

dt_GBV_access_Regional <- data_hh  %>% 
  mutate(answer = case_when(
    GBV01a_SS_HLTH == "yes"  ~ 1,
    GBV01a_SS_HLTH == "prefer_no_answer" ~ NA,    
    GBV01a_SS_HLTH == "no"  ~ 0,
    GBV01a_SS_HLTH == "do_not_know"  ~ 0)) %>%  filter(!is.na(answer)) %>% 
  group_by(answer) %>%  
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% 
  mutate(indicator = "% of respondents who know how to access GBV related health services.") %>% mutate(variable = "GBV_access")

dt_GBV_access_Countries <- data_hh  %>%
  mutate(answer = case_when(
    GBV01a_SS_HLTH == "yes"  ~ 1,
    GBV01a_SS_HLTH == "prefer_no_answer" ~ NA,    
    GBV01a_SS_HLTH == "no"  ~ 0,
    GBV01a_SS_HLTH == "do_not_know"  ~ 0)) %>%  filter(!is.na(answer)) %>% 
  group_by(answer, country) %>%  
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%"))%>% 
  mutate(indicator = "% of respondents who know how to access GBV related health services.") %>% mutate(variable = "GBV_access")

#----------------- Major barriers for accessing services on GBV -----------------

data_hh_clean <- filter(data_hh, GBV02_SM_GBV_BARR_no_need_to_check != 1 & 
                          GBV02_SM_GBV_BARR_dont_know != 1 &
                          GBV02_SM_GBV_BARR_prefer_not_to_answer != 1)

#regional
dt_GBV_barrier_Regional <- data_hh_clean %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
mutate(country = "regional") %>% mutate(indicator = "Barriers for accessing services on GB")

#Bulgaria
dt_GBV_barrier_Bulgaria <- data_hh_clean %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Bulgaria") %>% mutate(indicator = "Barriers for accessing services on GB")


#Czechia
dt_GBV_barrier_Czechia <- data_hh_clean %>% filter(country == "Czechia") %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Czechia") %>% mutate(indicator = "Barriers for accessing services on GB")

#Hungary
dt_GBV_barrier_Hungary <- data_hh_clean %>% filter(country == "Hungary") %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Hungary") %>% mutate(indicator = "Barriers for accessing services on GB")

#Moldova
dt_GBV_barrier_Moldova <- data_hh_clean %>% filter(country == "Moldova") %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Moldova") %>% mutate(indicator = "Barriers for accessing services on GB")

#Romania
dt_GBV_barrier_Romania <- data_hh_clean %>% filter(country == "Romania") %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Romania") %>% mutate(indicator = "Barriers for accessing services on GB")

#Slovakia
dt_GBV_barrier_Slovakia <- data_hh_clean %>% filter(country == "Slovakia") %>%
  select(starts_with("GBV02_SM_GBV_BARR_"), regional_weights) %>%
  pivot_longer(cols = starts_with("GBV02_SM_GBV_BARR_"),
               names_to = "variable",
               names_prefix = "GBV02_SM_GBV_BARR_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Slovakia") %>% mutate(indicator = "Barriers for accessing services on GB")

dt_GBV_barrier <- rbind(dt_GBV_barrier_Regional, dt_GBV_barrier_Bulgaria, dt_GBV_barrier_Czechia,dt_GBV_barrier_Hungary,dt_GBV_barrier_Moldova, dt_GBV_barrier_Romania, dt_GBV_barrier_Slovakia)

#--------------------- Food Consumption Score (FCS) ---------------------------

data_hh <- data_hh %>%
  mutate(across(c(FS_1_1_NUM_FOOD, FS_1_2_NUM_PULSES, FS_1_3_NUM_DAIRY,FS_1_4_NUM_MEAT, FS_1_5_NUM_VEG, FS_1_6_NUM_FRUITS, FS_1_7_NUM_OIL, FS_1_8_NUM_SUGAR,FS_1__NUM_SPICES), ~ replace(., . %in% c(9999, 8888), NA)))

# calculate FCS
data_hh <- data_hh %>% mutate(FCS = (2 * FS_1_1_NUM_FOOD) +(3 * FS_1_2_NUM_PULSES) +(4*FS_1_4_NUM_MEAT) +(4*FS_1_3_NUM_DAIRY) + FS_1_5_NUM_VEG  + FS_1_6_NUM_FRUITS +(0.5*FS_1_7_NUM_OIL) +(0.5*FS_1_8_NUM_SUGAR))

#create FCG groups based on 21/25 or 28/42 thresholds

#Use this when analyzing a country with low consumption of sugar and oil - thresholds 21-35
data_hh <- data_hh %>% mutate(FCSCat21 = case_when(
  FCS <= 21 ~ 1, between(FCS, 21.5, 35) ~ 2, FCS > 35 ~ 3))


dt_FCS_21 <- data_hh %>% group_by(FCSCat21, country) %>% filter(!is.na(FCSCat21)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% 
  mutate(FCS_21 = case_when(FCSCat21 == 1 ~ "Poor",
                            FCSCat21 == 2 ~ "Borderline",
                            FCSCat21 == 3 ~ "Acceptable")) %>% rename( answer = FCS_21) %>% 
  select(!FCSCat21) %>% mutate(variable = "FCSCat21") %>% mutate(indicator = "Food Consumption Score (FCS)")

data_hh <- data_hh %>% mutate(FCSCat28 = case_when(
  FCS <= 28 ~ 1, between(FCS, 28.5, 42) ~ 2, FCS > 42 ~ 3))

dt_FCS_28 <- data_hh %>% group_by(FCSCat28, country) %>% filter(!is.na(FCSCat28)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%
  mutate(FCS_28 = case_when(FCSCat28 == 1 ~ "Poor",
                            FCSCat28 == 2 ~ "Borderline",
                            FCSCat28 == 3 ~ "Acceptable")) %>% rename( answer = FCS_28) %>% 
  select(!FCSCat28) %>% mutate(variable = "FCSCat28") %>% mutate(indicator = "Food Consumption Score (FCS)")

#--------------------- Reduced health expenditures as part of livelihood coping strategies --------------------------------------

data_hh <- data_hh %>% 
  mutate(health_coping = case_when(L5_SS_REDUCED_HLTH_EXP == "yes" ~ 1,
                                   L5_SS_REDUCED_HLTH_EXP == "no_already_done" ~ 1,
                                   L5_SS_REDUCED_HLTH_EXP == "no_not_needed" ~ 0,
                                   L5_SS_REDUCED_HLTH_EXP == "do_not_know" ~ NA,
                                   L5_SS_REDUCED_HLTH_EXP == "prefer_no_answer" ~ NA,
                                   L5_SS_REDUCED_HLTH_EXP == "not_applicable" ~ NA,
                                   .default = NA))

dt_health_coping_regional <- data_hh %>% group_by(health_coping) %>% filter(!is.na(health_coping)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% 
  rename( answer = health_coping) %>% mutate(variable = "health_coping") %>% 
  mutate(indicator = "% of households which reduced health expenditures as part of livelihood coping strategies.")

dt_health_coping_Countries <- data_hh %>% group_by(health_coping, country) %>% filter(!is.na(health_coping)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n())  %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%"))%>% 
  rename( answer = health_coping) %>% mutate(variable = "health_coping") %>% 
  mutate(indicator = "% of households which reduced health expenditures as part of livelihood coping strategies.")

#-------------------- Access to health services --------------------------------

data_ind <- data_ind %>% # Those who were able to access healthcare
  mutate(health_access = case_when(
    H3_SS_HLTH_OBTAIN_CARE == "yes"  ~ 1,
    H3_SS_HLTH_OBTAIN_CARE == "do_not_know" ~ NA,
    H3_SS_HLTH_OBTAIN_CARE == "prefer_no_answer" ~ NA,
    H3_SS_HLTH_OBTAIN_CARE == "no" ~ 0,
    .default = NA)) 

data_ind <- data_ind %>% # Those who needed and asked to access healthcare - remove those who didn't try to access healthcare for some reasons
  mutate(health_need = case_when(
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "wanted_to_wait_and_see_if_the_problem_go_better" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "do_not_trust_local_provider" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "fear_or_distrust_of_HW_EXAM_treatment" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "other" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" ~ 1,
    H1_SS_HLTH_PBLM == "do_not_know" ~ NA_real_,
    H1_SS_HLTH_PBLM == "prefer_no_answer" ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(impact2_3_health = health_access / health_need)

#individual level
dt_health_access_regional_ind <- data_ind %>% group_by(impact2_3_health) %>% filter(!is.na(impact2_3_health)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = impact2_3_health) %>% 
  mutate(variable = "health_access_ind") %>% 
  mutate(indicator = "% of household members with access to health services (out of those who needed and asked to access healthcare).")

dt_health_access_countries_ind <- data_ind %>% group_by(impact2_3_health, country) %>% filter(!is.na(impact2_3_health)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%"))%>% rename( answer = impact2_3_health) %>% 
  mutate(variable = "health_access_ind") %>% 
  mutate(indicator = "% of household members with access to health services (out of those who needed and asked to access healthcare).")

#-------------------- Need to access health care in the last 30 days -----------

HH_health_need <- data_ind %>% select(unique_hh_index.x, health_need) %>% filter(!is.na(health_need))

HH_health_need <- unique(HH_health_need)

HH_health_need <- HH_health_need %>% group_by(unique_hh_index.x) %>%  mutate(health_need = sum(health_need))

HH_health_need <- unique(HH_health_need)

data_hh <- left_join(data_hh, HH_health_need, by = c("unique_hh_index" = "unique_hh_index.x"))

dt_health_need_regional <-  data_hh %>% group_by(health_need) %>% filter(!is.na(health_need)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = health_need) %>% 
  mutate(variable = "health_need_HH") %>% 
  mutate(indicator = "% of HHs with at least one member who needed to access health care in the last 30 days.")

dt_health_need_countries <-  data_hh %>% group_by(health_need, country) %>% filter(!is.na(health_need)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = health_need) %>% 
  mutate(variable = "health_need_HH") %>% 
  mutate(indicator = "% of HHs with at least one member who needed to access health care in the last 30 days.")

#-------------------- Chronical illness ----------------------------------------

data_ind <- data_ind %>% 
  mutate(chronic_ill = case_when(H2_SS_HLTH_CHRONIC_ILL == "yes" ~ 1,
                                 H2_SS_HLTH_CHRONIC_ILL == "no" ~ 0,
                                 H2_SS_HLTH_CHRONIC_ILL == "prefer_no_answer" ~ NA,
                                 H2_SS_HLTH_CHRONIC_ILL == "do_not_know" ~ NA,
                                 .default = NA))

HH_chronic_ill <- data_ind %>% select(unique_hh_index.x, chronic_ill) %>% filter(!is.na(chronic_ill))

HH_chronic_ill <- unique(HH_chronic_ill)

HH_chronic_ill <- HH_chronic_ill %>% group_by(unique_hh_index.x) %>%  mutate(chronic_ill = sum(chronic_ill))

HH_chronic_ill <- unique(HH_chronic_ill)

data_hh <- left_join(data_hh, HH_chronic_ill, by = c("unique_hh_index" = "unique_hh_index.x"))

dt_chronic_ill_regional <- data_hh %>% group_by(chronic_ill) %>% filter(!is.na(chronic_ill)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = chronic_ill) %>% 
  mutate(variable = "chronic_ill_HH") %>% 
  mutate(indicator = "% of households with at least one member with a chronical illness.")

dt_chronic_ill_countries <- data_hh %>% group_by(chronic_ill, country) %>% filter(!is.na(chronic_ill)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = chronic_ill) %>% 
  mutate(variable = "chronic_ill_HH") %>% 
  mutate(indicator = "% of households with at least one member with a chronical illness.")

#individual level
dt_chronic_ill_regional_ind <- data_ind %>% group_by(chronic_ill) %>% filter(!is.na(chronic_ill)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = chronic_ill) %>% 
  mutate(variable = "chronic_ill_HH") %>% 
  mutate(indicator = "% of individuals with a chronical illness.")

dt_chronic_ill_countries_ind <- data_ind %>% group_by(chronic_ill, country) %>% filter(!is.na(chronic_ill)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = chronic_ill) %>% 
  mutate(variable = "chronic_ill_HH") %>% 
  mutate(indicator = "% of individuals with a chronical illness.")

#-------------------- Disability -----------------------------------------------

data_ind <-  data_ind %>%
  mutate( # disability identifier variables according to Washington Group standards
    disaux1_34 = WG_1_1_SS_DIFF_SEE %in% c("lot_difficulty","cannot_all"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_34 = WG_1_2_SS_DIFF_HEAR %in% c("lot_difficulty","cannot_all"),
    disaux3_34 = WG_1_3_SS_DIFF_WALK %in% c("lot_difficulty","cannot_all"),
    disaux4_34 = WG_1_4_SS_DIFF_REM %in% c("lot_difficulty","cannot_all"),
    disaux5_34 = WG_1_5_SS_DIFF_DRESS %in% c("lot_difficulty","cannot_all"),
    disaux6_34 = WG_1_6_SS_DIFF_COMM %in% c("lot_difficulty","cannot_all")) %>%
  mutate(
    disSum34 = rowSums(select(., disaux1_34, disaux2_34 , disaux3_34 , disaux4_34 , disaux5_34 , disaux6_34)) # count number of TRUE indicator variables over 6 domains
    
  ) %>%
  mutate(
    DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum34 >= 1 ~ 1,
      disSum34 == 0 & (!(WG_1_1_SS_DIFF_SEE %in% c("refused","do_not_know","prefer_no_answer") & WG_1_2_SS_DIFF_HEAR %in% c("refused","do_not_know","prefer_no_answer") & WG_1_3_SS_DIFF_WALK %in% c("refused","do_not_know","prefer_no_answer") & WG_1_4_SS_DIFF_REM %in% c("refused","do_not_know","prefer_no_answer") & WG_1_5_SS_DIFF_DRESS %in% c("refused","do_not_know","prefer_no_answer") & WG_1_6_SS_DIFF_COMM %in% c("refused","do_not_know","prefer_no_answer"))) ~ 0,
      WG_1_1_SS_DIFF_SEE %in% c("refused","do_not_know","prefer_no_answer") & WG_1_2_SS_DIFF_HEAR %in% c("refused","do_not_know","prefer_no_answer") & WG_1_3_SS_DIFF_WALK %in% c("refused","do_not_know","prefer_no_answer") & WG_1_4_SS_DIFF_REM %in% c("refused","do_not_know","prefer_no_answer") & WG_1_5_SS_DIFF_DRESS %in% c("refused","do_not_know","prefer_no_answer") & WG_1_6_SS_DIFF_COMM %in% c("refused","do_not_know","prefer_no_answer") ~ 98
    )
  ) 

### Calculate having at least one disability identifier among 4 categories 

data_ind <- data_ind %>%
  mutate(disability = case_when(DISABILITY3 == 1 ~ 1, 
                                TRUE ~ 0)) 

#individual level
dt_disability_regional_ind <- data_ind %>% group_by(disability) %>% filter(!is.na(disability)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = disability) %>% 
  mutate(variable = "disability_ind") %>% 
  mutate(indicator = "% of individuals reported with disability level 3 and above per WGS guidance.")

dt_disability_countries_ind <- data_ind %>% group_by(disability, country) %>% filter(!is.na(disability)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n())  %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = disability) %>% 
  mutate(variable = "disability_ind") %>% 
  mutate(indicator = "% of individuals reported with disability level 3 and above per WGS guidance.")

#HH with at least one member with disability

HH_disability <- data_ind %>% select(unique_hh_index.x, disability) %>% filter(!is.na(disability))

HH_disability <- unique(HH_disability)

HH_disability <- HH_disability %>% group_by(unique_hh_index.x) %>%  mutate(disability = sum(disability))

HH_disability <- unique(HH_disability)

data_hh <- left_join(data_hh, HH_disability, by = c("unique_hh_index" = "unique_hh_index.x"))

dt_disability_regional_HH <- data_hh %>% group_by(disability) %>% filter(!is.na(disability)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n())  %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%"))  %>%  
  mutate(country = "Regional") %>% rename( answer = disability) %>% 
  mutate(variable = "disability_hh") %>% 
  mutate(indicator = "% of households with at least one member with disability level 3 and above per WGS guidance.")

dt_disability_countries_HH <- data_hh %>% group_by(disability, country) %>% filter(!is.na(disability)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = disability) %>% 
  mutate(variable = "disability_hh") %>% 
  mutate(indicator = "% of households with at least one member with disability level 3 and above per WGS guidance.")

#----------------------- Barriers to accessing health care --------------------

data_ind_clean <- filter(data_ind, is.na(H4_SM_HLTH_ACC_BARRIER_no_answer) | H4_SM_HLTH_ACC_BARRIER_no_answer != 1)

#Regional
dt_health_barrier_Regional <- data_ind_clean %>% 
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Regional") %>% mutate(indicator = "Barriers to accessing health care")

#Bulgaria
dt_health_barrier_Bulgaria <- data_ind_clean %>% filter(country == "Bulgaria") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Bulgaria") %>% mutate(indicator = "Barriers to accessing health care")

#Czechia
dt_health_barrier_Czechia <- data_ind_clean %>% filter(country == "Czechia") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Czechia") %>% mutate(indicator = "Barriers to accessing health care")

#Hungary
dt_health_barrier_Hungary <- data_ind_clean %>% filter(country == "Hungary") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Hungary") %>% mutate(indicator = "Barriers to accessing health care")

#Poland
dt_health_barrier_Poland <- data_ind_clean %>% filter(country == "Poland") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Poland") %>% mutate(indicator = "Barriers to accessing health care")

#Moldova
dt_health_barrier_Moldova <- data_ind_clean %>% filter(country == "Moldova") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Moldova") %>% mutate(indicator = "Barriers to accessing health care")

#Romania
dt_health_barrier_Romania <- data_ind_clean %>% filter(country == "Romania") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Romania") %>% mutate(indicator = "Barriers to accessing health care")

#Slovakia
dt_health_barrier_Slovakia <- data_ind_clean %>% filter(country == "Slovakia") %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H4_SM_HLTH_ACC_BARRIER_"),
               names_to = "variable",
               names_prefix = "H4_SM_HLTH_ACC_BARRIER_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Slovakia") %>% mutate(indicator = "Barriers to accessing health care")

dt_health_barrier <- rbind(dt_health_barrier_Regional, dt_health_barrier_Bulgaria, dt_health_barrier_Czechia, dt_health_barrier_Hungary, dt_health_barrier_Moldova, dt_health_barrier_Poland, dt_health_barrier_Romania, dt_health_barrier_Slovakia)

#-------------------- Barriers to accessing sexual and reproductive health -------

data_hh <- data_hh %>% mutate(barrier_sexual_health = case_when(H12_NUM_WOM_BARR == 0 ~ 0,
                                                                H12_NUM_WOM_BARR == 1 ~ 1,
                                                                H12_NUM_WOM_BARR == 2 ~ 1,
                                                                H12_NUM_WOM_BARR == 3 ~ 1,
                                                                H12_NUM_WOM_BARR == 5 ~ 1,
                                                                H12_NUM_WOM_BARR == 8 ~ 1,
                                                                H12_NUM_WOM_BARR == 9 ~ 1,
                                                                .default = NA))

dt_barrier_sexual_health_regional <- data_hh %>% filter(!is.na(H12_NUM_WOM_BARR)) %>% group_by(barrier_sexual_health) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = barrier_sexual_health) %>% 
  mutate(variable = "barrier_sexual_health") %>% 
  mutate(indicator = "% of HH with at least one women that self-reported barriers to accessing sexual and reproductive health.")

dt_barrier_sexual_health_countries <- data_hh %>% filter(!is.na(H12_NUM_WOM_BARR)) %>% group_by(barrier_sexual_health, country) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = barrier_sexual_health) %>% 
  mutate(variable = "barrier_sexual_health") %>% 
  mutate(indicator = "% of HH with at least one women that self-reported barriers to accessing sexual and reproductive health.")

#-------------------- Measles vaccination -------------------------------------

data_ind <- data_ind %>%
  mutate(outcome10_1_measles = case_when(
    H5_1_SS_HLTH_VACCINE_MEASLES == "yes" ~ 1, 
    H5_1_SS_HLTH_VACCINE_MEASLES == "no" ~ 0,
    H5_1_SS_HLTH_VACCINE_MEASLES == "do_not_know" ~ NA,
    H5_1_SS_HLTH_VACCINE_MEASLES == "prefer_no_answer"  ~ NA,
    TRUE ~ NA_real_
  )) 


dt_measles_regional <- data_ind %>% group_by(outcome10_1_measles) %>% filter(!is.na(outcome10_1_measles)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%"))  %>%  
  mutate(country = "Regional") %>% rename( answer = outcome10_1_measles) %>% 
  mutate(variable = "outcome10_1_measles") %>% mutate(indicator = "% of children (9mo-5years) who have received measles vaccination.")

dt_measles_countries <- data_ind %>% group_by(outcome10_1_measles, country) %>% filter(!is.na(outcome10_1_measles)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = outcome10_1_measles) %>% 
  mutate(variable = "outcome10_1_measles") %>% mutate(indicator = "% of children (9mo-5years) who have received measles vaccination.")

#-------------------- Polio vaccination ----------------------------------------

data_ind <- data_ind %>%
  mutate(outcome10_1_polio = case_when(
    H6_SS_HLTH_VACCINE_POLIO == "1_dose" ~ 1,
    H6_SS_HLTH_VACCINE_POLIO == "2_doses" ~ 1,
    H6_SS_HLTH_VACCINE_POLIO == "3_doses" ~ 1, 
    H6_SS_HLTH_VACCINE_POLIO == "4_doses" ~ 1, 
    H6_SS_HLTH_VACCINE_POLIO == "0_none" ~ 0,
    H6_SS_HLTH_VACCINE_POLIO == "do_not_know" ~ NA, 
    H6_SS_HLTH_VACCINE_POLIO == "prefer_no_answer"  ~ NA,
    TRUE ~ NA_real_
  )) 


dt_polio_regional <- data_ind %>% group_by(outcome10_1_polio) %>% filter(!is.na(outcome10_1_polio)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>%  
  mutate(country = "Regional") %>% rename( answer = outcome10_1_polio) %>% 
  mutate(variable = "outcome10_1_polio") %>% mutate(indicator = "% of children up to 6 years who have received polio vaccination.")

dt_polio_countries <- data_ind %>% group_by(outcome10_1_polio, country) %>% filter(!is.na(outcome10_1_polio)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct= prop.table(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = outcome10_1_polio) %>% 
  mutate(variable = "outcome10_1_polio") %>% mutate(indicator = "% of children up to 6 years who have received polio vaccination.")

#------------------- Mental health or psychosocial problems --------------------

data_ind <- data_ind %>% mutate(mental_health = case_when(H11_SS_PSY == "yes" ~ 1,
                                                          H11_SS_PSY == "no" ~ 0,
                                                          H11_SS_PSY == "prefer_no_answer" ~ NA,
                                                          H11_SS_PSY == "do_not_know" ~ NA,
                                                          .default = NA))


HH_mental_health <- data_ind %>% select(unique_hh_index.x, mental_health) %>% filter(!is.na(mental_health))

HH_mental_health <- unique(HH_mental_health)

HH_mental_health <- HH_mental_health %>% group_by(unique_hh_index.x) %>%  mutate(mental_health = sum(mental_health))

HH_mental_health <- unique(HH_mental_health)

data_hh <- left_join(data_hh, HH_mental_health, by = c("unique_hh_index" = "unique_hh_index.x"))

dt_mental_health_regional <- data_hh %>% group_by(mental_health) %>% filter(!is.na(mental_health)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n())  %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = mental_health) %>% 
  mutate(variable = "mental_health") %>% 
  mutate(indicator = "% of HH with at least one member experiencing mental health or psychosocial problems.")

dt_mental_health_countries <- data_hh %>% group_by(mental_health, country) %>% filter(!is.na(mental_health)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = mental_health) %>% 
  mutate(variable = "mental_health") %>% 
  mutate(indicator = "% of HH with at least one member experiencing mental health or psychosocial problems.")

#-------------------- Need mental health or psychosocial support ---------------

data_ind <- data_ind %>% mutate(mental_health_sup = case_when(H12_SS_NEED_SUP == "yes" ~ 1,
                                                              H12_SS_NEED_SUP == "no" ~ 0,
                                                              H12_SS_NEED_SUP == "prefer_no_answer" ~ NA,
                                                              H12_SS_NEED_SUP == "do_not_know" ~ NA,
                                                              .default = NA))

dt_mental_health_sup_regional <- data_ind %>% group_by(mental_health_sup) %>% filter(!is.na(mental_health_sup)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = mental_health_sup) %>% 
  mutate(variable = "mental_health_sup") %>% 
  mutate(indicator = "% of HH members (individuals) who need mental health or psychosocial support.")

dt_mental_health_sup_countries <- data_ind %>% group_by(mental_health_sup, country) %>% filter(!is.na(mental_health_sup)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = mental_health_sup) %>% 
  mutate(variable = "mental_health_sup") %>% 
  mutate(indicator = "% of HH members (individuals) who need mental health or psychosocial support.")

#-------------------- Help for mental health and psychosocial problems ----------

data_ind <- data_ind %>% mutate(mental_received_sup = case_when(H13_SS_RECEIVED_SUP == "yes" ~ 1,
                                                                H13_SS_RECEIVED_SUP == "no" ~ 0,
                                                                H13_SS_RECEIVED_SUP == "prefer_no_answer" ~ NA,
                                                                H13_SS_RECEIVED_SUP == "do_not_know" ~ NA,
                                                                .default = NA))

dt_mental_received_sup_regional <- data_ind %>% group_by(mental_received_sup) %>% filter(!is.na(mental_received_sup)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = mental_received_sup) %>% 
  mutate(variable = "mental_received_sup") %>% 
  mutate(indicator = "% of HH members who want help for mental health and psychosocial problems who received it.")

dt_mental_received_sup_countries <- data_ind %>% group_by(mental_received_sup, country) %>% filter(!is.na(mental_received_sup)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = mental_received_sup) %>% 
  mutate(variable = "mental_received_sup") %>% 
  mutate(indicator = "% of HH members who want help for mental health and psychosocial problems who received it.")

#-------------------- Barriers to accessing mental health and psychosocial support services ----------------

data_ind_clean <- filter(data_ind, H14_SM_UNA_ACC_dont_know != 1 )

#regional
dt_barrier_mental_health_regional <- data_ind_clean %>%
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "regional") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Bulgaria
dt_barrier_mental_health_Bulgaria <- data_ind_clean %>% filter(country == "Bulgaria") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Bulgaria") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Czechia
dt_barrier_mental_health_Czechia <- data_ind_clean %>% filter(country == "Czechia") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Czechia") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Hungary
dt_barrier_mental_health_Hungary <- data_ind_clean %>% filter(country == "Hungary") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Hungary") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Moldova
dt_barrier_mental_health_Moldova <- data_ind_clean %>% filter(country == "Moldova") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Moldova") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Poland
dt_barrier_mental_health_Poland <- data_ind_clean %>% filter(country == "Poland") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Poland") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Romania
dt_barrier_mental_health_Romania <- data_ind_clean %>% filter(country == "Romania") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Romania") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

#Slovakia
dt_barrier_mental_health_Slovakia <- data_ind_clean %>% filter(country == "Slovakia") %>% 
  select(starts_with("H14_SM_UNA_ACC_"), regional_weights) %>%
  pivot_longer(cols = starts_with("H14_SM_UNA_ACC_"),
               names_to = "variable",
               names_prefix = "H14_SM_UNA_ACC_",
               values_to = "answer") %>%
  group_by(variable, answer) %>% filter(answer == 0 | answer == 1) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>%
  group_by(variable) %>% mutate(per= prop.table(n_weighted),
                                pctlabel = paste0(round(per*100), "%")) %>% 
  mutate(country = "Slovakia") %>% mutate(indicator = "Barriers to accessing mental health and psychosocial support services")

dt_barrier_mental_health <- rbind(dt_barrier_mental_health_regional, dt_barrier_mental_health_Bulgaria, dt_barrier_mental_health_Czechia, dt_barrier_mental_health_Hungary, dt_barrier_mental_health_Moldova, dt_barrier_mental_health_Poland, dt_barrier_mental_health_Romania, dt_barrier_mental_health_Slovakia)

#------------------- Mental health and psychosocial support services and improvement in wellbeing --------------------------

data_ind <- data_ind %>% mutate(mental_health_improved = case_when(H15_SS_SHW_IMPRV == "yes" ~ 1,
                                                                   H15_SS_SHW_IMPRV == "slight" ~ 1,
                                                                   H15_SS_SHW_IMPRV == "no" ~ 0,
                                                                   H15_SS_SHW_IMPRV == "prefer_no_answer" ~ NA,
                                                                   H15_SS_SHW_IMPRV == "do_not_know" ~ NA,
                                                                   .default = NA))

dt_mental_health_improved_regional <- data_ind %>% group_by(mental_health_improved) %>% filter(!is.na(mental_health_improved)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = mental_health_improved) %>% 
  mutate(variable = "mental_health_improved") %>% 
  mutate(indicator = "% of HH members who received mental health and psychosocial support services and report improvement in well-being.")

dt_mental_health_improved_countries <- data_ind %>% group_by(mental_health_improved, country) %>% 
  filter(!is.na(mental_health_improved)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = mental_health_improved) %>% 
  mutate(variable = "mental_health_improved") %>% 
  mutate(indicator = "% of HH members who received mental health and psychosocial support services and report improvement in well-being.")

#---------------- Timely initiation of breastfeeding ---------------------------

data_ind <- data_ind %>% mutate(timely_breastfeeding = case_when(H7_SS_HLTH_BREAST_FEEDING == "less_than_1hr" ~ 1,
                                                                 H7_SS_HLTH_BREAST_FEEDING == "between_1hr_and_23hrs" ~ 0,
                                                                 H7_SS_HLTH_BREAST_FEEDING == "24hrs_and_more" ~ 0,
                                                                 H7_SS_HLTH_BREAST_FEEDING == "never" ~ NA,
                                                                 H7_SS_HLTH_BREAST_FEEDING == "do_not_know" ~ NA,
                                                                 H7_SS_HLTH_BREAST_FEEDING == "prefer_no_answer" ~ NA,
                                                                 .default = NA))

dt_timely_breastfeeding_regional <- data_ind %>% group_by(timely_breastfeeding) %>% filter(!is.na(timely_breastfeeding)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = timely_breastfeeding) %>% 
  mutate(variable = "timely_breastfeeding") %>% 
  mutate(indicator = "% of respondents who indicated timely initiation of breastfeeding in children aged 0-23 months (initiated breastfeeding within less than 1 hour).")


dt_timely_breastfeeding_countries <- data_ind %>% group_by(timely_breastfeeding, country) %>% filter(!is.na(timely_breastfeeding)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = timely_breastfeeding) %>% 
  mutate(variable = "timely_breastfeeding") %>% 
  mutate(indicator = "% of respondents who indicated timely initiation of breastfeeding in children aged 0-23 months (initiated breastfeeding within less than 1 hour).")


# ------------------- Exclusive breastfeeding ----------------------------------

data_ind <- data_ind %>% mutate(exclusive_breastfeeding = case_when(
  H8_SS_HLTH_EXCLUSIVE_BREAST_FED == "no_child_only_had_breastmilk" ~ 1,
  H8_SS_HLTH_EXCLUSIVE_BREAST_FED == "no_longer_breastfed" ~ 0,
  H8_SS_HLTH_EXCLUSIVE_BREAST_FED == "yes_beside_breastmilk" ~ 0,
  H8_SS_HLTH_EXCLUSIVE_BREAST_FED == "do_not_know" ~ NA,
  .default = NA))

dt_exclusive_breastfeeding_regional <- data_ind %>% group_by(exclusive_breastfeeding) %>% filter(!is.na(exclusive_breastfeeding)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% 
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% mutate(country = "Regional") %>% rename( answer = exclusive_breastfeeding) %>% 
  mutate(variable = "exclusive_breastfeeding") %>% 
  mutate(indicator = "% of respondents who indicated exclusive breastfeeding for infants under 6 months.")


dt_exclusive_breastfeeding_countries <- data_ind %>% group_by(exclusive_breastfeeding, country) %>% filter(!is.na(exclusive_breastfeeding)) %>% 
  summarise(n_weighted = sum(regional_weights),
            n_unweighted = n()) %>% group_by(country) %>%
  mutate(pct = n_weighted / sum(n_weighted),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( answer = exclusive_breastfeeding) %>% 
  mutate(variable = "exclusive_breastfeeding") %>% 
  mutate(indicator = "% of respondents who indicated exclusive breastfeeding for infants under 6 months.")

#--------------------  Correlation chronic diseases and difficulties in accessing health care -----------------

dt_cor_chronic_access_health <- data_ind %>% filter(!is.na(chronic_ill)) %>% filter(!is.na(impact2_3_health)) %>% 
  mutate(chronic_ill = case_when(chronic_ill == 0 ~ "no",
                                 chronic_ill == 1 ~ "yes")) %>% 
  mutate(impact2_3_health = case_when(impact2_3_health == 0 ~ "no",
                                      impact2_3_health == 1 ~ "yes")) %>% 
  group_by(chronic_ill, impact2_3_health) %>% 
  summarise(n = sum(regional_weights)) %>% group_by(chronic_ill) %>% 
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( access_health = impact2_3_health) %>% 
  mutate(indicator = "Correlation of persons with chronic diseases and difficulties in accessing health care.")

#--------------------  Correlation disability and difficulties in accessing health care -----------------

dt_cor_disability_access_health <- data_ind %>% filter(!is.na(disability)) %>% filter(!is.na(impact2_3_health)) %>% 
  mutate(disability = case_when(disability == 0 ~ "no",
                                disability == 1 ~ "yes")) %>% 
  mutate(impact2_3_health = case_when(impact2_3_health == 0 ~ "no",
                                      impact2_3_health == 1 ~ "yes")) %>% 
  group_by(disability, impact2_3_health) %>% 
  summarise(n = sum(regional_weights)) %>% group_by(disability) %>% 
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%")) %>% rename( access_health = impact2_3_health) %>% 
  mutate(indicator = "Correlation of persons with disability and difficulties in accessing health care.")


#-------------------- Merge tabulations ----------------------------------------

dt_tabulation <- rbind(dt_GBV_access_Regional, dt_GBV_access_Countries, 
                       dt_FCS_21, dt_FCS_28,
                       dt_health_coping_regional, dt_health_coping_Countries,
                       dt_health_access_regional_ind, dt_health_access_countries_ind,
                       dt_health_need_regional, dt_health_need_countries,
                       dt_chronic_ill_regional, dt_chronic_ill_countries,
                       dt_chronic_ill_regional_ind, dt_chronic_ill_countries_ind,
                       dt_disability_regional_ind, dt_disability_countries_ind,
                       dt_disability_regional_HH, dt_disability_countries_HH,
                       dt_barrier_sexual_health_regional,dt_barrier_sexual_health_countries,
                       dt_measles_regional, dt_measles_countries,
                       dt_polio_regional, dt_polio_countries,
                       dt_mental_health_regional, dt_mental_health_countries,
                       dt_mental_health_sup_regional, dt_mental_health_sup_countries,
                       dt_mental_received_sup_regional, dt_mental_received_sup_countries,
                       dt_mental_health_improved_regional, dt_mental_health_improved_countries,
                       dt_timely_breastfeeding_regional,dt_timely_breastfeeding_countries,
                       dt_exclusive_breastfeeding_regional,dt_exclusive_breastfeeding_countries)

#reorder columns
dt_tabulation <- relocate(dt_tabulation, indicator, .before = answer)
dt_tabulation <- relocate(dt_tabulation, variable, .before = indicator)

#------------------- Export to excel ------------------------------------------

# iniate workbook object
wb <- createWorkbook()

# add worksheets to the workbook object
addWorksheet(wb, "Tabulation")
addWorksheet(wb, "Priority needs")
addWorksheet(wb, "Health barriers")
addWorksheet(wb, "GBV barriers")
addWorksheet(wb, "Mental health barriers")
addWorksheet(wb, "Cor chronic health")
addWorksheet(wb, "Cor disability health")


# write data to worksheets in the workbook object
writeData(wb, 1, dt_tabulation)
writeData(wb, 2, dt_needs)
writeData(wb, 3, dt_health_barrier)
writeData(wb, 4, dt_GBV_barrier)
writeData(wb, 5, dt_barrier_mental_health)
writeData(wb, 6, dt_cor_chronic_access_health)
writeData(wb, 7, dt_cor_disability_access_health)

# save the workbook to a file
saveWorkbook(wb, "Health_analysis_tabulations.xlsx")






