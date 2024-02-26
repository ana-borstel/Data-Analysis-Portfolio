## MSNA merging

#----- Packages -----
library(tidyverse)
library(dplyr)
library(readxl)
library(arsenal)
library(janitor)
library(Hmisc)
library(openxlsx)
library(table1)
library(stringr)

#---- Load data ----

#household
data_czechia_hh <- read_excel("//.xlsx")

data_slovakia_hh <- read_excel("//.xlsx")

data_poland_hh <- read_excel("//.xlsx")

colnames(data_poland_hh) <- gsub("/", "_", colnames(data_poland_hh))# Replace "/" with "_" in columns 

data_bulgaria_hh <- read_excel("//.xlsx")

data_romania_hh <- read_excel("//.xlsx")

data_hungary_hh <- read_excel("//.xlsx")

data_hungary_hh <-data_hungary_hh %>% 
  rename_all(~stringr::str_replace(.,"^@",""))

data_moldova_hh <- read_excel("//2.xlsx", 
                              skip = 1)


#individual

data_czechia_in <- read_excel("//.xlsx", 
                              sheet = "Info")

data_slovakia_in <- read_excel("//.xlsx", 
                               sheet = "Info")
  
data_poland_in <- read_excel("//.xlsx", 
                             sheet = "people")

colnames(data_poland_in) <- gsub("/", "_", colnames(data_poland_in))# Replace "/" with "_" in columns
  
data_bulgaria_in <- read_excel("//.xlsx", 
                               sheet = "Info")  
  
data_romania_in <- read_excel("//.xlsx", 
                              sheet = "Info")   

data_hungary_in <- read_excel("//.xlsx")

data_hungary_in <-data_hungary_in %>% 
  rename_all(~stringr::str_replace(.,"^@",""))

data_hungary_in <- rename(data_hungary_in, "_submission___version__" = "submission___version")
data_hungary_in <- rename(data_hungary_in, "_submission__id" = "submission__id")

data_moldova_in <- read_excel("//.xlsx", 
                              sheet = "HH ind ren", skip = 1)



#---- create column "country" ----

#HH
data_czechia_hh$country <- "Czechia"

data_slovakia_hh$country <- "Slovakia"

data_poland_hh$country <- "Poland"

data_bulgaria_hh$country <- "Bulgaria"

data_romania_hh$country <- "Romania"

data_hungary_hh$country <- "Hungary"

data_moldova_hh$country <- "Moldova"

#Individual
data_czechia_in$country <- "Czechia"

data_slovakia_in$country <- "Slovakia"

data_poland_in$country <- "Poland"

data_bulgaria_in$country <- "Bulgaria"

data_romania_in$country <- "Romania"

data_hungary_in$country <- "Hungary"

data_moldova_in$country <- "Moldova"


#---- Compare datasets ----

#HH
col_compare_hh <- compare_df_cols(data_czechia_hh,data_slovakia_hh, data_poland_hh, data_bulgaria_hh, data_romania_hh,  data_moldova_hh, data_hungary_hh)
write.xlsx(col_compare_hh, 'col_compare_hh.xlsx')

col_compare_dif_hh <- col_compare_hh[rowSums(is.na(col_compare_hh)) > 0,]#filter to only columns with NAs
write.xlsx(col_compare_dif_hh, 'col_compare_dif_hh_11_27_v2.xlsx')

#Individual
col_compare_in <- compare_df_cols(data_czechia_in,data_slovakia_in, data_poland_in, data_bulgaria_in, data_romania_in, data_moldova_in, data_hungary_in)
write.xlsx(col_compare_in, 'col_compare_in.xlsx')

col_compare_dif_in <- col_compare_in[rowSums(is.na(col_compare_in)) > 0,]#filter to only columns with NAs
write.xlsx(col_compare_dif_in, 'col_compare_dif_in.xlsx')

#---- Check different columns ----

table(data_moldova_in$H14_SM_UNA_ACC_lack_required_aid)


#---- rename columns that are the same, but named differently ----

#Household
data_poland_hh <- rename(data_poland_hh, "AAP.4_SM_PRF_FEEDBACK_dont_know" = "AAP.4_SM_PRF_FEEDBACK_do_not_know")
data_poland_hh <- rename(data_poland_hh, "AAP.4_SM_PRF_FEEDBACK_face_to_face" = "AAP.4_SM_PRF_FEEDBACK_face-to-face_interactions")

data_bulgaria_hh <- rename(data_bulgaria_hh, SE2.11b_SM_BEN_HST_other_source = SE2.11b_SM_BEN_HST_other)

data_slovakia_hh <- rename(data_slovakia_hh, SE2.12_SM_SOC_PRT_no_benefit = SE2.12_SM_SOC_PRT_no_benefits)

data_romania_hh <- rename(data_romania_hh, CP01_SM_RISK_B_do_not_know = CP01_SM_RISK_B_don_t_know)
data_romania_hh <- rename(data_romania_hh, CP01_SM_RISK_B_other = CP01_SM_RISK_B_other__please_specify)

data_moldova_hh <- rename(data_moldova_hh, CP03_SM_RPT_CASE_no_services = CP03_SM_RPT_CASE_do_not_know_any)

data_hungary_hh <- rename(data_hungary_hh, AAP.2_SM_ACC_INFO_information_not_available_in_accessible_formats = AAP.2_SM_ACC_INFO_information_not_available_in_accessible_format)
data_hungary_hh <- rename(data_hungary_hh, CP01_SM_RISK_B_increased_risks_of_separation_from_the_family_and_or_placement_into_residential_facility = CP01_SM_RISK_B_increased_risks_of_separation_from_the_family_and)


#Individual

data_hungary_in <- rename(data_hungary_in, "H4.1_SM_HLTH_ACC_GREV_inadequate_communication_with_healthcare_provider" = "H4.1_SM_HLTH_ACC_GREV_inadequate_communication_with_healthcare_p")
data_hungary_in <- rename(data_hungary_in, "H4.1_SM_HLTH_ACC_GREV_inadequate_explanation_or_understanding_of_medical_conditions_and_treatment_option" = "H4.1_SM_HLTH_ACC_GREV_inadequate_explanation_or_understanding_of")
data_hungary_in <- rename(data_hungary_in, "H4.1_SM_HLTH_ACC_GREV_insufficient_availability_of_medications_or_medical_supplies" = "H4.1_SM_HLTH_ACC_GREV_insufficient_availability_of_medications_o")


#---- merge datasets ----

#household

#harmonize data
data_moldova_hh$today <- as.Date(data_moldova_hh$today)
data_moldova_hh <- rename(data_moldova_hh, H12_NUM_WOM_BARR_2 = H12_NUM_WOM_BARR)

data_hungary_hh$total_fem_10_55 <- as.character(data_hungary_hh$total_fem_10_55)
data_hungary_hh$total_girls <- as.character(data_hungary_hh$total_girls)
data_hungary_hh$total_boys <- as.character(data_hungary_hh$total_boys)
data_hungary_hh$total_women <- as.character(data_hungary_hh$total_women)
data_hungary_hh$total_men <- as.character(data_hungary_hh$total_men)
data_hungary_hh$total_females <- as.character(data_hungary_hh$total_females)
data_hungary_hh$total_males <- as.character(data_hungary_hh$total_males)

data_total_hh <- dplyr::bind_rows(data_czechia_hh, data_slovakia_hh, data_poland_hh, data_bulgaria_hh, data_romania_hh, data_moldova_hh, data_hungary_hh)

#individual 

#harmonizing Poland data for merging with other countries
data_poland_in$DR.15_NUM_INHC <- as.numeric(data_poland_in$DR.15_NUM_INHC)#ok

data_poland_in$E2_SM_RES_NO_EDU_OTH <- as.character(data_poland_in$E2_SM_RES_NO_EDU_OTH)#ok

#class(data_poland_in$"_submission__submission_time")
#class(data_czechia_in$"_submission__submission_time")

data_poland_in$"_submission__submission_time" <- as.Date.POSIXct(data_poland_in$"_submission__submission_time")

#harmonizing Moldova data

data_moldova_in$H14_SM_UNA_ACC_cannot_afford_fee <- as.numeric(data_moldova_in$H14_SM_UNA_ACC_cannot_afford_fee)
data_moldova_in$H14_SM_UNA_ACC_did_not_know <- as.numeric(data_moldova_in$H14_SM_UNA_ACC_did_not_know)
data_moldova_in$H14_SM_UNA_ACC_dont_know <- as.numeric(data_moldova_in$H14_SM_UNA_ACC_dont_know)

#merge

data_total_in <- dplyr::bind_rows(data_czechia_in, data_slovakia_in, data_poland_in, data_bulgaria_in, data_romania_in, data_moldova_in, data_hungary_in)

#---- Fix column index ----

data_total_hh$unique_index <- paste(data_total_hh$country, data_total_hh$"_index", sep="_")

data_total_in$unique_parent_index <- paste(data_total_in$country, data_total_in$"_parent_index", sep="_")

#---- reorganize columns ----

data_total_hh <- relocate(data_total_hh, country) #move to first column

data_total_hh <- relocate(data_total_hh, unique_index, .after = country) #or move to after another column

data_total_in <- relocate(data_total_in, country) 

data_total_in <- relocate(data_total_in, unique_parent_index, .after = country)

#---- create excel file for merged dataset ----

write.xlsx(data_total_hh, 'MSNA_merged_total_hh.xlsx')

write.xlsx(data_total_in, 'MSNA_merged_total_in.xlsx')

#---- delete columns that are not relevant for regional analysis ----

#columns not in original kobo and empty + columns with introduction/note
data_regional_hh <- data_total_hh[,-which(names(data_total_hh) %in% c ("child_prot_001", "respondent_months", "thank", "thank_you",
                                          "contact_info_tool", "DR7.1_SS_WILL_NOTE", "INT06_SS_LOC_TYPE_WARN",
                                          "note_hh_members_female", "note_hh_members_male", "note_hh_members_other",
                                          "SE2.11_sum", "INT00_NOTE_CON", "INT01_SS_CON", "INT01_SS_CON_NOTE", 
                                          "INT02_SS_AGE", "INT02.1_SS_AGE_NOTE", "_notes", "_tags"))]


#check if columns were deleted
col_comp_hh_regional <- compare_df_cols(data_czechia_hh,data_slovakia_hh, data_poland_hh, data_bulgaria_hh, data_romania_hh,  data_moldova_hh, data_hungary_hh, data_regional_hh)
col_comp_dif_hh_regional <- col_comp_hh_regional[rowSums(is.na(col_comp_hh_regional)) > 0,]#filter to only columns with NAs
write.xlsx(col_comp_dif_hh_regional, 'col_comp_dif_hh_regional.xlsx')



#---- create excel file for regional dataset ----

write.xlsx(data_regional_hh, 'MSNA_merged_regional_hh.xlsx')

write.xlsx(data_regional_in, 'MSNA_merged_regional_in.xlsx')

