## Centre for International Security
# Mediterranean Reporting Project
# R Script for matching cases of dead and missing migrants in the Mediterranean


#--------------------- R packages ----------------------------------------------

library(tidyr)
library(tidyverse)
library(arsenal)
library(readxl)
library(readr)
library(fuzzyjoin)
library(openxlsx)
library(stringr)
library(ggplot2)
library(stargazer)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(geosphere)
library(osmdata)


#------------------- Load data -------------------------------------------------

setwd("~/Documents/GitHub/Mediterranean Counting")

United <-read_excel("///")

Migrants_files <- read_csv("///")

Missing_Migrants <- read_excel("////")


#----------------- Rename columns ----------------------------------------------

# rename columns in Missing Migrants list
Missing_Migrants <- rename(Missing_Migrants, MM_ID = 1)
Missing_Migrants <- rename(Missing_Migrants, MM_incident_ID = 2)
Missing_Migrants <- rename(Missing_Migrants, MM_incident_type = 3)
Missing_Migrants <- rename(Missing_Migrants, MM_region_incident = 4)
Missing_Migrants <- rename(Missing_Migrants, MM_date = 5)
Missing_Migrants <- rename(Missing_Migrants, year = 6 )
Missing_Migrants <- rename(Missing_Migrants, MM_n_dead = 8)
Missing_Migrants <- rename(Missing_Migrants, MM_n_missing = 9)
Missing_Migrants <- rename(Missing_Migrants, MM_n_dead_missing = 10)
Missing_Migrants <- rename(Missing_Migrants, MM_n_survivors = 11)
Missing_Migrants <- rename(Missing_Migrants, MM_n_females = 12)
Missing_Migrants <- rename(Missing_Migrants, MM_n_males = 13)
Missing_Migrants <- rename(Missing_Migrants, MM_n_children = 14)
Missing_Migrants <- rename(Missing_Migrants, MM_region_origin = 15)
Missing_Migrants <- rename(Missing_Migrants, MM_cause_death = 16)
Missing_Migrants <- rename(Missing_Migrants, MM_country_origin = 17)
Missing_Migrants <- rename(Missing_Migrants, MM_migration_route = 18)
Missing_Migrants <- rename(Missing_Migrants, MM_location = 19)
Missing_Migrants <- rename(Missing_Migrants, MM_source = 20)
Missing_Migrants <- rename(Missing_Migrants, MM_coordinates = 21)
Missing_Migrants <- rename(Missing_Migrants, MM_description = 23)
Missing_Migrants <- rename(Missing_Migrants, MM_URL = 25)

# rename columns in Migrants Files list
Migrants_files <- rename(Migrants_files, MF_ID = 1)
Migrants_files <- rename(Migrants_files, MF_date = 5)
Migrants_files <- rename(Migrants_files, MF_n_dead = 9)
Migrants_files <- rename(Migrants_files, MF_n_missing = 10)
Migrants_files <- rename(Migrants_files, MF_n_dead_missing = 11)
Migrants_files <- rename(Migrants_files, MF_description = 13)
Migrants_files <- rename(Migrants_files, MF_location = 14)
Migrants_files <- rename(Migrants_files, MF_latitude = 15)
Migrants_files <- rename(Migrants_files, MF_longitude = 16)
Migrants_files <- rename(Migrants_files, MF_name_news = 19)
Migrants_files <- rename(Migrants_files, MF_route = 20)
Migrants_files <- rename(Migrants_files, MF_source = 21)
Migrants_files <- rename(Migrants_files, year = Year)
Migrants_files <- rename(Migrants_files, MF_URL = 22)

# rename columns in United list
United <- rename(United, U_ID = 1)
United <- rename(United, U_date = 2)
United <- rename(United, U_n_dead_missing = 5)
United <- rename(United, U_name_gender = 6)
United <- rename(United, U_region_origin = 7)
United <- rename(United, U_cause_death = 8)
United <- rename(United, U_source = 9)
United <- rename(United, U_country_death = 11)
United <- rename(United, U_place_death = 12)
United <- rename(United, U_latitude = 13)
United <- rename(United, U_longitude = 14)
United <- rename(United, U_URL = 116)
United <- rename(United, U_description = 118)
United <- rename(United, U_merge = 120)

#-------- Restrict data to 2014 onward ----------------------------------------

# Missing Migrants data
Missing_Migrants$MM_date <- as.Date(Missing_Migrants$MM_date, "%Y-%m-%d")#date format

Missing_Migrants <- Missing_Migrants %>%#filter data by year
  filter(year >= 2014)

#Missing_Migrants <- Missing_Migrants %>% filter(MM_date <= "2016-06-01")#1630


# Migrants Files data
Migrants_files$MF_date <- as.Date(Migrants_files$MF_date, "%Y-%m-%d")#date format

Migrants_files <- Migrants_files %>% filter(year >= 2014)#431

Migrants_files$MF_ID_new <- seq(from = 1, to = 431)#create new ID number

#Migrants_files <- Migrants_files %>% filter(MF_date <= "2016-06-01")#422


# United data
United$U_date <- as.Date(United$U_date, "%Y-%m-%d")

United$year <- format(as.Date(United$U_date), "%Y")

United$year  <- as.numeric(United$year)

United <- filter(United, year >= 2014)#3542

#United <- United %>% filter(U_date <= "2016-06-01")#585


#--------- Restrict data to region of analysis (Mediterranean) -----------------

# Missing Migrants data
Missing_Migrants <- Missing_Migrants %>% #exclude regions not relevant
  filter(!MM_region_incident %in% c("Caribbean", "Central America","North America", 
                                    "South America", "South-eastern Asia", "Southern Africa",
                                    "Southern Asia","Western Asia", "Eastern Africa"))#854

Missing_Migrants <- Missing_Migrants %>% #exclude routes not relevant
  filter(!MM_migration_route %in% c("English Channel to the UK","Italy to France", 
                                    "Sahara Desert crossing", 
                                    "Western Africa / Atlantic route to the Canary Islands",
                                     "Western Balkans", "Horn of Africa to Yemen crossing"))#434

Missing_Migrants <- Missing_Migrants[!grepl("China|Russia|United Kingdom|Austria| #delete the rows that contain specific locations not relevant
                                             Netherlands|Sudan|Eritrea|Sinai|
                                             Belgium|Macau|UK|Israel|Gaza", 
                                            Missing_Migrants$MM_location),]#392

Missing_Migrants <- Missing_Migrants %>% drop_na(MM_migration_route) #clean events without information on the route


# Migrants Files data
Migrants_files <- Migrants_files %>% 
  filter(!MF_route %in% c("Western African route", "Western Balkan route"))#403

Migrants_files <- Migrants_files[!grepl("Brussels|Hökarängen|Luxembourg|Calais|
                                         Ljusne|Rijn|Paris|Hassloch|Kidal|Sinai|
                                         Sunndal|Berlin|Vienna|Folkestone|Niger|
                                         Amsterdam|Gaulle|Amygdaleza|Tilbury|Yarl|
                                         Harwich|Dirkou|Saafeld|Trent|Idlib|Sinai|
                                         Rafah|Gravelines|Cherbourg|Coquelles|
                                         Frankfurt|Munich|Oxfordshire", 
                                        Migrants_files$MF_location),]#345

Migrants_files <- Migrants_files %>% 
  filter(!cause_of_death %in% c("bombed", "car accident", "crushed by a wooden pallet in a truck",
                                "died in a sewer", "electrocuted", "fell off a train", 
                                "hit by a car", "hit by a train", "hit by a truck", 
                                "intoxicated", "ran over by a truck", "run over", 
                                "run over by a bus", "run over by a car",
                                "run over by a truck"))#329


# United data

United <- United %>% 
  filter(!U_country_death %in% c("Afghanistan",  "Austria", "Belgium", "Bulgaria",
                                 "Djibouti", "Estonia", "Finland", "Germany", 
                                 "Great Britain",  "Hungary", "Ireland", "Luxembourg", 
                                 "Macedonia", "Netherlands", "Niger", "Norway", 
                                 "Russia", "Scotland", "Serbia", "Sweden", 
                                 "Switzerland", "North Macedonia"))#431

United <- United[!grepl("Isonzo|Rome|Calais|Paris|Black Sea|Canaria|Canary|North Atlantic Ocean", 
                        United$U_place_death),]#390

#colnames(United)

United <- United %>% filter((`truck/car/ bus` !=1) %>% replace_na(TRUE)) #remove rows with truck/car/bus death related
United <- United %>% filter((`truck/car/ bus` !=2) %>% replace_na(TRUE))
United <- United %>% filter((`truck/car/ bus` !=6) %>% replace_na(TRUE))
United <- United %>% filter((`truck/car/ bus` !=7) %>% replace_na(TRUE))#382

United <- United %>% filter((`train` !=1) %>% replace_na(TRUE)) #remove rows with train death related
United <- United %>% filter((`train` !=14) %>% replace_na(TRUE))#377

United <- United %>% filter((`car accident` !=1) %>% replace_na(TRUE))#remove rows with car accident related
United <- United %>% filter((`car accident` !=2) %>% replace_na(TRUE))#
United <- United %>% filter((`car accident` !=9) %>% replace_na(TRUE))#372

United <- United %>% filter((`asylum/refugee centre` !=1) %>% replace_na(TRUE))#remove rows with asylum/refugee centre death related
United <- United %>% filter((`asylum/refugee centre` !=18) %>% replace_na(TRUE))#371

United <- United %>% filter((`in detention` !=1) %>% replace_na(TRUE))#remove rows with detention death related
United <- United %>% filter((`in detention` !=4) %>% replace_na(TRUE))#365

United <- United %>% filter((`in prison` !=1) %>% replace_na(TRUE))#remove rows with prison death related#363#354


#---------------- Delete and add columns ---------------------------------------------

# Missing Migrants data
Missing_Migrants <- Missing_Migrants[, -which(names(Missing_Migrants) %in% 
                      c("Source Quality", "UNSD Geographical Grouping", "Reported Month"))]

Missing_Migrants$MM_list <- 1 #create dummy variable for event in the list


# Migrant Files data  

Migrants_files <- Migrants_files[, -which(names(Migrants_files) %in% 
                    c("CartoDB_Cause_of_death","dataset","quarter", "Date-month",
                     "Intent of going to Eur: 1(yes) 0(not confirmed)", "latitude, 
                     longitude","Somme Dedoublement"))]

Migrants_files$MF_list <- 1 # create dummy variable for event in the list


# United data

United <- select(United, "U_ID", "U_date", "U_n_dead_missing", "U_name_gender", 
                 "U_region_origin", "U_cause_death", "U_source", "U_country_death", 
                 "U_place_death", "U_latitude", "U_longitude", "U_description", 
                 "U_URL", "year", "U_merge")

United$U_list <- 1 # create dummy variable for event in the list


#----------- Manually clean events unrelated to the research -------------------

# Missing Migrants data
MM_clean <- Missing_Migrants %>% filter(!MM_ID %in% 
              c("2014.MMP00046", "2014.MMP00055", "2014.MMP00090")) #360

# Migrants Files data
MF_clean <- Migrants_files %>% filter(!MF_ID %in% #original ID numbers
              c(55402,54508,72748))#291

MF_clean <- MF_clean %>% filter(!MF_ID_new %in% #assigned ID numbers
              c(181,335,147))#291

# United data
U_clean <- United %>% filter(!U_ID %in% 
             c(3010,3437,3439))#287


#United_2016 <- filter(U_clean, year == 2016)

#-------- Update and merge the same events -------------------------------------

#Missing Migrants events
subset_data <- MM_clean[MM_clean$MM_ID == "2014.MMP00114",]
subset_data$MM_n_missing <- 90
MM_clean[MM_clean$MM_ID == "2014.MMP00114",] <- subset_data
subset_data <- MM_clean[MM_clean$MM_ID == "2014.MMP00114",]
subset_data$MM_n_dead_missing <- 100
MM_clean[MM_clean$MM_ID == "2014.MMP00114",] <- subset_data


# United events
selected_rows <- U_clean[U_clean$U_ID %in% c(3031, 4747, 4748, 4749),] #Subset the data frame to selected rows
new_value <- sum(selected_rows$U_n_dead_missing) #Calculate the new value of U_n_dead_missing
selected_rows$U_n_dead_missing <- new_value #Update the value of U_n_dead_missing for the selected rows
U_clean[U_clean$U_ID %in% c(3031, 4747, 4748, 4749), ] <- selected_rows #Update the original data frame with the changes
U_clean <- U_clean %>% filter(!U_ID %in% c(3031,4747,4748))


# Migrants Files events
subset_data <- MF_clean[MF_clean$MF_ID_new == 408,]
subset_data$MF_route <- "Eastern Mediterranean route"
MF_clean[MF_clean$MF_ID_new == 408,] <- subset_data


#--------- Subset lists based on route to match events -------------------------

# Missing Migrants data
MM_CM <- filter(MM_clean, MM_migration_route == "Central Mediterranean")

MM_EM <- filter(MM_clean, MM_migration_route == "Eastern Mediterranean")

MM_WM <- filter(MM_clean, MM_migration_route == "Western Mediterranean")

# Migrants Files data
MF_CM <- filter(MF_clean, MF_route == "Central Mediterranean route" | MF_route == "Central Mediterranean Route")

MF_CM$MF_route[MF_CM$MF_route == "Central Mediterranean route"] <- "Central Mediterranean"

MF_CM$MF_route[MF_CM$MF_route == "Central Mediterranean Route"] <- "Central Mediterranean"

MF_EM <- filter(MF_clean, MF_route == "Eastern Mediterranean route")

MF_EM$MF_route[MF_EM$MF_route == "Eastern Mediterranean route"] <- "Eastern Mediterranean"

MF_WM <- filter(MF_clean, MF_route == "Western Mediterranean route" | MF_route == "Western Mediterranean Route")

MF_WM$MF_route[MF_WM$MF_route == "Western Mediterranean route"] <- "Western Mediterranean"

MF_WM$MF_route[MF_WM$MF_route == "Western Mediterranean Route"] <- "Western Mediterranean"


# United data
U_CM <- filter(U_clean, U_country_death == "Italy" | U_country_death == "Libya" | 
               U_country_death == "France" | U_country_death == "Tunisia" | 
                 U_country_death == "Egypt" | U_country_death == "Malta" | 
                 U_ID %in% c(6731, 4689, 4698, 3398, 4300, 5861, 3244, 3240, 
                             3238, 6269, 3507, 3231))

U_CM$U_route <- "Central Mediterranean"

U_EM <- filter(U_clean, U_country_death == "Greece" | U_country_death == "Turkey" | 
                 U_ID %in% c(3387, 3397, 3289))

U_EM$U_route <- "Eastern Mediterranean"

U_WM <- filter(U_clean, U_country_death == "Morocco" | U_country_death == "Spain")

U_WM$U_route <- "Western Mediterranean"


#----------- Merging 1st step (Central Med): intersection MM & MF lists --------

#intersection MM & MF for CM
MM_MF_CM <-fuzzyjoin::difference_left_join(MM_CM, MF_CM, by = c("MM_date" = "MF_date"), max_dist = 4) #merge lists by date #438

MM_MF_CM <-mutate(MM_MF_CM, diff_MM_MF = MM_n_dead_missing - MF_n_dead_missing) #calculate difference between dead and missing numbers in both lists

MM_MF_CM$diff_MM_MF <- abs(MM_MF_CM$diff_MM_MF) #transform the difference into positive numbers only

MM_MF_CM <-mutate(MM_MF_CM, diff_dates = MM_date - MF_date) #calculate difference in dates

MM_MF_CM$diff_dates <- abs(MM_MF_CM$diff_dates)

MM_MF_CM <- filter(MM_MF_CM, diff_MM_MF <= 1 | # 1 or less difference in "dead and missing"
                  (MM_n_dead_missing >= 8 & diff_MM_MF <= 2) |
                  (MM_n_dead_missing >= 9 & diff_MM_MF <= 3) |
                  (MM_n_dead_missing >= 20 & diff_MM_MF <= 14) | 
                  (MM_n_dead_missing >= 100 & diff_MM_MF<= 65) | 
                  (MM_n_dead_missing >= 800 & diff_MM_MF<= 300)) #136

#MM_MF_CM <- select(MM_MF_CM, "MM_ID", "MF_ID","MF_ID_new", "MM_date", "MF_date", 
#                "MM_n_dead_missing","MF_n_dead_missing","MM_n_dead","MF_n_dead", "MM_n_survivors", "MM_description",
#                "MF_description", "MM_location", "MF_location", "diff_MM_MF")

# manually clean matching cases that are not the same
MM_MF_CM <- MM_MF_CM[!(MM_MF_CM$MM_ID == "2014.MMP00080" & MM_MF_CM$MF_ID_new == 399) & !(MM_MF_CM$MM_ID == "2014.MMP00149" & MM_MF_CM$MF_ID_new == 367) &
                 !(MM_MF_CM$MM_ID == "2014.MMP00152" & MM_MF_CM$MF_ID == 72744) & !(MM_MF_CM$MM_ID == "2014.MMP00152" & MM_MF_CM$MF_ID_new == 371) &
                 !(MM_MF_CM$MM_ID == "2014.MMP00162" & MM_MF_CM$MF_ID == 72742) & !(MM_MF_CM$MM_ID == "2014.MMP00170" & MM_MF_CM$MF_ID == 72742), ]#year 2014 (112)

MM_MF_CM  <- MM_MF_CM [!(MM_MF_CM$MM_ID == "2015.MMP00094" & MM_MF_CM$MF_ID == 78114)   & !(MM_MF_CM$MM_ID == "2015.MMP00095" & MM_MF_CM$MF_ID == 78112)   &
                                   !(MM_MF_CM$MM_ID == "2015.MMP00103" & MM_MF_CM$MF_ID_new == 325)   & !(MM_MF_CM$MM_ID == "2015.MMP00223" & MM_MF_CM$MF_ID_new == 289) &          
                                   !(MM_MF_CM$MM_ID == "2015.MMP00125" & MM_MF_CM$MF_ID == 80995)   & !(MM_MF_CM$MM_ID == "2015.MMP00146" & MM_MF_CM$MF_ID_new == 309), ]# year 2015 (100)


MM_MF_CM  <- MM_MF_CM [!(MM_MF_CM$MM_ID == "2016.MMP00265" & MM_MF_CM$MF_ID_new == 73) & !(MM_MF_CM$MM_ID == "2016.MMP00266" & MM_MF_CM$MF_ID_new == 75) &
                           !(MM_MF_CM$MM_ID == "2016.MMP00266" & MM_MF_CM$MF_ID_new == 66) &  
                           !(MM_MF_CM$MM_ID == "2016.MMP00303" & MM_MF_CM$MF_ID_new == 63) & !(MM_MF_CM$MM_ID == "2016.MMP00303" & MM_MF_CM$MF_ID_new == 70), ]#2016 (87)

#----------- Merging 1st step (Eastern Med): intersection MM & MF lists --------

MM_MF_EM <-fuzzyjoin::difference_left_join(MM_EM, MF_EM, by = c("MM_date" = "MF_date"), max_dist = 3)
#974

MM_MF_EM <-mutate(MM_MF_EM, diff_MM_MF = MM_n_dead_missing - MF_n_dead_missing)#calculate difference between dead and missing numbers in both lists
MM_MF_EM$diff_MM_MF <- abs(MM_MF_EM$diff_MM_MF)#transform the difference into positive numbers only

MM_MF_EM <- filter(MM_MF_EM, diff_MM_MF <= 1 | (MM_n_dead_missing >= 8 & diff_MM_MF <= 3) |(MM_n_dead_missing >= 20 & diff_MM_MF <= 10) | MM_n_dead_missing >= 100 & diff_MM_MF<= 65 | MM_n_dead_missing >= 800 & diff_MM_MF<= 300)#filter the cases based on difference in number of dead and missings from both lists
#315 cases#322

#MM_MF_EM <- select(MM_MF_EM, "MM_ID", "MF_ID","MF_ID_new", "MM_date", "MF_date", 
#                        "MM_n_dead_missing","MF_n_dead_missing", "MM_n_survivors", "MM_description", 
#                        "MF_description","MM_location", "MF_location", "diff_MM_MF")

#clean cases that are not a match
MM_MF_EM <- MM_MF_EM[!(MM_MF_EM$MM_ID == "2014.MMP00045" & MM_MF_EM$MF_ID_new == 408) &
                                 !(MM_MF_EM$MM_ID == "2015.MMP00303" & MM_MF_EM$MF_ID_new == 276) & !(MM_MF_EM$MM_ID == "2015.MMP00306" & MM_MF_EM$MF_ID_new == 277), ]#2015 (174)


MM_MF_EM <- MM_MF_EM[!(MM_MF_EM$MM_ID == "2016.MMP00371" & MM_MF_EM$MF_ID_new == 42) & !(MM_MF_EM$MM_ID == "2016.MMP00371" & MM_MF_EM$MF_ID_new == 47) &
                       !(MM_MF_EM$MM_ID == "2016.MMP00371" & MM_MF_EM$MF_ID_new == 49) & !(MM_MF_EM$MM_ID == "2016.MMP00309" & MM_MF_EM$MF_ID_new == 67) &
                       !(MM_MF_EM$MM_ID == "2016.MMP00309" & MM_MF_EM$MF_ID_new == 69) & !(MM_MF_EM$MM_ID == "2016.MMP00308" & MM_MF_EM$MF_ID_new == 68),]#2016 (102)

#----------- Merging 1st step (Western Med): intersection MM & MF lists --------

MM_MF_WM <-fuzzyjoin::difference_left_join(MM_WM, MF_WM, by = c("MM_date" = "MF_date"), max_dist = 3)
#39

MM_MF_WM <-mutate(MM_MF_WM, diff_MM_MF = MM_n_dead_missing - MF_n_dead_missing)#calculate difference between dead and missing numbers in both lists
MM_MF_WM$diff_MM_MF <- abs(MM_MF_WM$diff_MM_MF)#transform the difference into positive numbers only

MM_MF_WM <- filter(MM_MF_WM, diff_MM_MF <= 1 |(MM_n_dead_missing >= 8 & diff_MM_MF <= 2) | (MM_n_dead_missing >= 20 & diff_MM_MF <= 10) | MM_n_dead_missing >= 100 & diff_MM_MF<= 65 | MM_n_dead_missing >= 800 & diff_MM_MF<= 300)#filter the cases based on difference in number of dead and missings from both lists
#12

#MM_MF_WM <- select(MM_MF_WM, "MM_ID", "MF_ID","MF_ID_new", "MM_date", "MF_date", 
#                   "MM_n_dead_missing","MF_n_dead_missing", "MM_n_survivors", "MM_description", 
#                   "MF_description","MM_location", "MF_location", "diff_MM_MF")

MM_MF_WM <- MM_MF_WM[!(MM_MF_WM$MM_ID == "2016.MMP00427" & MM_MF_WM$MF_ID_new == 28),]#11

#----------- Merging 2nd step (Central Med): intersection MM, MF & U lists -----

MM_MF_U_CM <-fuzzyjoin::difference_left_join(MM_MF_CM, U_CM, by = c("MM_date" = "U_date"), max_dist = 4)#merge lists by date 
#210#213

MM_MF_U_CM <-mutate(MM_MF_U_CM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MM and U list
MM_MF_U_CM <-mutate(MM_MF_U_CM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MF and U list
MM_MF_U_CM$diff_MM_U <- abs(MM_MF_U_CM$diff_MM_U)#transform the difference into positive numbers only
MM_MF_U_CM$diff_MF_U <- abs(MM_MF_U_CM$diff_MF_U)#transform the difference into positive numbers only


MM_MF_U_CM <- filter(MM_MF_U_CM, diff_MM_U <= 2 | diff_MF_U <= 2 |(MM_n_dead_missing >= 8 & diff_MM_U <= 2 & diff_MF_U <= 2) |(MM_n_dead_missing >= 20 & diff_MM_U <= 10 & diff_MF_U <= 10) | 
                            MM_n_dead_missing >= 100 & diff_MM_U<= 65  & diff_MF_U<= 65 | 
                            MM_n_dead_missing >= 800 & diff_MM_U<= 300  & diff_MF_U<= 300)#filter the cases based on difference in number of dead and missings from both lists
#58 cases

#MM_MF_U_CM <- select(MM_MF_U_CM, "MM_ID", "MF_ID","MF_ID_new","U_ID", "MM_date", "MF_date", "U_date",
#                          "MM_n_dead_missing","MF_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
#                          "MF_description","U_cause_death", "U_description","MM_location", "MF_location", "U_place_death", "diff_MM_U", "diff_MF_U")

MM_MF_U_CM <- MM_MF_U_CM[!(MM_MF_U_CM$MM_ID == "2014.MMP00102" & MM_MF_U_CM$MF_ID == 54563) & 
                     !(MM_MF_U_CM$MM_ID == "2014.MMP00181" & MM_MF_U_CM$MF_ID == 56549), ]#2014 (51)

MM_MF_U_CM <- MM_MF_U_CM[!(MM_MF_U_CM$MM_ID == "2015.MMP00094" & MM_MF_U_CM$U_ID == 3240) & !(MM_MF_U_CM$MM_ID == "2015.MMP00094" & MM_MF_U_CM$U_ID == 6269) &
                         !(MM_MF_U_CM$MM_ID == "2015.MMP00095" & MM_MF_U_CM$U_ID == 3240) & !(MM_MF_U_CM$MM_ID == "2015.MMP00095" & MM_MF_U_CM$U_ID == 6269), ]#2015 (40)

#----------- Merging 2nd step (Eastern Med): intersection MM, MF & U lists -----
MM_MF_U_EM <-fuzzyjoin::difference_left_join(MM_MF_EM, U_EM, by = c("MM_date" = "U_date"), max_dist = 4)#merge lists by date 
#497#514

MM_MF_U_EM <-mutate(MM_MF_U_EM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MM and U list
MM_MF_U_EM <-mutate(MM_MF_U_EM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MF and U list
MM_MF_U_EM$diff_MM_U <- abs(MM_MF_U_EM$diff_MM_U)#transform the difference into positive numbers only
MM_MF_U_EM$diff_MF_U <- abs(MM_MF_U_EM$diff_MF_U)#transform the difference into positive numbers only


MM_MF_U_EM <- filter(MM_MF_U_EM, diff_MM_U <= 2 | diff_MF_U <= 2 |(MM_n_dead_missing >= 8 & diff_MM_U <= 2 & diff_MF_U <= 2) |(MM_n_dead_missing >= 20 & diff_MM_U <= 10 & diff_MF_U <= 10) | 
                            MM_n_dead_missing >= 100 & diff_MM_U<= 65  & diff_MF_U<= 65 | 
                            MM_n_dead_missing >= 800 & diff_MM_U<= 300  & diff_MF_U<= 300)#filter the cases based on difference in number of dead and missings from both lists
#175 cases#180

#MM_MF_U_EM <- select(MM_MF_U_EM, "MM_ID", "MF_ID","MF_ID_new","U_ID", "MM_date", "MF_date", "U_date",
#                          "MM_n_dead_missing","MF_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
#                          "MF_description","U_cause_death", "U_description","MM_location", "MF_location", "U_place_death", "diff_MM_U", "diff_MF_U")

MM_MF_U_EM <- MM_MF_U_EM[!(MM_MF_U_EM$MM_ID == "2015.MMP00346" & MM_MF_U_EM$MF_ID_new == 268) & !(MM_MF_U_EM$MM_ID == "2015.MMP00366" & MM_MF_U_EM$U_ID == 3547) &
                                     !(MM_MF_U_EM$MM_ID == "2015.MMP00393" & MM_MF_U_EM$U_ID == 3396) & !(MM_MF_U_EM$MM_ID == "2015.MMP00394" & MM_MF_U_EM$U_ID == 3852), ]#2015 (52)


MM_MF_U_EM <- MM_MF_U_EM[!(MM_MF_U_EM$MM_ID == "2016.MMP00001" & MM_MF_U_EM$U_ID == 5374) & !(MM_MF_U_EM$MM_ID == "2016.MMP00004" & MM_MF_U_EM$U_ID == 5374) & 
                         !(MM_MF_U_EM$MM_ID == "2016.MMP00005" & MM_MF_U_EM$MF_ID_new == 143) & !(MM_MF_U_EM$MM_ID == "2016.MMP00006" & MM_MF_U_EM$MF_ID_new == 138), ]#2016 (33)

#----------- Merging 2nd step (Western Med): intersection MM, MF & U lists -----
MM_MF_U_WM <-fuzzyjoin::difference_left_join(MM_MF_WM, U_WM, by = c("MM_date" = "U_date"), max_dist = 4)#merge lists by date 
#11

MM_MF_U_WM <-mutate(MM_MF_U_WM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MM and U list
MM_MF_U_WM <-mutate(MM_MF_U_WM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in MF and U list
MM_MF_U_WM$diff_MM_U <- abs(MM_MF_U_WM$diff_MM_U)#transform the difference into positive numbers only
MM_MF_U_WM$diff_MF_U <- abs(MM_MF_U_WM$diff_MF_U)#transform the difference into positive numbers only

#MM_MF_U_WM <- select(MM_MF_U_WM, "MM_ID", "MF_ID","MF_ID_new","U_ID", "MM_date", "MF_date", "U_date",
#                          "MM_n_dead_missing","MF_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
#                          "MF_description","U_cause_death", "U_description","MM_location", "MF_location", "U_place_death", "diff_MM_U", "diff_MF_U")

MM_MF_U_WM <- filter(MM_MF_U_WM, diff_MM_U <= 2 | diff_MF_U <= 2 |(MM_n_dead_missing >= 8 & diff_MM_U <= 2 & diff_MF_U <= 2) |(MM_n_dead_missing >= 20 & diff_MM_U <= 10 & diff_MF_U <= 10) | 
                            MM_n_dead_missing >= 100 & diff_MM_U<= 65  & diff_MF_U<= 65 | 
                            MM_n_dead_missing >= 800 & diff_MM_U<= 300  & diff_MF_U<= 300)#filter the cases based on difference in number of dead and missings from both lists
#7 cases


#---- Merging 3rd step (Central Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and MF lists only ----

MF_MM_CM <- anti_join(MM_MF_CM, MM_MF_U_CM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#44 cases

MM_MF_CM <- anti_join(MM_MF_CM, MM_MF_U_CM, by = "MM_ID")#subtraction for the MM list  --> this list is final
#44 cases

#---- Merging 3rd step (Eastern Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and MF lists only ----

MF_MM_EM <- anti_join(MM_MF_EM, MM_MF_U_EM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#70 cases

MM_MF_EM <- anti_join(MM_MF_EM, MM_MF_U_EM, by = "MM_ID")#subtraction for the MM list  --> this list is final
#70

#---- Merging 3rd step (Western Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and MF lists only ----
MF_MM_WM <- anti_join(MM_MF_WM, MM_MF_U_WM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#4 cases

MM_MF_WM <- anti_join(MM_MF_WM, MM_MF_U_WM, by = "MM_ID")#subtraction for the MM list  --> this list is final
#4


#----------- Merging 4th step (Central Med): intersection MM & U ----------------

MM_U_CM <-fuzzyjoin::difference_left_join(MM_CM, U_CM, by = c("MM_date" = "U_date"), max_dist = 3)
#288 cases

MM_U_CM <-mutate(MM_U_CM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MM_U_CM$diff_MM_U <- abs(MM_U_CM$diff_MM_U)#transform the difference into positive numbers only

MM_U_CM <- filter(MM_U_CM, diff_MM_U <= 1 |(MM_n_dead_missing >= 8 & diff_MM_U <= 5) | (MM_n_dead_missing >= 20 & diff_MM_U <= 10) | MM_n_dead_missing >= 100 & diff_MM_U<= 65)
#73 

MM_U_CM <- select(MM_U_CM, "MM_ID", "U_ID", "MM_date", "U_date",
                  "MM_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
                   "U_description","MM_location", "U_place_death", "diff_MM_U")

# 2014
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2014.MMP00099" & MM_U_CM$U_ID == 3231)  & 
               !(MM_U_CM$MM_ID == "2014.MMP00102" & MM_U_CM$U_ID == 3231), ]#66

# 2015
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2015.MMP00094" & MM_U_CM$U_ID == 3240) & !(MM_U_CM$MM_ID == "2015.MMP00095" & MM_U_CM$U_ID == 3240) &
                     !(MM_U_CM$MM_ID == "2015.MMP00094" & MM_U_CM$U_ID == 6269) & !(MM_U_CM$MM_ID == "2015.MMP00095" & MM_U_CM$U_ID == 6269), ]#50

# 2016
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2016.MMP00486" & MM_U_CM$U_ID == 4689), ]#49

# 2017
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2017.MMP00004" & MM_U_CM$U_ID == 5187) & !(MM_U_CM$MM_ID == "2017.MMP00009" & MM_U_CM$U_ID == 3934) &
                   !(MM_U_CM$MM_ID == "2017.MMP00024" & MM_U_CM$U_ID == 3310) & !(MM_U_CM$MM_ID == "2017.MMP00031" & MM_U_CM$U_ID == 3310), ]

# 2018
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2018.MMP00018" & MM_U_CM$U_ID == 3995) & !(MM_U_CM$MM_ID == "2018.MMP00064" & MM_U_CM$U_ID == 4021) &
                   !(MM_U_CM$MM_ID == "2018.MMP00185" & MM_U_CM$U_ID == 4063) & !(MM_U_CM$MM_ID == "2018.MMP00186" & MM_U_CM$U_ID == 4064), ]

# 2019
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2019.MMP00006" & MM_U_CM$U_ID == 4628) & !(MM_U_CM$MM_ID == "2019.MMP00011" & MM_U_CM$U_ID == 4526) &
                   !(MM_U_CM$MM_ID == "2019.MMP00018" & MM_U_CM$U_ID == 4628) & !(MM_U_CM$MM_ID == "2019.MMP00067" & MM_U_CM$U_ID == 4629), ]

# 2020
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2020.MMP00033" & MM_U_CM$U_ID == 5133) & !(MM_U_CM$MM_ID == "2020.MMP00033" & MM_U_CM$U_ID == 5128) &
                   !(MM_U_CM$MM_ID == "2020.MMP00045" & MM_U_CM$U_ID == 5133) & !(MM_U_CM$MM_ID == "2020.MMP00332" & MM_U_CM$U_ID == 5355), ]

# 2021
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2021.MMP00040" & MM_U_CM$U_ID == 5699) & !(MM_U_CM$MM_ID == "2021.MMP00082" & MM_U_CM$U_ID == 6588) &
                   !(MM_U_CM$MM_ID == "2021.MMP00089" & MM_U_CM$U_ID == 6588) & !(MM_U_CM$MM_ID == "2021.MMP00134" & MM_U_CM$U_ID == 5562) &
                   !(MM_U_CM$MM_ID == "2021.MMP00147" & MM_U_CM$U_ID == 5562) & !(MM_U_CM$MM_ID == "2021.MMP00225" & MM_U_CM$U_ID == 5885), ]

# 2022
MM_U_CM <- MM_U_CM[!(MM_U_CM$MM_ID == "2022.MMP00027" & MM_U_CM$U_ID == 6121) & !(MM_U_CM$MM_ID == "2022.MMP00038" & MM_U_CM$U_ID == 6247) &
                   !(MM_U_CM$MM_ID == "2022.MMP00038" & MM_U_CM$U_ID == 5978) & !(MM_U_CM$MM_ID == "2022.MMP00049" & MM_U_CM$U_ID == 6113), ]


#----------- Merging 4th step (Eastern Med): intersection MM & U ----------------

MM_U_EM <-fuzzyjoin::difference_left_join(MM_EM, U_EM, by = c("MM_date" = "U_date"), max_dist = 3)
#817 cases

MM_U_EM <-mutate(MM_U_EM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MM_U_EM$diff_MM_U <- abs(MM_U_EM$diff_MM_U)#transform the difference into positive numbers only

MM_U_EM <- filter(MM_U_EM, diff_MM_U <= 1 |(MM_n_dead_missing >= 8 & diff_MM_U <= 2) | (MM_n_dead_missing >= 20 & diff_MM_U <= 10) | MM_n_dead_missing >= 100 & diff_MM_U<= 65)
#217

MM_U_EM <- select(MM_U_EM, "MM_ID", "U_ID", "MM_date", "U_date",
                  "MM_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
                  "U_description","MM_location", "U_place_death", "diff_MM_U")


MM_U_EM <- MM_U_EM[!(MM_U_EM$MM_ID == "2015.MMP00346") & !(MM_U_EM$MM_ID == "2015.MMP00349") & 
                     !(MM_U_EM$MM_ID == "2015.MMP00363") & !(MM_U_EM$MM_ID == "2015.MMP00364"), ]#2015

MM_U_EM <- MM_U_EM[!(MM_U_EM$MM_ID == "2016.MMP00001" & MM_U_EM$U_ID == 5374) & !(MM_U_EM$MM_ID == "2016.MMP00005"), ]#2016 (46)


#----------- Merging 4th step (Western Med): intersection MM & U ----------------

MM_U_WM <-fuzzyjoin::difference_left_join(MM_WM, U_WM, by = c("MM_date" = "U_date"), max_dist = 3)
#35 cases

MM_U_WM <-mutate(MM_U_WM, diff_MM_U = MM_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MM_U_WM$diff_MM_U <- abs(MM_U_WM$diff_MM_U)#transform the difference into positive numbers only

MM_U_WM <- filter(MM_U_WM, diff_MM_U <= 1 |(MM_n_dead_missing >= 8 & diff_MM_U <= 2) | (MM_n_dead_missing >= 20 & diff_MM_U <= 10) | MM_n_dead_missing >= 100 & diff_MM_U<= 65)
#13

#MM_U_WM <- select(MM_U_WM, "MM_ID", "U_ID", "MM_date", "U_date",
#                  "MM_n_dead_missing", "U_n_dead_missing", "MM_n_survivors", "MM_description", 
#                  "U_description","MM_location", "U_place_death", "diff_MM_U")


#----------- Merging 5th step (Central Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and U lists only ----------------

#Central Mediterranean
U_MM_CM <- anti_join(MM_U_CM, MM_MF_U_CM, by = "U_ID")#subtraction for the U list --> this list is final
#11 cases
MM_U_CM <- anti_join(MM_U_CM, MM_MF_U_CM, by = "MM_ID")#subtraction for the MM list --> this list is final
#11 cases

#----------- Merging 5th step (Eastern Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and U lists only ----------------

U_MM_EM <- anti_join(MM_U_EM, MM_MF_U_EM, by = "U_ID")#subtraction for the U list --> this list is final
#14 cases
MM_U_EM <- anti_join(MM_U_EM, MM_MF_U_EM, by = "MM_ID")#subtraction for the MM list --> this list is final
#14 cases

#----------- Merging 5th step (Western Med): exclude matching cases from the 3 lists to keep cases that appear in the MM and U lists only ----------------

U_MM_WM <- anti_join(MM_U_WM, MM_MF_U_WM, by = "U_ID")#subtraction for the U list --> this list is final
#7 cases
MM_U_WM <- anti_join(MM_U_WM, MM_MF_U_WM, by = "MM_ID")#subtraction for the MM list --> this list is final
#7 cases

#----------- Merging 6th step (Central Med): intersection MF and U ----------------

MF_U_CM <- fuzzyjoin::difference_left_join(MF_CM, U_CM, by =  c("MF_date" = "U_date"), max_dist = 3)
#253 cases

MF_U_CM <-mutate(MF_U_CM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MF_U_CM$diff_MF_U <- abs(MF_U_CM$diff_MF_U)#transform the difference into positive numbers only

MF_U_CM <- filter(MF_U_CM, diff_MF_U <= 1 |(MF_n_dead_missing >= 8 & diff_MF_U <= 2)|(MF_n_dead_missing >= 15 & diff_MF_U <= 5) | (MF_n_dead_missing >= 20 & diff_MF_U <= 10) | MF_n_dead_missing >= 100 & diff_MF_U<= 65 | MF_n_dead_missing >= 800 & diff_MF_U<= 300)
#63


#MF_U_CM <- select(MF_U_CM, "MF_ID","MF_ID_new","U_ID",  "MF_date", "U_date",
#                       "MF_n_dead_missing", "U_n_dead_missing","MF_description","U_cause_death", 
#                    "U_description", "MF_location", "U_place_death", "diff_MF_U")

MF_U_CM <- MF_U_CM[!(MF_U_CM$MF_ID == 56549 & MF_U_CM$U_ID == 4759), ]#2014(52)

MF_U_CM <- MF_U_CM[!(MF_U_CM$MF_ID_new == 271 & MF_U_CM$U_ID == 3291) & !(MF_U_CM$MF_ID_new == 284 & MF_U_CM$U_ID == 3277), ]#2015(41)

MF_U_CM <- MF_U_CM[!(MF_U_CM$MF_ID_new == 35 & MF_U_CM$U_ID == 3740) & !(MF_U_CM$MF_ID_new == 16 & MF_U_CM$U_ID == 4689), ]#2016(42)

#----------- Merging 6th step (Eastern Med): intersection MF and U ----------------

MF_U_EM <- fuzzyjoin::difference_left_join(MF_EM, U_EM, by =  c("MF_date" = "U_date"), max_dist = 3)
#563 cases

MF_U_EM <-mutate(MF_U_EM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MF_U_EM$diff_MF_U <- abs(MF_U_EM$diff_MF_U)#transform the difference into positive numbers only

MF_U_EM <- filter(MF_U_EM, diff_MF_U <= 1 |(MF_n_dead_missing >= 8 & diff_MF_U <= 2) | (MF_n_dead_missing >= 20 & diff_MF_U <= 10) | MF_n_dead_missing >= 100 & diff_MF_U<= 65 | MF_n_dead_missing >= 800 & diff_MF_U<= 300)
#125


#MF_U_EM <- select(MF_U_EM, "MF_ID","MF_ID_new","U_ID",  "MF_date", "U_date",
#                  "MF_n_dead_missing", "U_n_dead_missing","MF_description","U_cause_death", 
#                  "U_description", "MF_location", "U_place_death", "diff_MF_U")

MF_U_EM <- MF_U_EM[!(MF_U_EM$MF_ID_new == 268 & MF_U_EM$U_ID == 3544) & !(MF_U_EM$MF_ID_new == 268 & MF_U_EM$U_ID == 3546) &
                     !(MF_U_EM$MF_ID_new == 266 & MF_U_EM$U_ID == 3547) & !(MF_U_EM$MF_ID_new == 252 & MF_U_EM$U_ID == 3847), ]#(37)

#----------- Merging 6th step (Western Med): intersection MF and U ----------------

MF_U_WM <- fuzzyjoin::difference_left_join(MF_WM, U_WM, by =  c("MF_date" = "U_date"), max_dist = 3)
#19 cases

MF_U_WM <-mutate(MF_U_WM, diff_MF_U = MF_n_dead_missing - U_n_dead_missing)#calculate difference between reported values in both lists
MF_U_WM$diff_MF_U <- abs(MF_U_WM$diff_MF_U)#transform the difference into positive numbers only

MF_U_WM <- filter(MF_U_WM, diff_MF_U <= 1 | (MF_n_dead_missing >= 8 & diff_MF_U <= 2) |(MF_n_dead_missing >= 20 & diff_MF_U <= 10) | MF_n_dead_missing >= 100 & diff_MF_U<= 65 | MF_n_dead_missing >= 800 & diff_MF_U<= 300)
#7 cases


#MF_U_WM <- select(MF_U_WM, "MF_ID","MF_ID_new","U_ID",  "MF_date", "U_date",
#                  "MF_n_dead_missing", "U_n_dead_missing","MF_description","U_cause_death",
#                  "U_description", "MF_location", "U_place_death", "diff_MF_U")


#----------- Merging 7th step (Central Med): exclude matching cases from the 3 lists to keep cases that appear in the MF and U lists only ----------------

U_MF_CM <-  anti_join(MF_U_CM, MM_MF_U_CM, by = "U_ID")#subtraction for the U list --> this list is final
#5 case

MF_U_CM <- anti_join(MF_U_CM, MM_MF_U_CM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#5 case

#----------- Merging 7th step (Eastern Med): exclude matching cases from the 3 lists to keep cases that appear in the MF and U lists only ----------------

U_MF_EM <-  anti_join(MF_U_EM, MM_MF_U_EM, by = "U_ID")#subtraction for the U list --> this list is final
#6 case

MF_U_EM <- anti_join(MF_U_EM, MM_MF_U_EM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#6 case

#----------- Merging 7th step (Western Med): exclude matching cases from the 3 lists to keep cases that appear in the MF and U lists only ----------------

U_MF_WM <-  anti_join(MF_U_WM, MM_MF_U_WM, by = "U_ID")#subtraction for the U list --> this list is final
#0 case

MF_U_WM <- anti_join(MF_U_WM, MM_MF_U_WM, by = "MF_ID_new")#subtraction for the MF list --> this list is final
#0 case

#----------- Merging 8th step (Central Med): create the list for cases that appear at MM list only ----------------

MM_anti_CM <- anti_join(MM_CM, MM_MF_CM, by = "MM_ID")#subtract the intersection (MM_MF) from the complete MM list
#97

MM_anti_CM <- anti_join(MM_anti_CM, MM_U_CM, by = "MM_ID")
#87 cases

MM_anti_CM <- anti_join(MM_anti_CM, MM_MF_U_CM, by = "MM_ID")
#44 cases

#----------- Merging 8th step (Eastern Med): create the list for cases that appear at MM list only ----------------

MM_anti_EM <- anti_join(MM_EM, MM_MF_EM, by = "MM_ID")#subtract the intersection (MM_MF) from the complete MM list
#114

MM_anti_EM <- anti_join(MM_anti_EM, MM_U_EM, by = "MM_ID")
#103 cases

MM_anti_EM <- anti_join(MM_anti_EM, MM_MF_U_EM, by = "MM_ID")
#67 cases

#----------- Merging 8th step (Western Med): create the list for cases that appear at MM list only ----------------

MM_anti_WM <- anti_join(MM_WM, MM_MF_WM, by = "MM_ID")#subtract the intersection (MM_MF) from the complete MM list
#31

MM_anti_WM <- anti_join(MM_anti_WM, MM_U_WM, by = "MM_ID")
#24 cases

MM_anti_WM <- anti_join(MM_anti_WM, MM_MF_U_WM, by = "MM_ID")
#17 cases

#----------- Merging 9th step (Central Med): create the list for cases that appear at MF list only ----------------

MF_anti_CM <- anti_join(MF_CM, MF_MM_CM, by = "MF_ID_new")#subtract the intersection (MF_MM) from the complete MF list
#75 cases

MF_anti_CM <- anti_join(MF_anti_CM, MF_U_CM, by = "MF_ID_new")
#70 cases

MF_anti_CM <- anti_join(MF_anti_CM, MM_MF_U_CM, by = "MF_ID_new")
#27

#----------- Merging 9th step (Eastern Med): create the list for cases that appear at MF list only ----------------

MF_anti_EM <- anti_join(MF_EM, MF_MM_EM, by = "MF_ID_new")#subtract the intersection (MF_MM) from the complete MF list
#85 cases

MF_anti_EM <- anti_join(MF_anti_EM, MF_U_EM, by = "MF_ID_new")
#80 cases

MF_anti_EM <- anti_join(MF_anti_EM, MM_MF_U_EM, by = "MF_ID_new")
#44

#----------- Merging 9th step (Western Med): create the list for cases that appear at MF list only ----------------

MF_anti_WM <- anti_join(MF_WM, MF_MM_WM, by = "MF_ID_new")#subtract the intersection (MF_MM) from the complete MF list
#15 cases

MF_anti_WM <- anti_join(MF_anti_WM, MF_U_WM, by = "MF_ID_new")
#15 cases

MF_anti_WM <- anti_join(MF_anti_WM, MM_MF_U_WM, by = "MF_ID_new")
#8

#----------- Merging 10th step (Central Med): create the list for cases that appear at U list only ----------------

U_anti_CM <- anti_join(U_CM, U_MM_CM, by = "U_ID")
#78 cases

U_anti_CM <- anti_join(U_anti_CM, U_MF_CM, by = "U_ID")
#73 cases

U_anti_CM <- anti_join(U_anti_CM, MM_MF_U_CM, by = "U_ID")
#31 cases

#----------- Merging 10th step (Eastern Med): create the list for cases that appear at U list only ----------------

U_anti_EM <- anti_join(U_EM, U_MM_EM, by = "U_ID")
#117 cases

U_anti_EM <- anti_join(U_anti_EM, U_MF_EM, by = "U_ID")
#113 cases

U_anti_EM <- anti_join(U_anti_EM, MM_MF_U_EM, by = "U_ID")
#79 cases

#----------- Merging 10th step (Western Med): create the list for cases that appear at U list only ----------------
#Western Mediterranean
U_anti_WM <- anti_join(U_WM, U_MM_WM, by = "U_ID")
#9 cases

U_anti_WM <- anti_join(U_anti_WM, U_MF_WM, by = "U_ID")
#9 cases

U_anti_WM <- anti_join(U_anti_WM, MM_MF_U_WM, by = "U_ID")
#2 cases


#------------------------- Final dataset ---------------------------------------

data_final <- dplyr::bind_rows(MM_anti_CM, MF_anti_CM, U_anti_CM, MM_MF_U_CM, MM_U_CM, MF_U_CM, MM_MF_CM, MM_anti_EM, MF_anti_EM, U_anti_EM, MM_MF_U_EM, MM_U_EM, MF_U_EM, MM_MF_EM, MM_anti_WM, MF_anti_WM, U_anti_WM, MM_MF_U_WM, MM_U_WM, MF_U_WM, MM_MF_WM)

data_final$deaths_min <- with(data_final,pmin(MM_n_dead_missing, MF_n_dead_missing, U_n_dead_missing, na.rm = TRUE))
data_final$deaths_max <- with(data_final,pmax(MM_n_dead_missing, MF_n_dead_missing, U_n_dead_missing, na.rm = TRUE))
data_final$deaths_mean <- with(data_final,rowMeans(with(data_final, cbind(MM_n_dead_missing, MF_n_dead_missing, U_n_dead_missing)), na.rm = TRUE))

#use data_final and duplicate the column MM_coordinates

data_final$MM_coordinates_2 <- data_final$MM_coordinates

#use data_final column MM_coordinates and remove "POINT (" of all columns

data_final$MM_coordinates_2 <- gsub("POINT \\(", "", data_final$MM_coordinates_2)

#use data_final column MM_coordinates and remove ")" of all columns

data_final$MM_coordinates_2 <- gsub("\\)", "", data_final$MM_coordinates_2)


data_final <- separate(data_final, col = MM_coordinates_2, into = c("MM_longitude", "MM_latitude"),
                       sep = " ", remove = FALSE, convert = TRUE)

# Use the sub() function to replace the comma at the end of the strings in the U_latitude and U_longitude columns with an empty string
data_final$U_latitude <- sub(",+$", "", data_final$U_latitude)
data_final$U_longitude <- sub(",+$", "", data_final$U_longitude)

data_final$U_longitude <- as.numeric(data_final$U_longitude)
data_final$U_latitude <- as.numeric(data_final$U_latitude)

#------------------------- Export Final dataset --------------------------------

write.xlsx(data_final, '////')










