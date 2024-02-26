## Script for Master Thesis
# By Ana Elisa von Borstel

#----------------- Packages ----------------------------------------------------
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(stargazer) 
library(gtsummary) 
library(table1) 
library(descr) 
library(dplyr) 
library(tidyr) 
library(sjPlot)
library(plm)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
options(scipen=999)
library(stargazer)
library(fixest)
library(coefplot)

#-------------------- Data -----------------------------------------------------

#Frontex dataset filtered by Central and Western Mediterranean routes
detection_data <- read_excel("detection_data.xlsx")

#Conflict data 
conflict_death <- read_excel("GEDEvent_v22_1-2.xlsx")

#ISO list
ISO <- read_excel("ISO_complete.xlsx")

#GDP per capita
GDP_per_capita <- read_excel("GDP_per_capita.xls")

#Unemployment
unemployment <- read_excel("unemployment.xls")

#Population size
population <- read_excel("population.xls")

#Political variables
political <- read_csv("qog_std_ts_jan23.csv")

#2014 arrival gender
data_2014 <- read_excel("data_2014.xlsx")

#2015 arrival gender
data_2015 <- read_excel("data_2015.xlsx")

#2016 arrival gender 
data_2016 <- read_excel("data_2016.xlsx")

#2017 arrival gender
data_2017 <- data_2017 <- read_excel("data_2017.xlsx")

#2018 arrival gender
data_2018 <- read_excel("data_2018.xlsx")

# -------------------- Data transformation -------------------------------------

#detection data
detection_data <- pivot_longer(data = detection_data, cols = 4:135, #change the dataset to longer format 
                               names_to = c("month", "year"), #create columns month and year
                               names_pattern = "([A-Za-z]+)(\\d+)",
                               values_to = "arrival") #add values to column arrival

detection_data <- rename(detection_data, country= nationality)

detection_data  <-  mutate(detection_data , month_number = case_when(month == "JAN" ~ 1, 
                                                                     month == "FEB" ~ 2,
                                                                     month == "MAR" ~ 3,
                                                                     month == "APR" ~ 4,
                                                                     month == "MAY" ~ 5,
                                                                     month == "JUN" ~ 6,
                                                                     month == "JUL" ~ 7,
                                                                     month == "AUG" ~ 8,
                                                                     month == "SEP" ~ 9,
                                                                     month == "OCT" ~ 10,
                                                                     month == "NOV" ~ 11,
                                                                     month == "DEC" ~ 12))

detection_data$year  <- as.numeric(detection_data$year)

detection_data$date <- paste(detection_data$year, detection_data$month_number, sep="-") %>% ym() %>% as.Date()

#Conflict data
conflict_death <- filter(conflict_death, year >= 2011 & year <= 2018) #filter dataset to period of analysis

conflict_death$date_conflict <- format(as.Date(conflict_death$date_start), "%Y-%m-%d")

conflict_death$month_number <- format(as.Date(conflict_death$date_start), "%m")

conflict_death$month_number  <- as.numeric(conflict_death$month_number)

conflict_death <- select(conflict_death, year, type_of_violence, country, region, deaths_civilians, date_conflict,month_number, latitude, longitude) #keep only columns relevant for the analysis

conflict_death <- conflict_death %>% group_by(country,month_number, year) %>%
  mutate(total_death_month = sum(deaths_civilians)) #calculate number of civilian deaths per month per country

conflict_death$date_conflict <- as.Date(conflict_death$date_conflict, "%Y-%m-%d")

#harmonize names of countries
conflict_death$country[conflict_death$country == "DR Congo (Zaire)"] <- "DR Congo"
conflict_death$country[conflict_death$country == "Madagascar (Malagasy)"] <- "Madagascar"
conflict_death$country[conflict_death$country == "Yemen (North Yemen)"] <- "Yemen"
conflict_death$country[conflict_death$country == "Zimbabwe (Rhodesia)"] <- "Zimbabwe"

conflict_death$conflict <- 1 #create dummy variable for countries that experienced conflict

#GDP per capita 
GDP_per_capita <- pivot_longer(data = GDP_per_capita, cols = 2:63, #transform dataset to longer format
                               names_to = "year",
                               values_to = "GDP_per_capita")

GDP_per_capita$year  <- as.numeric(GDP_per_capita$year)

#Unemployment
unemployment <- pivot_longer(data = unemployment, cols = 2:63, #transform dataset to longer format
                             names_to = "year",
                             values_to = "unemployment")

unemployment$year  <- as.numeric(unemployment$year)

#Population size
population <- pivot_longer(data = population, cols = 2:63, #transform dataset to longer format
                           names_to = "year",
                           values_to = "population")

population$year  <- as.numeric(population$year)

population <- rename(population, ISO = `Country Code`)

#ISO
ISO <- select(ISO, c("country", "ISO", "region", "sub-region"))

#political factors 

political <- filter(political, year >= 2011 & year <= 2018) #filter dataset to period of analysis

political <- rename(political, ISO = ccodealp)

political$ISO[political$ISO == "SSD"] <- "SDS"

#--------------------- Merging datasets ----------------------------------------

data <- left_join(detection_data, conflict_death, by = c("year", "month_number", "country"))

data <- left_join(data, ISO, by = "country")

data <- left_join(data, GDP_per_capita, by = c("ISO", "year"))

data <- left_join(data, unemployment, by = c("ISO", "year"))

data <- left_join(data, population, by = c("ISO", "year"))

data <- left_join(data, political, by = c("ISO", "year"))

# Create new variables and remove duplicates

data$deaths_civilians[is.na(data$deaths_civilians)] <- 0 #dummy variable for civilian deaths per month per country

data$conflict[is.na(data$conflict)] <- 0 #dummy variable for civilian deaths per month per country

#remove duplicates (when conflict happened twice in a month)

data <- data %>% distinct(country,month,year,arrival, route, .keep_all = TRUE)

data$total_death_month[is.na(data$total_death_month)] <- 0

data$type_of_violence[is.na(data$type_of_violence)] <- 0

#---------------- Visualization - Figure 1 -------------------------------------

data %>% group_by(route,year, month_number) %>%
  summarise(total = sum(arrival))%>%
  ggplot(aes(x= as.factor(month_number), y = total)) +
  geom_col(fill="grey") +
  facet_wrap(~route+year)+
  ggtitle("Figure 1: Monthly arrivals via Central and Eastern Mediterranean Routes from 2011 to 2018") +
  ylab("number arrivals")+
  xlab("month") +
  theme_classic()

#---------------- Data and Analytical Sample -----------------------------------

#restrict analysis to 2011-2018
data <- filter(data, year >= 2011 & year <= 2018)

#exclude countries not relevant to the analysis: in America, Europe, Oceania, Asia etc
data <- data %>% filter(country != "Unknown") %>%
  filter(country != "Albania") %>%
  filter(country != "Belarus") %>%
  filter(country != "Colombia") %>%
  filter(country != "French Guiana") %>%
  filter(country != "Kosovo") %>%
  filter(country != "Moldova") %>%
  filter(country != "Panama") %>%
  filter(country != "Russia") %>%
  filter(country != "Stateless") %>%
  filter(country != "Ukraine") %>%
  filter(country != "China")%>%
  filter(country != "Haiti")%>%
  filter(country != "Kyrgyzstan")%>%
  filter(country != "Malaysia")%>%
  filter(country != "Myanmar")%>%
  filter(country != "Tajikistan")%>%
  filter(country != "Uzbekistan")%>%
  filter(country != "Vietnam") %>%
  filter(country != "Belize")%>%
  filter(country != "Bermuda")%>%
  filter(country != "Sub-Sahara")%>% ### not sure what to do with this
  filter(country != "Kazakhstan")%>%
  filter(country != "Jamaica")

#exclude more countries based on sub-region

data <- rename(data,sub_region = `sub-region`)

data <- data %>% filter(!sub_region %in% c("Latin America and the Caribbean", "Southern Europe", 
                                           "South-eastern Asia", "Eastern Asia"))

data_c <- data %>% filter (route == "Central Mediterranean Route") 

# ----------------- Visualization - Figure 2 -----------------------------------

#Figure 2

data_2 <- data_c %>% filter(arrival > 1) %>% group_by(country, year) %>% summarise(total_arrival = sum(arrival)) %>% 
  filter(country %in% c("Syria", "Nigeria", "Eritrea", "Tunisia"))

data_label <- data_2 %>% group_by(country) %>% filter(country %in% c("Syria", "Nigeria", "Eritrea", "Tunisia")) %>%
  group_by(country) %>% 
  arrange(-total_arrival) %>% 
  top_n(1)

data_c %>% filter(arrival > 1) %>% group_by(country, year) %>% summarise(total_arrival = sum(arrival)) %>% 
  ggplot(aes(x = year, y = total_arrival, group = country)) +
  scale_x_continuous(breaks = seq(2011, 2018, 1)) +
  geom_line(color= "grey")+
  geom_line(data = data_2, aes(x = year, y = total_arrival, color = country), size = 1) +
  geom_label(data = data_label,
             aes(x = year + 0.03, label=country, color = country), hjust=0)+
  theme(
    legend.position = "none")+
  ylab("Number of arrivals")+
  xlab("Year")+
  ggtitle("Main countries of nationality of arrivals in Italy")

#---------------- Visualization - Figure 3 ------------------------------------
#Figure 3
Fig_2 <- data_c %>% group_by(year, ISO) %>%
  summarise(total = sum(arrival))

Fig_2 <- Fig_2 %>% mutate(dummy = case_when(total >= 1 ~ 1,
                                            total == 0 ~ 0))


Fig_2 %>% group_by(dummy, year) %>% summarise(n = n()) %>%
  ggplot(aes(x = as.factor(year), y = n, fill = as.factor(dummy)))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))+
  scale_fill_manual(name = "", labels = c("No", "Yes"),
                    values = c("lightgrey", "darkgrey"))+
  ggtitle("Figure 3: Countries in the sample producing migrants crossing the Central Mediterranean by year")+
  ylab("Number of countries")+
  xlab("")+
  theme_bw()

# -------------- Visualization - Figure 4 --------------------------------------

data_c %>% select(year, month_number, type_of_violence, total_death_month, ISO) %>%
  filter(type_of_violence != "0") %>% group_by(ISO, year) %>%
  mutate(total_death_year = sum(total_death_month)) %>% 
  ggplot(aes(x = as.factor(year), y = total_death_year)) +
  geom_boxplot()+
  geom_text(aes(label = ISO), data = data_c %>% filter(type_of_violence != "0") %>% group_by(ISO, year) %>%
              mutate(total_death_year = sum(total_death_month)) %>% filter(total_death_year > 1000), vjust= -1)+
  ylab("Number of civilian deaths")+
  xlab("Year")+
  ggtitle("Civilian deaths in country of origin of arrivals in Italy")+
  theme_classic()

#---------------- Visualization - Figure 5 -------------------------------------

data_c %>% group_by(type_of_violence, year) %>% summarise(n = n()) %>% 
  ggplot(aes(x = as.factor(year), y = n, fill = as.factor(type_of_violence)))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Type of violence", labels = c("No conflict", "State", "Non-state", "Mixed"),
                    values = c("lightgrey", "coral2", "coral3", "coral4"))+
  ggtitle("Number of country-month by type of conflict")+
  ylab("Number of country-month")+
  xlab("Year")+
  theme_bw()

#--------------- Summary statistics --------------------------------------------

data_c %>% group_by(ISO, year) %>% mutate(total_arrival = sum(arrival)) %>% filter(month_number ==1) %>%
  select(ISO, total_arrival, GDP_per_capita, unemployment,fh_pr, gd_ptss,  vdem_gender,   wdi_empf,population,)%>%
  as.data.frame() %>%
  stargazer(type = "text", #or html or latex 
            out = "html_table.html",
            digits=2,
            summary.stat = c("n", "min", "max", "median", "mean", "sd"))

#--------------- Visualization - Figure 6, map ---------------------------------

#map
Fig_2 <- data_c %>% group_by(year, ISO) %>%
  summarise(total = sum(arrival))

Fig_2$year <- as.factor(Fig_2$year)

Fig_2 <- rename(Fig_2, adm0_a3 = ISO)

conflict_death <- left_join(conflict_death, ISO, by = "country")


Fig_conflict <- conflict_death %>% filter(region.x == "Asia" | region.x == "Africa" | region.x == "Middle East" )

Conflict <- Fig_conflict %>% group_by(year, country) %>% summarise(n())

world <- ne_countries(scale = "medium", returnclass = "sf")

map <- 
  ne_countries(scale = "medium", returnclass = "sf")  %>% 
  left_join(Fig_2 ,
            by = "adm0_a3") %>% 
  filter(year == "2011" | year == "2012" |  year == "2013" | year == "2014" |year == "2015" | year == "2016" |year == "2017" | year == "2018")

fig_map <- ggplot(data = map) +
  geom_sf(aes(fill = total)) +
  geom_point(data = Fig_conflict, aes(x = longitude, y = latitude), size = Fig_conflict$deaths_civilians/70, color = "darkred", alpha = 0.8)+
  coord_sf(xlim = c(-25, 105), ylim = c(50, -40), expand = FALSE)+
  scale_fill_gradient(low = "white", high = "black", na.value = "grey80", trans = "sqrt",
                      labels = scales::number_format(accuracy = 1)) +
  facet_wrap(~year)+
  theme(legend.position = "bottom",
        legend.box = "horizontal")+
  xlab("") + ylab("") +
  labs(fill = "Arrivals in Italy")+
  ggtitle("Figure 6: Number of arrivals in Italy by country of origin and conflict incidence")+
  theme_void()

fig_map

# -------------- Regression Results --------------------------------------------

# Model for the whole period 2011 to 2018 (no gender effect)
data_c <- filter(data_c, year >= 2011)
data_c$vdem_gender <- data_c$vdem_gender*10
data_c$type_of_violence <- as.factor(data_c$type_of_violence)
data_c <- data_c %>%
  group_by(ISO) %>%
  mutate(cumulative_arrival = cumsum(arrival))

model03b <- plm(log(arrival+1) ~ lag(type_of_violence,12) + lag(total_death_month, 12)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population)+as.factor(year) + as.factor(month_number) , data = data_c, model= "within", index = "ISO")

coefplot(model03b)

summary(model03b)

#prepare datasets for restricted analysis + gender analysis

#2014 arrival data

data_2014 <- rename(data_2014, male_total = Arrivals_male)

data_2014 <- rename(data_2014, female_total = Arrivals_female)

data_2014 <- mutate(data_2014 , month_number = case_when(month == "january" ~ 1, 
                                                         month == "february" ~ 2,
                                                         month == "march" ~ 3,
                                                         month == "april" ~ 4,
                                                         month == "may" ~ 5,
                                                         month == "june" ~ 6,
                                                         month == "july" ~ 7,
                                                         month == "august" ~ 8,
                                                         month == "september" ~ 9,
                                                         month == "october" ~ 10,
                                                         month == "november" ~ 11,
                                                         month == "december" ~ 12))

data_2014 <- select(data_2014, year, ISO, male_total, female_total, month_number)

#2015 arrival data

data_2015$Reported_date <- format(as.Date(data_2015$Reported_date), "%m") #add number of month

data_2015 <- mutate(data_2015, year= 2015) #create column year

data_2015 <- rename(data_2015, month_number = Reported_date) #rename month column

data_2015$month_number <- as.numeric(data_2015$month_number)

data_2015<-data_2015[order(data_2015$ISO, data_2015$month_number),] #order data to calculate arrivals per month

data_2015<-data_2015 %>%
  group_by(ISO) %>% mutate(male_total = c(NA, diff(Arrivals_cumulative_male)))

data_2015 <-data_2015 %>% 
  mutate(male_total = ifelse(month_number == 1, Arrivals_cumulative_male, male_total))

data_2015<-data_2015 %>%
  group_by(ISO) %>% mutate(female_total = c(NA, diff(Arrivals_cumulative_female)))

data_2015 <-data_2015 %>% 
  mutate(female_total = ifelse(month_number == 1, Arrivals_cumulative_female, female_total))

#2016 arrival data

data_2016$Reported_date <- format(as.Date(data_2016$Reported_date), "%m") #add number of month

data_2016 <- mutate(data_2016, year= 2016) #create column year

data_2016 <- rename(data_2016, month_number = Reported_date) #rename month column

data_2016 <- rename(data_2016, ISO = ISO_3_origin) #rename ISO column

data_2016<-data_2016[order(data_2016$ISO, data_2016$month_number),] #order data to calculate arrivals per month

data_2016<-data_2016 %>%
  group_by(ISO) %>% mutate(male_total = c(NA, diff(Arrivals_cumulative_male)))

data_2016 <-data_2016 %>% 
  mutate(male_total = ifelse(month_number == 1,  male_total, Arrivals_cumulative_male))

data_2016<-data_2016 %>%
  group_by(ISO) %>% mutate(female_total = c(NA, diff(Arrivals_cumulative_female)))

data_2016 <-data_2016 %>% 
  mutate(female_total = ifelse(month_number == 1,  female_total, Arrivals_cumulative_female))

#add 2017 data

data_2017$Reported_date <- format(as.Date(data_2017$Reported_date), "%m") #add number of month

data_2017 <- mutate(data_2017, year= 2017) #create column year

data_2017 <- rename(data_2017, month_number = Reported_date) #rename month column

data_2017<-data_2017[order(data_2017$ISO, data_2017$month_number),] #order data to calculate arrivals per month

data_2017<-data_2017 %>%
  group_by(ISO) %>% mutate(male_total = c(NA, diff(Arrivals_cumulative_male)))

data_2017 <-data_2017 %>% 
  mutate(male_total = ifelse(month_number == 1, male_total,Arrivals_cumulative_male))

data_2017<-data_2017 %>%
  group_by(ISO) %>% mutate(female_total = c(NA, diff(Arrivals_cumulative_female)))

data_2017 <-data_2017 %>% 
  mutate(female_total = ifelse(month_number == 1,  female_total, Arrivals_cumulative_female))

data_2017 <- rename(data_2017, Arrivals_cumulative = Cumulative_arrivals)

#add 2018 data

data_2018 <- mutate(data_2018, year= 2018) #create column year

data_2018<-data_2018 %>%
  group_by(ISO) %>% mutate(male_total = c(NA, diff(Arrivals_cumulative_male)))

data_2018 <-data_2018 %>% 
  mutate(male_total = ifelse(month_number == 1, Arrivals_cumulative_male, male_total))

data_2018<-data_2018 %>%
  group_by(ISO) %>% mutate(female_total = c(NA, diff(Arrivals_cumulative_female)))

data_2018 <-data_2018 %>% 
  mutate(female_total = ifelse(month_number == 1,   Arrivals_cumulative_female, female_total))

data_2015$month_number <- as.numeric(data_2015$month_number)

data_2016$month_number <- as.numeric(data_2016$month_number)

data_2017$month_number <- as.numeric(data_2017$month_number)

data_2015 <- select(data_2015, month_number,ISO,Arrivals_cumulative,Arrivals_cumulative_male, Arrivals_cumulative_female, year, male_total, female_total)

data_2016 <- select(data_2016, month_number,ISO,Arrivals_cumulative,Arrivals_cumulative_male, Arrivals_cumulative_female, year, male_total, female_total)

data_2017 <- select(data_2017, month_number,ISO,Arrivals_cumulative,Arrivals_cumulative_male, Arrivals_cumulative_female, year, male_total, female_total)

data_2018 <- select(data_2018, month_number,ISO,Arrivals_cumulative,Arrivals_cumulative_male, Arrivals_cumulative_female, year, male_total, female_total)

data_gender <- bind_rows(data_2014, data_2015, data_2016, data_2017, data_2018)

#model restricted to 2014 to 2018

data_c_2 <- filter(data_c, year >= 2014 & date <= "2018-04-01")

data_c_2 <- left_join(data_c_2, data_gender, by = c("ISO", "year", "month_number"))

data_c_2 <- mutate(data_c_2, male_total = ifelse(arrival == 0, 0, male_total))

data_c_2 <- mutate(data_c_2, female_total = ifelse(arrival == 0, 0, female_total))

data_c_2$male_total <- abs(data_c_2$male_total)
data_c_2$female_total <- abs(data_c_2$female_total)

data_c_2 <- data_c_2 %>%
  group_by(ISO) %>%
  mutate(cumulative_arrival_male = cumsum(male_total))

data_c_2 <- data_c_2 %>%
  group_by(ISO) %>%
  mutate(cumulative_arrival_female = cumsum(female_total))

data_c_2 <- filter(data_c_2, !is.na(male_total))
data_c_2 <- filter(data_c_2, !is.na(female_total))

model04b <- plm(log(arrival+1) ~lag(type_of_violence,8) + lag(total_death_month,8)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population)+ as.factor(year) + as.factor(month_number) , data = data_c_2, model= "within", index = "ISO")

summary(model04b)

#Regression for female model

model_female_b <- plm(log(female_total+1) ~ lag(type_of_violence,10) + lag(total_death_month, 10)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population) + as.factor(year) + as.factor(month_number), data = data_c_2, model= "within", index = "ISO")

model_female_c <- plm(log(female_total+1) ~ lag(type_of_violence,11) + lag(total_death_month, 11)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population) + as.factor(year) + as.factor(month_number), data = data_c_2, model= "within", index = "ISO")

summary(model_female_b)

#Regression for male model

model_male_b <- plm(log(male_total+1) ~ lag(type_of_violence,4) + lag(total_death_month, 4)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population)+ as.factor(year) + as.factor(month_number), data = data_c_2, model= "within", index = "ISO")

model_male_c <- plm(log(male_total+1) ~ lag(type_of_violence,11) + lag(total_death_month, 11)+ unemployment +log(GDP_per_capita) + fh_pr+ gd_ptss+  wdi_empf+ vdem_gender + lag(cumulative_arrival) +log(population)+ as.factor(year) + as.factor(month_number), data = data_c_2, model= "within", index = "ISO")

summary(model_male_b)

stargazer(model03b, model04b, model_female_b, model_male_b, type= "text", 
          covariate.labels = c( "State-base conflict", "Non-state conflict",  
                                "One-side conflict", "Civilian casualties", 
                                "Unemployment", "(Log) GDP per Capita",  
                                "Political rights","Terror scale","Women employment rate", 
                                "Gender empowerment", "Network effect", "(Log) Population",  
                                "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
                                "February", "March","April", "May", "June", "July", 
                                "August", "September", "October", "November", 
                                "December"), title = "Table 2: Regression results of Models 1, 2, 3 and 4", 
          column.labels = c("Model 2011-2018", "Model 2014-2018", "Model Female", "Model Male"), 
          align=TRUE, dep.var.labels = c("Arrivals in Italy","", "", ""))



