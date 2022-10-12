library(MASS) #use to conduct negative binomial analysis
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(pscl)
library(lmtest)

library(sandwich)
library(tibble)

########## Data Pre-processing ########## 
##prepare three datasets
daily <- read.csv("WHO-COVID-19-global-data.csv")
vaccine <- read.csv("vaccination-data.csv")
test <- read.csv("owid-covid-data.csv")

##process "daily" dataset
#descriptive Analysis
daily <- daily %>% mutate(Date_reported = ymd(Date_reported)) #change Date_reported attribute to date data
daily <- daily %>% mutate_if(is.character, as.factor) #change qualitative data into factors
str(daily)
sapply(daily, function(x) sum(is.na(x))) #examine numbers of missing values in each column

#in order to combine three datasets, filter only same country name as those in "vaccine" data
same_country <- unique(vaccine$COUNTRY)[(unique(vaccine$COUNTRY) %in% unique(daily$Country))]
daily_need <- daily %>%
  filter(Country %in% same_country) %>%
  select(c(Date_reported,Country, WHO_region,New_cases,New_deaths)) %>%
  mutate(year = year(Date_reported), 
         month = month(Date_reported), 
         mday = mday(Date_reported)) %>%
  filter(year == '2022') 

#create a column for the number of days in Feburary
feb_day_count <- daily_need %>%
  group_by(Country,month) %>%
  summarize(count_mday = n())

#filter date from 2022/1/1 to the day created this project 2022/2/9
daily_need <- daily_need %>%
  filter(Date_reported >= '2022-01-01' & Date_reported <= '2022-02-09')

#obtain clean and targeted data in "daily" dataset
daily_final <- daily_need %>%
  group_by(Country) %>%
  summarise(total_new_cases = sum(New_cases),
            total_new_deaths = sum(New_deaths))

#------------------------------------------------------------

##process "vaccine" dataset
#descriptive analysis
str(vaccine)

# obtain every vaccine types
vac_use_vec <- sapply(vaccine$VACCINES_USED, paste) #concatenate every row values in VACCINES_USED column
vac_use_list <- str_split(vac_use_vec,",") #split the whole string by "," and make it become a list
vac_use_vec <- unlist(vac_use_list) #turn it back to a character vector
vac_use_vec <- sapply(vac_use_vec,str_trim) #remove all white spaces
vaccine_type <- stri_unique(vac_use_vec) #find unique vaccines type

#create a table contains binary values of whether a country have used each type of vaccines
vaccine_type_binary <- data.frame(matrix(ncol = length(vaccine_type), nrow = nrow(vaccine)))
for (i in 1:length(vaccine_type)) {
  vaccine_type_binary[,i] <- as.integer(grepl(vaccine_type[i],vaccine$VACCINES_USED))
}
vaccine_type_name <- vaccine_type #change the columns name as same as vaccine_type
vaccine_type_name <- str_replace_all(vaccine_type_name," - ","_")
vaccine_type_name <- str_replace_all(vaccine_type_name," ","_")
vaccine_type_name <- str_replace_all(vaccine_type_name,"-","_")
colnames(vaccine_type_binary) <- vaccine_type_name
vaccine_type_binary <- vaccine_type_binary %>%
  #remove one unnecessary row
  select(-c(12)) %>%
  #organize types of vaccines by which method a vaccine was made 
  mutate(inactivated_virus =  
           Beijing_CNBG_BBIBP_CorV + 
           Sinovac_CoronaVac + 
           Bharat_Covaxin +
           IMB_Covidful +
           Wuhan_CNBG_Inactivated + 
           Shifa_COVIran_Barakat + 
           RIBSP_QazVac + 
           Julphar_Hayat_Vax + 
           Turkovac,
         viral_vector = 
           Janssen_Ad26.COV_2_S + 
           SII_Covishield + 
           AstraZeneca_Vaxzevria + 
           Gamaleya_Gam_Covid_Vac + 
           CanSino_Convidecia + 
           Gamaleya_Sputnik_Light + 
           Gamaleya_Sputnik_V + 
           AstraZeneca_AZD1222 + 
           Shenzhen_LV_SMENP_DC,
         mRNA = 
           Pfizer_BioNTech_Comirnaty + 
           Moderna_Spikevax + 
           Moderna_mRNA_1273,
         subunit = 
           Novavax_Covavax + 
           Anhui_ZL_Recombinant + 
           CIGB_CIGB_66 + 
           Finlay_Soberana_Plus + 
           Finlay_Soberana_02 + 
           SRCVB_EpiVacCorona,
         DNA = Zydus_ZyCov_D) %>%
  #if the country has use as least one of that type, code it 1, else 0
  mutate(inactivated_virus = ifelse(inactivated_virus != 0, 1, 0),
         viral_vector = ifelse(viral_vector != 0, 1, 0),
         mRNA = ifelse(mRNA != 0, 1, 0),
         subunit = ifelse(subunit != 0, 1, 0),
         DNA = ifelse(subunit != 0, 1, 0)) %>%
  mutate(vaccine_type_used = inactivated_virus + viral_vector + mRNA + subunit + DNA)

#joining back with "vaccine" data
vaccine <- cbind(vaccine, vaccine_type_binary)
vaccine <- vaccine %>% 
  mutate(
    #change DATE_UPDATED column to date data
    DATE_UPDATED = ymd(DATE_UPDATED), 
    FIRST_VACCINE_DATE = ymd(FIRST_VACCINE_DATE)) %>%
  #change qualitative data into factors
  mutate_if(is.character, as.factor) %>%
  #change binary columns and NUMBER_VACCINES_TYPES_USED into factor
  mutate_at(tail(colnames(vaccine), n = 29), factor)

#obtain clean and targeted data in "vaccine" dataset
vaccine_final <- vaccine %>% select(! c('DATA_SOURCE', 'TOTAL_VACCINATIONS','PERSONS_VACCINATED_1PLUS_DOSE', 'TOTAL_VACCINATIONS_PER100', 'PERSONS_VACCINATED_1PLUS_DOSE','PERSONS_FULLY_VACCINATED'))

#------------------------------------------------------------

##process "test" dataset
#descriptive analysis
str(test)
test <- test %>% mutate(iso_code = replace(iso_code, iso_code == "OWID_KOS", "XKX")) #convert one wrong iso_code to a correct one
same_iso <- unique(vaccine_final$ISO3)[(unique(vaccine_final$ISO3) %in% unique(test$iso_code))] #find the same iso code in both "test" data and "vaccine_final"

#select target columns and rows
test_need <- test %>% 
  select(c("iso_code","location", "date", "stringency_index", "population")) %>%
  filter(iso_code %in% same_iso)
str(test_need)

#change data type
test_need <- test_need %>% 
  mutate(date = ymd(date)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(year = year(date), 
         month = month(date)) %>%
  filter(year == '2022') 

#create a column for the number of days in Feburary
feb_day_count_test <- test_need %>%
  group_by(iso_code,month) %>%
  summarize(count_mday = n())

#filter date from 2022/1/1 to the day created this project 2022/2/9
test_need <- test_need %>% 
  filter(date >= "2022-01-01" & date <= "2022-02-09")

#remove na value
test_need <- na.omit(test_need)

#obtain clean and targeted data in "test" dataset
test_final <-  test_need %>%
  group_by(iso_code) %>%
  summarise(stringency_index = mean(stringency_index),
            population = max(population))

#------------------------------------------------------------

##Merge three datasets to a whole
#merge by countries and their iso codes
vaccine_and_daily <- inner_join(vaccine_final,daily_final, by = c("COUNTRY" = "Country"))
covid <- inner_join(vaccine_and_daily, test_final, by = c("ISO3" = "iso_code"))

#remove rows with missing values
sapply(covid, function(x) sum(is.na(x)))
covid <- na.omit(covid)

#create three addition columns
covid <- covid %>%
  mutate(total_new_cases_per_million = round((total_new_cases/population)*1000000),
         total_new_deaths_per_million = round((total_new_deaths/population)*1000000),
         #calculate the date difference between now and when a country first start provide vaccines
         FIRST_VACCINE_DATE = as.Date('2022-02-09') - FIRST_VACCINE_DATE,
         FIRST_VACCINE_DATE = as.numeric(FIRST_VACCINE_DATE)
  )

#obtain the final data which will be using in this project
covid_trim <- covid %>%
  select(c("COUNTRY", 
           "ISO3", 
           "WHO_REGION", 
           "PERSONS_VACCINATED_1PLUS_DOSE_PER100", 
           "PERSONS_FULLY_VACCINATED_PER100", 
           "FIRST_VACCINE_DATE","vaccine_type_used", 
           "total_new_cases_per_million", 
           "total_new_deaths_per_million", 
           "stringency_index" ))  %>% 
  mutate(vaccine_type_used = droplevels(vaccine_type_used))

tibble(head(covid_trim,6))
