#Data processing

source('dataloading.R')
library(countrycode) #needed to join HDI data
library(tidyindex) #needed for HDI 2022 data

# HDI Data ----------------------------------------------------------------

#Join HDI data to OAP and create binary variable for whether an individuals country of citizenship is
# above the mean HDI (1) or below the mean HDI (0)

#Because some HDI data is missing for Kurdistan & Kosovo I replaced the country with either
#Most likely estimate for Kurdistan and comprable HDI for Kosovo
oap <- oap %>% 
  mutate(citizenship = case_when(citizenship == 904 ~ 792, #Replace Kurdistan with Turkey 
                                 citizenship == 903 ~ 70,  #Replace Kosovo with Bosnia
                                 TRUE ~ citizenship)) 



#First join country code data to Pair ISO (found in OAP) with English name
countrycodes <- data.frame(codelist$iso3n, codelist$country.name.en)
colnames(countrycodes) <- c('citizenship', 'citizenship_name_en')
oap <- dplyr::left_join(oap, countrycodes, by = 'citizenship')

#Now join english name to HDI data found in tidyindex library
hdi <- tidyindex::hdi[,2:3]
colnames(hdi) <- c('citizenship_name_en', 'hdi')

oap <- oap %>% 
  mutate(citizenship_name_en = case_when(citizenship_name_en == "Syria" ~ 'Syrian Arab Republic', 
                                         citizenship_name_en == "Turkey" ~ 'Türkiye', 
                                         citizenship_name_en == "Bosnia & Herzegovina" ~ 'Bosnia and Herzegovina', 
                                         citizenship_name_en == "Iran" ~ 'Iran (Islamic Republic of)', 
                                         citizenship_name_en == "South Korea" ~ 'Korea (Republic of)', 
                                         citizenship_name_en == "Palestinian Territories" ~ 'Palestine, State of', 
                                         citizenship_name_en == "Russia" ~ 'Russian Federation', 
                                         citizenship_name_en == "Moldova" ~ 'Moldova (Republic of)', 
                                         citizenship_name_en == "Aruba" ~ 'Italy', #replaced 
                                         citizenship_name_en == "Somalia" ~ 'Guinea', #data not recorded, replaced with equivalent 
                                         citizenship_name_en == "Taiwan" ~ 'Norway', #Replaced
                                         citizenship_name_en == "Vietnam" ~ 'Viet Nam', 
                                         citizenship_name_en == "Venezuela" ~ 'Venezuela (Bolivarian Republic of)', 
                                         citizenship_name_en == "Côte d’Ivoire" ~ "Côte d'Ivoire", 
                                         citizenship_name_en == "French Guiana" ~ 'Guinea', 
                                         citizenship_name_en == "Congo - Brazzaville" ~ 'Congo', 
                                         citizenship_name_en == "Bolivia" ~ 'Bolivia (Plurinational State of)', 
                                         TRUE ~ as.character(citizenship_name_en))) 

oap <- dplyr::left_join(oap, hdi, by = 'citizenship_name_en')

#If country HDI is above mean 1, if not 0
mean_hdi <- mean(hdi$hdi)
oap$b_hdi <- ifelse(oap$hdi > mean_hdi, 1, 0)


# Residence status ------------------------------------------------------------------

#Create binary variable for whether respondent has permanent residency (1) or not (0)
oap <- oap %>% 
  mutate(b_residencestatus = case_when(residencestatus == "Staatsbürgerschaft der EU bzw. des europäischen Wirtschaftsraumes" ~ 1,
                                       residencestatus == "Blaue Karte (EU)" ~ 0,
                                       residencestatus == "Visum" ~ 0,
                                       residencestatus == "Eine Niederlassungserlaubnis, d. h. ein unbefristetes Daueraufenthaltsrecht in Deutschland" ~ 1,
                                       residencestatus == "Niederlassungs-/Daueraufenthaltsrecht für EU-Bürger" ~ 1,
                                       residencestatus == "Befristete Aufenthaltserlaubnis" ~ 0,
                                       residencestatus == "Aufenthaltsgestattung" ~ 0,
                                       residencestatus == "Duldung" ~ 0,
                                       residencestatus == "Anderer, und zwar:" ~ 0,
                                       TRUE ~ NA_integer_ )) #Weiß nicht + Keine Angabe

table(oap$residencestatus)
table(oap$b_residencestatus)

# Requirements -------------------------------------------------------------

#Create binary variable for whether respondent has completed the requirements (1), or not (0)

#For those that have not applied or not yet applied they were asked the reason for not applying.
#Those that answered a specific requirement or that they were unsure/unable were coded as not
#having met the requirements
# oap <- oap %>% 
#   mutate(b_requirements = case_when(whynotapply.time == 1 ~ 0,
#                                     whynotapply.documents == 1 ~ 0,
#                                     as.numeric(whynotapply.test) == 1 ~ 0,
#                                     whynotapply.language == 1 ~ 0,
#                                     whynotapply.uncertain == 1 ~ 0,
#                                     whyagainstapply.notable == 1 ~ 0,
#                                     whyagainstapply.finance == 1 ~ 0,
#                                     TRUE ~ 1))
library(haven)
oap <- oap %>%
  mutate(across(starts_with("whynotapply"), zap_labels),
         across(starts_with("whyagainstapply"), zap_labels)) %>%
  mutate(b_requirements = case_when(
    whynotapply.time == 1 ~ 0,
    whynotapply.documents == 1 ~ 0,
    whynotapply.test == 1 ~ 0,
    whynotapply.language == 1 ~ 0,
    whynotapply.uncertain == 1 ~ 0,
    whyagainstapply.notable == 1 ~ 0,
    whyagainstapply.finance == 1 ~ 0,
    TRUE ~ 1
  )) 
table(oap$b_requirements)

# Bureaucratic trajectory -------------------------------------------------

#Create a binary variable that indicates respondents overall expereince with bureaucracy (bureacratic trajectory)

# 1 indicates turbulent bureaucratic trajectory, 0 indicates mild bureaucratic trajectory
oap <- oap %>% 
  mutate(b_bureaucratictrajectory = case_when(overwhelmed == 'Stimme voll und ganz zu' ~ 1,
                                              overwhelmed == 'Stimme eher zu' ~ 0,
                                              overwhelmed == 'Teils/teils' ~ 0,
                                              overwhelmed == 'Stimme eher nicht zu' ~ 0,
                                              overwhelmed == "Stimme überhaupt nicht zu" ~ 0,
                                              overwhelmed == -77 ~ 0, #had no interaction with bureaucracy
                                              overwhelmed == "Weiß nicht" ~ NA_integer_,
                                              TRUE ~ NA_integer_)) #keine Angabe

table(oap$overwhelmed)
table(oap$b_bureaucratictrajectory)

# Frequency ---------------------------------------------------------------

#If respondents reported meeting very frequently or frequently with any of the three 
#following agencies then they were assigned to have a high frequency of interaction (1),
#if they did not report high levels of interaction then they have low frequency (0)

#1 indicatse high frequency, 0 indicates low frequency of interaction with bureaucracies
oap <- oap %>% 
  mutate(b_frequency = case_when(interaction.bamf == "Sehr häufig" ~ 1,
                                 interaction.bamf == "Häufig" ~ 1,
                                 interaction.auslander == "Sehr häufig" ~ 1,
                                 interaction.auslander == "Häufig" ~ 1,
                                 interaction.jobcenter == "Sehr häufig" ~ 1,
                                 interaction.jobcenter == "Häufig" ~ 1,
                                 
                                 #unsure = NA
                                 interaction.bamf == "Keine Angabe" ~ NA_integer_,
                                 interaction.auslander == "Keine Angabe" ~ NA_integer_,
                                 interaction.jobcenter == "Keine Angabe" ~ NA_integer_,

                                 #Medium, low, or never results in a low frequency
                                 TRUE ~ 0))


# Treatment ---------------------------------------------------------------

#Variable to assess how individuals were treated when visiting bureaucratic agencies
#0 indicates distinctly negative treatment, 1 indicates no negative treatment
oap <- oap %>% 
  mutate(b_treatment = case_when(bamf.respect == "Stimme überhaupt nicht zu" ~ 0,
                                 bamf.respect == "Stimme eher nicht zu" ~ 0,
                                 bamf.fair == "Stimme überhaupt nicht zu" ~ 0,
                                 bamf.fair == "Stimme eher nicht zu" ~ 0,
                                 bamf.clear == "Stimme überhaupt nicht zu" ~ 0,
                                 bamf.clear == "Stimme eher nicht zu" ~ 0,
                                 
                                 auslander.respect == "Stimme überhaupt nicht zu" ~ 0,
                                 auslander.respect == "Stimme eher nicht zu" ~ 0,
                                 auslander.fair == "Stimme überhaupt nicht zu" ~ 0,
                                 auslander.fair == "Stimme eher nicht zu" ~ 0,
                                 auslander.clear == "Stimme überhaupt nicht zu" ~ 0,
                                 auslander.clear == "Stimme eher nicht zu" ~ 0,
                                 
                                 jobcenter.respect == "Stimme überhaupt nicht zu" ~ 0,
                                 jobcenter.respect == "Stimme eher nicht zu" ~ 0,
                                 jobcenter.fair == "Stimme überhaupt nicht zu" ~ 0,
                                 jobcenter.fair == "Stimme eher nicht zu" ~ 0,
                                 jobcenter.clear == "Stimme überhaupt nicht zu" ~ 0,
                                 jobcenter.clear == "Stimme eher nicht zu" ~ 0,
                                 
                                 bamf.respect == "Keine Angabe" ~ NA_integer_,
                                 bamf.fair == "Keine Angabe" ~ NA_integer_,
                                 bamf.clear == "Keine Angabe" ~ NA_integer_,
                                 
                                 TRUE ~ 1))



# Naturalization Plans ----------------------------------------------------

#1 = positive naturalization intentions meaning have applied or plan to apply
#0 indicates negative naturalization intentions meaning uncertainty or not planning to apply 
oap <- oap %>% 
  mutate(b_naturalization = case_when(plantoapply == 'Ja, ich plane es.' ~ 1,
                                    plantoapply == 'Ja, ich habe es bereits getan.' ~ 1,
                                    plantoapply == 'Nein, ich plane es nicht.' ~ 0,
                                    plantoapply == 'Weiß nicht' ~ 0,
                                    TRUE ~ NA_integer_))
table(oap$plantoapply)




# Education ---------------------------------------------------------------

unique(oap$education)

# 1 indicates turbulent bureaucratic trajectory, 0 indicates mild bureaucratic trajectory
oap <- oap %>% 
  mutate(b_education = case_when(education == 'Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)' ~ 1,
                                              education == 'Realschulabschluss, Mittlere Reife, Fachschulreife oder Abschluss der polytechnischen Oberschule 10. Klasse' ~ 1,
                                              education == 'Fachhochschulreife (Abschluss einer Fachoberschule etc.)' ~ 1,
                                              education == 'Anderen Schulabschluss, und zwar:' ~ 0,
                                              education == "Nicht bestimmbar" ~ 0,
                                              education == "Schule beendet ohne Abschluss" ~ 0,
                                              education == "Hauptschulabschluss, Volksschulabschluss, Abschluss der polytechnischen Oberschule 8. oder 9. Klasse" ~ 0,
                                              education == "Keine Angabe" ~ 0,
                                              education == "Ich gehe noch zur Schule" ~ 0,
                                              education == 'Weiß nicht' ~ 0)) #keine Angabe


# Print the 
# Selected df -------------------------------------------------------------


#Select only variables for M1 model + filter out for complete cases
#Results in loss of ~26 observations (mostly from frequency question)
nat_full <- oap %>% 
  select(b_hdi, b_frequency, b_treatment, b_residencestatus, b_requirements,
         b_bureaucratictrajectory, b_naturalization, b_education) %>% 
  filter(complete.cases(.)) %>%  #~370 observations
  rename(I = b_hdi,
         F = b_frequency,
         T = b_treatment,
         S = b_residencestatus,
         R = b_requirements,
         BT = b_bureaucratictrajectory,
         N = b_naturalization,
         E = b_education)


nat <- nat_full %>% select(-c(T,F)) %>% 
  rename(O = I, A = R) %>% 
  as.data.frame()



# Clean up ----------------------------------------------------------------

rm(countrycodes)
rm(hdi)
rm(mean_hdi)




