#dataloading


library(haven)
library(tidyverse)

oap <- haven::read_dta('/Users/haller/Desktop/DeZIM/Projects/Bayesian Citizenship/dezim_panel_dw1_p_2024_03_07.dta')

oap %>% select(1:60) %>% as_factor() %>% glimpse()

oap %>% select(aw0soc040) %>% as_factor()

# Select and rename relevant questions ------------------------------------


#Selaw0soc009#Select only relevant questions 
oap <- oap %>% 
  select(aw0soc001, aw0soc003, aw0soc004, aw0soc005_o, 
         aw0soc007, cw2v_430, aw0soc009, dw1soc004, dw1soc005_e, aw0soc014, #aw0soc014 = first wave residence, cw2v_430 = second wave residence
         dw1soc005_f, dw1soc005_g, dw1soc005_h, dw1soc005_i, dw1soc005_j,
         dw1soc005_k, dw1soc005a_wn, dw1soc005a_ka, dw1soc005b_a, dw1soc005b_f,
         dw1soc005b_g, dw1soc005b_b, dw1soc005b_i, dw1soc005b_c, dw1soc005b_k,
         dw1soc005b_d, dw1soc005b_wn, dw1soc005b_ka, dw1soc005c_e, dw1soc005c_n,
         dw1soc005c_g, dw1soc005c_p, dw1soc005c_i, dw1soc005c_j, dw1soc005c_k,
         dw1soc005c_l, dw1soc005c_m, dw1soc005c_wn, dw1soc005c_ka,
         dw1soc006a, dw1soc006b, dw1soc006c, dw1soc007aa, dw1soc007ab, dw1soc007ac,
         dw1soc007ba, dw1soc007bb, dw1soc007bc, dw1soc007ca, dw1soc007cb, dw1soc007cc,
         dw1soc008, aw0soc040) %>% 
  #Demographics
  mutate(gender = as_factor(aw0soc001), .keep = 'unused') %>% 
  mutate(birthyear = as_factor(aw0soc003), .keep = 'unused') %>% 
  mutate(birthinde = as_factor(aw0soc004), .keep = 'unused') %>% 
  mutate(birthcountry = as_factor(aw0soc005_o), .keep = 'unused') %>% 
  mutate(migrationyear = as_factor(aw0soc007), .keep = 'unused') %>% 
  mutate(residencestatus = as_factor(cw2v_430), .keep = 'unused') %>% 
  mutate(residencestatus0 = as_factor(aw0soc014), .keep = 'unused') %>% 
  rename(citizenship = aw0soc009) %>% 
  mutate(plantoapply = as_factor(dw1soc004), .keep = 'unused') %>% 
  mutate(education = as_factor(aw0soc040), .keep = 'unused') %>% 

  
  rename(
        #Why not yet apply for citizenship
         whynotapply.time = dw1soc005_e,
         whynotapply.documents = dw1soc005_f,
         whynotapply.test = dw1soc005_g,
         whynotapply.renounce = dw1soc005_h,
         whynotapply.appointment = dw1soc005_i,
         whynotapply.language = dw1soc005_j,
         whynotapply.uncertain = dw1soc005_k,
         whynotapply.dontknow = dw1soc005a_wn,
         whynotapply.noresponse = dw1soc005a_ka,
         
        #Why against applying for citizenship
         whyagainstapply.redtape = dw1soc005b_a, 
         whyagainstapply.feelgerman = dw1soc005b_f,
         whyagainstapply.renounce = dw1soc005b_g, 
         whyagainstapply.notable = dw1soc005b_b, 
         whyagainstapply.finance = dw1soc005b_i, 
         whyagainstapply.noadvantage = dw1soc005b_c, 
         whyagainstapply.canlivewithout = dw1soc005b_k, 
         whyagainstapply.unsurestay = dw1soc005b_d, 
         whyagainstapply.dontknow = dw1soc005b_wn, 
         whyagainstapply.noresponse = dw1soc005b_ka, 
         
        #Why apply for citizenship
         whyapply.equalrights = dw1soc005c_e,
         whyapply.vote = dw1soc005c_n,
         whyapply.security = dw1soc005c_g,
         whyapply.optiontostay = dw1soc005c_p,
         whyapply.kidcit = dw1soc005c_i,
         whyapply.moveinEU = dw1soc005c_j,
         whyapply.travel = dw1soc005c_k,
         whyapply.feelgerman = dw1soc005c_l,
         whyapply.moveoutEU = dw1soc005c_m,
         whyapply.dontknow = dw1soc005c_wn,
         whyapply.noresponse = dw1soc005c_ka) %>% 
         
        #Frequency of encounters
        mutate(interaction.bamf = as_factor(dw1soc006a), .keep = 'unused') %>% 
        mutate(interaction.auslander = as_factor(dw1soc006b), .keep = 'unused') %>% 
        mutate(interaction.jobcenter = as_factor(dw1soc006c), .keep = 'unused') %>% 
  
        mutate(bamf.respect = as_factor(dw1soc007aa), .keep = 'unused') %>% 
        mutate(bamf.fair = as_factor(dw1soc007ab), .keep = 'unused') %>% 
        mutate(bamf.clear = as_factor(dw1soc007ac), .keep = 'unused') %>% 
        
        mutate(auslander.respect = as_factor(dw1soc007ba), .keep = 'unused') %>% 
        mutate(auslander.fair = as_factor(dw1soc007bb), .keep = 'unused') %>% 
        mutate(auslander.clear = as_factor(dw1soc007bc), .keep = 'unused') %>% 
      
        mutate(jobcenter.respect = as_factor(dw1soc007ca), .keep = 'unused') %>% 
        mutate(jobcenter.fair = as_factor(dw1soc007cb), .keep = 'unused') %>% 
        mutate(jobcenter.clear = as_factor(dw1soc007cc), .keep = 'unused') %>% 
        
        mutate(overwhelmed = as_factor(dw1soc008), .keep = 'unused') 
      

#Drop unused levels

oap <- oap %>% droplevels()


# Rename question categories ----------------------------------------------------------


oap$interaction.auslander <- factor(oap$interaction.auslander, levels = c('Sehr häufig', 'Häufig', 'Gele&shy;gentlich', 'Selten',
                                                                          'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'), 
                                    labels = c('Sehr häufig', 'Häufig', 'Gelegentlich', 'Selten',
                                               'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'))

oap$interaction.bamf <- factor(oap$interaction.bamf, levels = c('Sehr häufig', 'Häufig', 'Gele&shy;gentlich', 'Selten',
                                                                          'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'), 
                                    labels = c('Sehr häufig', 'Häufig', 'Gelegentlich', 'Selten',
                                               'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'))


oap$interaction.jobcenter <- factor(oap$interaction.jobcenter, levels = c('Sehr häufig', 'Häufig', 'Gele&shy;gentlich', 'Selten',
                                                                          'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'), 
                                    labels = c('Sehr häufig', 'Häufig', 'Gelegentlich', 'Selten',
                                               'Sehr selten', 'Nie', 'Weiß nicht', 'Keine Angabe'))



# Residence status --------------------------------------------------------

#Data on residence status was collected twice. Once during Wave 1 (aw0soc014)
#and again during wave 6 cw2v_430. Wave 6 is taken as the most up to date, and
#missign values are replaced with wave 1 information when availalbe. 

#The names of categories changed between the waves so they are synced here
oap <- oap %>% 
  mutate(residencestatus0 = case_when(residencestatus0 == 'Niederlassungs-/Daueraufenthaltsrecht für EU-Bürger Aufenthaltsgestattung' ~
                                        'Eine Niederlassungserlaubnis, d. h. ein unbefristetes Daueraufenthaltsrecht in Deutschland',
                                      #not exact mach, but neither are perminant residency which the cateogories are ultimatly fitlered into as a binary
                                      residencestatus0 == 'Antrag auf Aufenthaltstitel gestellt' ~ 'Aufenthaltsgestattung', 
                                      residencestatus0 == 'EU Blaue Karte' ~ 'Blaue Karte (EU)',
                                      residencestatus0 == 'Touristenvisum' ~ 'Visum',
                                      residencestatus0 == 'Filtermissing' ~ NA_character_,
                                      residencestatus0 == 'Nicht bestimmbar' ~ NA_character_,
                                      residencestatus0 == 'Keine Angabe' ~ NA_character_,
                                      TRUE ~ as.character(residencestatus0)))


oap <- oap %>% 
  mutate(residencestatus = coalesce(residencestatus, residencestatus0)) 

# Filter dataset ----------------------------------------------------------

#only include individuals who have answered the plan to apply and citizenship question ~ 396
oap <- oap %>% 
  filter(!plantoapply == -77) %>% 
  filter(!citizenship %in% c(-999,-998,-996, -969))
  
