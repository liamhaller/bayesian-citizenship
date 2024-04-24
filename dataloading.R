#dataloading


library(haven)
library(dplyr)
oap <- haven::read_dta('/Users/haller/Desktop/DeZIM/Projects/Bayesian Citizenship/dezim_panel_dw1_p_2024_03_07.dta')

#Select only relevant questions 
oap <- oap %>% 
  select(aw0soc001, aw0soc003, aw0soc004, aw0soc005_o, 
         aw0soc007, aw0soc014, dw1soc002, dw1soc004, dw1soc005_e,
         dw1soc005_f, dw1soc005_g, dw1soc005_h, dw1soc005_i, dw1soc005_j,
         dw1soc005_k, dw1soc005a_wn, dw1soc005a_ka, dw1soc005b_a, dw1soc005b_f,
         dw1soc005b_g, dw1soc005b_b, dw1soc005b_i, dw1soc005b_c, dw1soc005b_k,
         dw1soc005b_d, dw1soc005b_wn, dw1soc005b_ka, dw1soc005c_e, dw1soc005c_n,
         dw1soc005c_g, dw1soc005c_p, dw1soc005c_i, dw1soc005c_j, dw1soc005c_k,
         dw1soc005c_l, dw1soc005c_m, dw1soc005c_wn, dw1soc005c_ka,
         dw1soc006a, dw1soc006b, dw1soc006c, dw1soc007aa, dw1soc007ab, dw1soc007ac,
         dw1soc007ba, dw1soc007bb, dw1soc007bc, dw1soc007ca, dw1soc007cb, dw1soc007cc,
         dw1soc008) %>% 
  #Demographics
  mutate(gender = as_factor(aw0soc001), .keep = 'unused') %>% 
  mutate(birth_year = as_factor(aw0soc003), .keep = 'unused') %>% 
  mutate(birth_in_de = as_factor(aw0soc004), .keep = 'unused') %>% 
  mutate(birth_country = as_factor(aw0soc005_o), .keep = 'unused') %>% 
  mutate(migration_year = as_factor(aw0soc007), .keep = 'unused') %>% 
  mutate(residence_status = as_factor(aw0soc014), .keep = 'unused') %>% 
  mutate(citizenship = as_factor(dw1soc002), .keep = 'unused') %>% 
  mutate(plan_to_apply = as_factor(dw1soc004), .keep = 'unused') %>% 
  
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
        


#filter only respondents that do not have German citizenship
oap <- oap %>% 
  filter(citizenship == 'eine andere Staatsangehörigkeit, und zwar:')

#Drop unused levels

oap <- oap %>% droplevels()


# Rename factors ----------------------------------------------------------


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

# levels(oap$overwhelmed)[levels(oap$overwhelmed)=='Keine Angabe'] <- NA
# levels(oap$overwhelmed)[levels(oap$overwhelmed)=='-77'] <- NA
# levels(oap$overwhelmed)[levels(oap$overwhelmed)=='Weiß nicht'] <- NA
# 
# oap$overwhelmed <- factor(oap$overwhelmed, levels = c('Stimme überhaupt nicht zu', 'Stimme eher nicht zu', 
#                                                       'Teils/teils', 'Stimme eher zu', 'Stimme voll und ganz zu'), 
#                           labels =  c('Stimme überhaupt nicht zu', 'Stimme eher nicht zu', 
#                                       'Teils/teils', 'Stimme eher zu', 'Stimme voll und ganz zu'))
       


