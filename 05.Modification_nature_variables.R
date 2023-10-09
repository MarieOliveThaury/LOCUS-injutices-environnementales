##Tranforming data that are supposed to be numeric and not character

ordre <- c("ordre_RC", "ordre_Eol", "ordre_Pol", "ordre_DPE", "ordre_Nat", 
         "Qcompr_RC_C_ordre", "Qcompr_RC_D_ordre", "Qcompr_RC_P_ordre", "Qcompr_RC_R_ordre", 
         "Qcompr_Eol_C_ordre", "Qcompr_Eol_D_ordre", "Qcompr_Eol_P_ordre", "Qcompr_Eol_R_ordre",
         "Qcompr_Pol_C_ordre","Qcompr_Pol_D_ordre", "Qcompr_Pol_P_ordre", "Qcompr_Pol_R_ordre",
         "Qcompr_DPE_C_ordre", "Qcompr_DPE_D_ordre", "Qcompr_DPE_P_ordre", "Qcompr_DPE_R_ordre",
         "Qcompr_Nat_C_ordre", "Qcompr_Nat_D_ordre", "Qcompr_Nat_P_ordre", "Qcompr_Nat_R_ordre")

count <- c("count_RC", "count_Eol", "count_Pol", "count_DPE", "count_Nat")
to_numeric <- c(ordre, count, 'age', 'enfants', 'adultes', 'time')
for (x in to_numeric) {data[[x]] <- as.numeric(data[[x]])} 


ordre <- c("ordre", "Qcompr_C_ordre", "Qcompr_D_ordre", "Qcompr_P_ordre", "Qcompr_R_ordre")
to_numeric <- c(ordre, "count", 'age', 'enfants', 'adultes', 'time')
for (x in to_numeric) {data_compil[[x]] <- as.numeric(data_compil[[x]])} 


##Transforming the judgement into a ordinal categorical variable 
to_factor <- c("Qjug_RC", "Qjug_Eol", "Qjug_Pol", "Qjug_DPE", "Qjug_Nat")

for (x in to_factor) {
  data[[x]] <- factor(data[[x]],
                      levels = c("Très juste", "Plutôt juste", "Plutôt injuste", "Très injuste"))
  #data[[x]] <- as.numeric(data[[x]])
}

data_compil$"Qjug" <- factor(data_compil$"Qjug",
                             levels = c("Très juste", "Plutôt juste", "Plutôt injuste", "Très injuste"))
#data_compil$"Qjug" <-  as.numeric(data_compil$"Qjug")

##Transforming other variables into a ordinal categorical variable 

features = c("sexe", "activité", "travail", "CSP", "public", 
             "region", "departement", "UDA5", "UDA9")

var <- c("C2_RC", "D2_RC", "P2_RC", "R2_RC", 
         "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
         "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", 
         "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", 
         "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat", 
         "Qcompr_RC_C", "Qcompr_RC_D", "Qcompr_RC_P", "Qcompr_RC_R",
         "Qcompr_Eol_C", "Qcompr_Eol_D", "Qcompr_Eol_P", "Qcompr_Eol_R",
         "Qcompr_Pol_C", "Qcompr_Pol_D", "Qcompr_Pol_P", "Qcompr_Pol_R",
         "Qcompr_DPE_C", "Qcompr_DPE_D", "Qcompr_DPE_P", "Qcompr_DPE_R",
         "Qcompr_Nat_C", "Qcompr_Nat_D", "Qcompr_Nat_P", "Qcompr_Nat_R",
         "C_verif_RC", "D_verif_RC", "P_verif_RC", "R_verif_RC", 
         "C_verif_Eol", "D_verif_Eol", "P_verif_Eol", "R_verif_Eol", 
         "C_verif_Pol", "D_verif_Pol", "P_verif_Pol", "R_verif_Pol", 
         "C_verif_DPE", "D_verif_DPE", "P_verif_DPE", "R_verif_DPE",
         "C_verif_Nat", "D_verif_Nat", "P_verif_Nat", "R_verif_Nat",
         "cumul_RC_D", "cumul_RC_P", "cumul_RC_R",
         "cumul_Eol_D", "cumul_Eol_P", "cumul_Eol_R",
         "cumul_Pol_D", "cumul_Pol_P", "cumul_Pol_R",
         "cumul_DPE_D", "cumul_DPE_P", "cumul_DPE_R",
         "cumul_Nat_D", "cumul_Nat_P", "cumul_Nat_R",
         "Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat", 
         "cadrage", "C2", "D2", "P2", "R2", 
         "Qcompr_C", "Qcompr_D", "Qcompr_P", "Qcompr_R", 
         "C_verif", "D_verif", "P_verif", "R_verif", 
         "cumul_D", "cumul_P", "cumul_R", "Injuste")

to_factor <- c(features, var[1:80])
for (x in to_factor) {data[[x]] <- factor(data[[x]])}

to_factor <- c(features, var[81:97])
for (x in to_factor) {data_compil[[x]] <- factor(data_compil[[x]])}


data$"age_recode" <- factor(data$'age_recode', 
                            levels = c("18-24 ans", "25-34 ans", "35-49 ans", "50-64 ans", "65+"))
data_compil$"age_recode" <- factor(data_compil$'age_recode', 
                                   levels = c("18-24 ans", "25-34 ans", "35-49 ans", "50-64 ans", "65+"))

data$"diplome" <- factor(data$'diplome', 
                         levels = c("École primaire ou aucun", "Brevet", "CAP ou BEP",
                                    "Baccalauréat technologique ou professionnel", 
                                    "Baccalauréat général",
                                    "Bac +2 (BTS, DUT, DEUG…)", "Bac +3 (licence…)", 
                                    "Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)"))
data_compil$"diplome" <- factor(data_compil$'diplome', 
                         levels = c("École primaire ou aucun", "Brevet", "CAP ou BEP",
                                    "Baccalauréat technologique ou professionnel", 
                                    "Baccalauréat général",
                                    "Bac +2 (BTS, DUT, DEUG…)", "Bac +3 (licence…)", 
                                    "Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)"))


data$"revenu" <- factor(data$'revenu', 
                        levels = c("400 € ou moins", "De plus de 400 € à 800 €",
                                   "De plus de 800 € à 1 200 €",
                                   "De plus de 1 200 € à 1 800 €",
                                   "De plus de 1 800 € à 2 500 €",
                                   "De plus de 2 500 € à 4 000 €",
                                   "De plus de 4 000 € à 6 000 €",
                                   "De plus de 6 000 € à 10 000 €",
                                   "Plus de 10 000 €", "Ne souhaite pas répondre"))
data_compil$"revenu" <- factor(data_compil$'revenu', 
                               levels = c("400 € ou moins", 
                                          "De plus de 400 € à 800 €",
                                          "De plus de 800 € à 1 200 €",
                                          "De plus de 1 200 € à 1 800 €", 
                                          "De plus de 1 800 € à 2 500 €", 
                                          "De plus de 2 500 € à 4 000 €", 
                                          "De plus de 4 000 € à 6 000 €", 
                                          "De plus de 6 000 € à 10 000 €", 
                                          "Plus de 10 000 €", 
                                          "Ne souhaite pas répondre"))

data$"agglo" <- factor(data$'agglo', 
                       levels = c("Commune rurale",
                                  "De 2.000 à moins de 20.000 hab.",
                                  "De 20.000 à moins de 100.000 hab.",
                                  "100.000 hab. et plus", 
                                  "Agglomération parisienne"))
data_compil$"agglo" <- factor(data_compil$'agglo', 
                              levels = c("Commune rurale",
                                         "De 2.000 à moins de 20.000 hab.",
                                         "De 20.000 à moins de 100.000 hab.",
                                         "100.000 hab. et plus", 
                                         "Agglomération parisienne"))

rm(ordre, var, count, features, to_numeric, to_factor, x)
