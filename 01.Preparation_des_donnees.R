#install.packages("janitor")
#install.packages("htmlTable")
#install.packages("MASS")
#install.packages('lmtest')
#install.packages('VGAM')
#install.packages('rms')
#install.packages('oglmx')
library("tidyverse")
library('VGAM')
library('rms')
library('oglmx')
library(tidyr)
library("readxl")
library(janitor)
library(dplyr)
library(RColorBrewer)
library(htmlTable)
library(data.table)
require("MASS")
library(sandwich)
library(lmtest)
dev.off()
rm(list = ls())

##Uploading the data 
data <- read_excel("110243_Centre_CIRED_Pb_Env_Donnees_finales_VF.xlsx", sheet = "Labels")
data <- data %>% row_to_names(row_number = 1)

##Deleting data that are not useful for the analysis 
data <- data %>% dplyr::select(-c(record, 
                           C1_GENDER, C1_PrenomA, C1_PrenomB, 
                           Q2B_2, Q2B_2_OE, Q2B_3, 
                           C2__GENDER, C2__PrenomA, C2__PrenomB, 
                           Q2B_2_eolien, Q2B_2_eolien_OE, Q2B_3_eolien,
                           C3__GENDER, C3__PrenomA, C3__PrenomB,
                           Q2B2_pollution, Q2B2_pollution_OE, Q2B_3_pollution, 
                           C4__GENDER, C4__PrenomA, C4__PrenomB, 
                           Q2b_2_energique, Q2b_2_energique_OE, Q2B_3_energique, 
                           C5__GENDER, C5__PrenomA, C5__PrenomB,
                           Q2b_2_esapce_naturel, Q2b_2_esapce_naturel_OE, Q2b_3_espace_naturel))

##Renaming the variable with relevant names 
data <- data %>% rename(#caractériques socio-économiques des répondants : 
                        "sexe" = "Q1_1", 
                        "age" = "Q1_2", "age_recode" = "recode_age",
                        "diplome" = "Q1_3", 
                        "activité" = "Q1_4", "travail" = "Q1_5", "public" = "Q1_6", 
                        "enfants" = "Q1_7r1", "adultes" = "Q1_7r2", 
                        "agglo" = "CC", "region" = "REG2016", "departement" = "DPT", 
                        "revenu" = "Q1_9", 
                        "time" = "qtime",
                              
                        #risques climatiques 
                        "combi_RC" = "C1_REC_Combi", 
                        "Qcompr_RC_D" = "Q1_risquer1", "Qcompr_RC_R" = "Q1_risquer2", 
                        "Qcompr_RC_C" = "Q1_risquer3", "Qcompr_RC_P" = "Q1_risquer4",
                        "Qcompr_RC_D_ordre" = "order_Q1_risquer1",
                        "Qcompr_RC_R_ordre" = "order_Q1_risquer2",
                        "Qcompr_RC_C_ordre" = "order_Q1_risquer3",
                        "Qcompr_RC_P_ordre" = "order_Q1_risquer4", 
                        "Qjug_RC" = "Q2B_1", "ordre_RC" = "ORDRE1",
                              
                        #parc éolien 
                        "combi_Eol" = "C2__REC_Combi", 
                        "Qcompr_Eol_D" = "Q1_eolienr2", "Qcompr_Eol_R" = "Q1_eolienr1", 
                        "Qcompr_Eol_C" = "Q1_eolienr4", "Qcompr_Eol_P" = "Q1_eolienr3",
                        "Qcompr_Eol_D_ordre" = "order_Q1_eolienr2",
                        "Qcompr_Eol_R_ordre" = "order_Q1_eolienr1",
                        "Qcompr_Eol_C_ordre" = "order_Q1_eolienr4",
                        "Qcompr_Eol_P_ordre" = "order_Q1_eolienr3", 
                        "Qjug_Eol" = "Q2B_1_eolien", "ordre_Eol" = "ORDRE2",
                              
                        #pollution de l'air 
                        "combi_Pol" = "C3__REC_Combi", 
                        "Qcompr_Pol_D" = "Q1_pollutionr3", "Qcompr_Pol_R" = "Q1_pollutionr4", 
                        "Qcompr_Pol_C" = "Q1_pollutionr1", "Qcompr_Pol_P" = "Q1_pollutionr2",
                        "Qcompr_Pol_D_ordre" = "order_Q1_pollutionr3",
                        "Qcompr_Pol_R_ordre" = "order_Q1_pollutionr4",
                        "Qcompr_Pol_C_ordre" = "order_Q1_pollutionr1",
                        "Qcompr_Pol_P_ordre" = "order_Q1_pollutionr2",
                        "Qjug_Pol" = "Q2B_1_pollution", "ordre_Pol" = "ORDRE3",
                          
                        #performance énergétique
                        "combi_DPE" = "C4__REC_Combi", 
                        "Qcompr_DPE_D" = "Q1_energiquer4", "Qcompr_DPE_R" = "Q1_energiquer1", 
                        "Qcompr_DPE_C" = "Q1_energiquer2", "Qcompr_DPE_P" = "Q1_energiquer3",
                        "Qcompr_DPE_D_ordre" = "order_Q1_energiquer4",
                        "Qcompr_DPE_R_ordre" = "order_Q1_energiquer1",
                        "Qcompr_DPE_C_ordre" = "order_Q1_energiquer2",
                        "Qcompr_DPE_P_ordre" = "order_Q1_energiquer3", 
                        "Qjug_DPE" = "Q2b_1_energique", "ordre_DPE" = "ORDRE4",
                              
                        #espaces naturels
                        "combi_Nat" = "C5__REC_Combi", 
                        "Qcompr_Nat_D" = "Q1_espace_naturelr3", "Qcompr_Nat_R" = "Q1_espace_naturelr2", 
                        "Qcompr_Nat_C" = "Q1_espace_naturelr1", "Qcompr_Nat_P" = "Q1_espace_naturelr4",
                        "Qcompr_Nat_D_ordre" = "order_Q1_espace_naturelr3",
                        "Qcompr_Nat_R_ordre" = "order_Q1_espace_naturelr2",
                        "Qcompr_Nat_C_ordre" = "order_Q1_espace_naturelr1",
                        "Qcompr_Nat_P_ordre" = "order_Q1_espace_naturelr4", 
                        "Qjug_Nat" = "Q2B_1_espace_naturel", "ordre_Nat" = "ORDRE5")


##
var <- c("ordre_RC", "ordre_Eol", "ordre_Pol", "ordre_DPE", "ordre_Nat", 
         "Qcompr_RC_C_ordre", "Qcompr_RC_D_ordre", "Qcompr_RC_P_ordre", "Qcompr_RC_R_ordre", 
         "Qcompr_Eol_C_ordre", "Qcompr_Eol_D_ordre", "Qcompr_Eol_P_ordre", "Qcompr_Eol_R_ordre",
         "Qcompr_Pol_C_ordre","Qcompr_Pol_D_ordre", "Qcompr_Pol_P_ordre", "Qcompr_Pol_R_ordre",
         "Qcompr_DPE_C_ordre", "Qcompr_DPE_D_ordre", "Qcompr_DPE_P_ordre", "Qcompr_DPE_R_ordre",
         "Qcompr_Nat_C_ordre", "Qcompr_Nat_D_ordre", "Qcompr_Nat_P_ordre", "Qcompr_Nat_R_ordre")

for (j in var){
  data[[j]][data[[j]] == "Premier"] <- 1
  data[[j]][data[[j]] == "Deuxième"] <- 2
  data[[j]][data[[j]] == "Troisième"] <- 3
  data[[j]][data[[j]] == "Quatrième"] <- 4
  data[[j]][data[[j]] == "Cinquième"] <- 5
}


##Creating new variables in order to ease the analyses  
new_var <- c("C2_RC", "D2_RC", "P2_RC", "R2_RC", 
             "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
             "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", 
             "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", 
             "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat") 

var <- c("combi_RC", "combi_Eol", "combi_Pol", "combi_DPE", "combi_Nat")
dim <- c("C2", "D2", "P2", "R2")

for (j in 0:4) {
  for (k in 1:4) {
    column <- var[j+1]
    new_column <- new_var[4*j+k]
    data[[new_column]] <- grepl(dim[k], data[[column]])
  }}

count <- c("count_RC", "count_Eol", "count_Pol", "count_DPE", "count_Nat") 
for (j in 0:4) {data[[count[j+1]]] <- rowSums(data[new_var[(4*j + 1) : (4*j + 4)]], na.rm=TRUE)}

##
var <- c("C2_RC", "D2_RC", "P2_RC", "R2_RC", 
         "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
         "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", 
         "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", 
         "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat")

new_var <- c("cumul_RC_D", "cumul_RC_P", "cumul_RC_R",
             "cumul_Eol_D", "cumul_Eol_P", "cumul_Eol_R",
             "cumul_Pol_D", "cumul_Pol_P", "cumul_Pol_R",
             "cumul_DPE_D", "cumul_DPE_P", "cumul_DPE_R",
             "cumul_Nat_D", "cumul_Nat_P", "cumul_Nat_R")

for (j in 0:4) {
  for (k in 1:3) {
    var1 <- var[j*4 + 1]
    var2 <- var[j*4 + 1 + k]
    new_column <- new_var[3*j+k]
    data[[new_column]] <- data[[var1]]*data[[var2]]
  }}

#new_var <- c("cumul_RC_DP", "cumul_RC_DR",
             #"cumul_Eol_DP", "cumul_Eol_DR",
             #"cumul_Pol_DP", "cumul_Pol_DR",
             #"cumul_DPE_DP", "cumul_DPE_DR",
             #"cumul_Nat_DP", "cumul_Nat_DR")

#for (j in 0:4) {
  #for (k in 1:2) {
    #var1 <- var[j*4 + 2]
  #   var2 <- var[j*4 + 2 + k]
  #   new_column <- new_var[2*j+k]
  #   data[[new_column]] <- data[[var1]]*data[[var2]]
  # }}

# new_var <- c("cumul_RC_PR", "cumul_Eol_PR", 
#              "cumul_Pol_PR", "cumul_DPE_PR", "cumul_Nat_PR")
# 
# for (j in 0:4) {
#     var1 <- var[j*4 + 3]
#     var2 <- var[j*4 + 4]
#     new_column <- new_var[j +1]
#     data[[new_column]] <- data[[var1]]*data[[var2]]
#   }


##
var <- c("Qjug_RC", "Qjug_Eol", "Qjug_Pol", "Qjug_DPE", "Qjug_Nat")
new_var <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 1:5) {
  data[[new_var[i]]] <- (data[[var[i]]] %in% c("Plutôt injuste", "Très injuste"))}


##
data$'travail'[is.na(data$'travail')] = 'Inactif'
data$'public'[is.na(data$'public')] = 'Inactif'

##
data$'statut' = 'Inconnu'
for (i in 1:nrow(data)) {
  if (data$revenu[i] %in% c("De plus de 2 500 € à 4 000 €", "De plus de 4 000 € à 6 000 €", 
                            "De plus de 6 000 € à 10 000 €", "Plus de 10 000 €")) {
    #&& data$travail[i] %in% c("Artisan, commerçant, chef d’entreprise",
    #"Cadre, profession intellectuelle supérieure")) {
    data$statut[i] <- "Favorisé"}
  else if (data$travail[i] %in% c("Artisan, commerçant, chef d’entreprise", 
                                  "Cadre, profession intellectuelle supérieure")) {
    data$statut[i] <- "Favorisé"} 
  else if (data$public[i] == "Oui") {data$statut[i] <- "Favorisé"} 
  else {data$statut[i] <- "Défavorisé"}} 

data$"statut" <- factor(data$"statut", levels = c("Favorisé", "Défavorisé"))

##
data$diplome_recode[data$diplome %in% c("École primaire ou aucun", "Brevet", "CAP ou BEP")] <- 'Diplôme du brevet,\néquivalent ou moins'
data$diplome_recode[data$diplome %in% c("Baccalauréat technologique ou professionnel", "Baccalauréat général")] <- 'Diplôme du baccalauréat'
data$diplome_recode[data$diplome %in% c("Bac +2 (BTS, DUT, DEUG…)", "Bac +3 (licence…)")] <- "Premier cycle de\nl'enseignement supérieur"
data$diplome_recode[data$diplome %in% c("Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)")] <- "Deuxième cycle de\nl'enseignement supérieur\nou plus"
data$diplome_recode <- factor(data$diplome_recode, levels = c('Diplôme du brevet,\néquivalent ou moins',
                                                              'Diplôme du baccalauréat', 
                                                              "Premier cycle de\nl'enseignement supérieur", 
                                                              "Deuxième cycle de\nl'enseignement supérieur\nou plus"))

##overview
str(data)

rm(var, var1, var2, new_var, dim, i, j, k, new_column, column, count)
