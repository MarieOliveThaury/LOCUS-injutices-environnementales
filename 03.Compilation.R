features = c("ID", "sexe", "age", "age_recode", "enfants", "adultes", 'time',
             "diplome", "diplome_recode",
             "activité", "travail", "CSP", "public", "revenu", "statut",
             "CP", "CODGEO", "LIBGEO", "agglo", "region", "departement", "UDA5", "UDA9")

var = c(#Risques climatiques : 
        "combi_RC", "C2_RC", "D2_RC", "P2_RC", "R2_RC", "count_RC", 
        "Qjug_RC", "Injuste_RC", 
        "Qcompr_RC_C", "Qcompr_RC_D", "Qcompr_RC_P", "Qcompr_RC_R",
        "Qcompr_RC_C_ordre", "Qcompr_RC_D_ordre", 
        "Qcompr_RC_P_ordre", "Qcompr_RC_R_ordre", 
        "C_verif_RC", "D_verif_RC", "P_verif_RC", "R_verif_RC", 
        "nb_ok_RC", "ordre_RC", "cumul_RC_D", "cumul_RC_P", "cumul_RC_R",
        
        #Parc eolien :
        "combi_Eol", "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", "count_Eol", 
        "Qjug_Eol", "Injuste_Eol", 
        "Qcompr_Eol_C", "Qcompr_Eol_D", "Qcompr_Eol_P", "Qcompr_Eol_R",
        "Qcompr_Eol_C_ordre", "Qcompr_Eol_D_ordre", 
        "Qcompr_Eol_P_ordre", "Qcompr_Eol_R_ordre", 
        "C_verif_Eol", "D_verif_Eol", "P_verif_Eol", "R_verif_Eol", 
        "nb_ok_Eol", "ordre_Eol", "cumul_Eol_D", "cumul_Eol_P", "cumul_Eol_R",
        
        #Pollution de l'air :
        "combi_Pol", "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", "count_Pol", 
        "Qjug_Pol", "Injuste_Pol", 
        "Qcompr_Pol_C", "Qcompr_Pol_D", "Qcompr_Pol_P", "Qcompr_Pol_R",
        "Qcompr_Pol_C_ordre", "Qcompr_Pol_D_ordre", 
        "Qcompr_Pol_P_ordre", "Qcompr_Pol_R_ordre", 
        "C_verif_Pol", "D_verif_Pol", "P_verif_Pol", "R_verif_Pol", 
        "nb_ok_Pol", "ordre_Pol", "cumul_Pol_D", "cumul_Pol_P", "cumul_Pol_R",
        
        #Performance énergétique :
        "combi_DPE", "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", "count_DPE", 
        "Qjug_DPE", "Injuste_DPE", 
        "Qcompr_DPE_C", "Qcompr_DPE_D", "Qcompr_DPE_P", "Qcompr_DPE_R",
        "Qcompr_DPE_C_ordre", "Qcompr_DPE_D_ordre", 
        "Qcompr_DPE_P_ordre", "Qcompr_DPE_R_ordre", 
        "C_verif_DPE", "D_verif_DPE", "P_verif_DPE", "R_verif_DPE", 
        "nb_ok_DPE", "ordre_DPE", "cumul_DPE_D", "cumul_DPE_P", "cumul_DPE_R",
        
        #Espaces naturels :
        "combi_Nat", "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat", "count_Nat", 
        "Qjug_Nat", "Injuste_Nat", 
        "Qcompr_Nat_C", "Qcompr_Nat_D", "Qcompr_Nat_P", "Qcompr_Nat_R",
        "Qcompr_Nat_C_ordre", "Qcompr_Nat_D_ordre", 
        "Qcompr_Nat_P_ordre", "Qcompr_Nat_R_ordre", 
        "C_verif_Nat", "D_verif_Nat", "P_verif_Nat", "R_verif_Nat", 
        "nb_ok_Nat", "ordre_Nat", "cumul_Nat_D", "cumul_Nat_P", "cumul_Nat_R"
        )

cadrage = c('Risques climatiques', 'Parc éolien', "Pollution de l'air", 
            "Performance énergétique", "Espaces naturels")

new_name = c("combi", "C2", "D2", "P2", "R2", "count",
             "Qjug", "Injuste", 
             "Qcompr_C", "Qcompr_D", "Qcompr_P", "Qcompr_R", 
             "Qcompr_C_ordre", "Qcompr_D_ordre", "Qcompr_P_ordre", "Qcompr_R_ordre", 
             "C_verif", "D_verif", "P_verif", "R_verif", "nb_ok", "ordre", 
             "cumul_D", "cumul_P", "cumul_R")

n = length(new_name)

data_compil = data.frame() 

for (i in 0:4) {
  tab <- data %>% dplyr::select(c(features, var[(n*i + 1): (n*(i + 1))], "moy_ok"))
  tab <- setnames(tab, old = var[(n*i + 1): (n*(i + 1))], new = new_name[1:n])
  tab$'cadrage' <- cadrage[i+1]
  data_compil <- rbind(data_compil, tab) }

rm(features, var, cadrage, new_name, n, i, tab)
                      
