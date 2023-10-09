features = c("ID", "sexe", "age", "age_recode", "enfants", "adultes", 
             "diplome", "activité", "travail", "CSP", "public", "revenu", 
             "CP", "CODGEO", "LIBGEO", "agglo", "region", "departement", "UDA5", "UDA9", 
             "combi", 'C2', 'D2', 'P2', 'R2', 'Qjug', 'nb_ok', 
             'ordre', 'moy_ok', 'cadrage', 'ordre2')

dimension = c('C', 'D', "P", 'R')
var = c("Qcompr_C", "Qcompr_C_ordre", "C_verif", 
        "Qcompr_D", "Qcompr_D_ordre", "D_verif", 
        "Qcompr_P", "Qcompr_P_ordre", "P_verif", 
        "Qcompr_R", "Qcompr_R_ordre", "R_verif") 

new_name = c("Qcompr", "Qcompr_ordre", "Verif")

n = length(new_name)

data_compil_2 = data.frame() 

data_compil$'ordre2' <- data_compil$'ordre' * data_compil$'ordre'

for (i in 0:3) {
  tab <- data_compil %>% dplyr::select(c(features, var[(n*i + 1): (n*(i + 1))]))
  tab <- setnames(tab, old = var[(n*i + 1): (n*(i + 1))], new = new_name[1:n])
  tab$'dimension' <- dimension[i+1]
  data_compil_2 <- rbind(data_compil_2, tab) }

rm(features, var, dimension, new_name, n, i, tab)

