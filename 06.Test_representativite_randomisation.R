##Tests de la représentativité de l'échantillon :
##H0 : la proportion observée est égale à la proportion attendue 

meta_representativite = list()

n = nrow(data)
meta_representativite[['sexe']] = prop.test(c(table(data$'sexe')), c(n, n), c(0.52, 0.48))$'p.value'
meta_representativite[['age']] = prop.test(c(table(data$'age_recode')), c(n, n, n, n, n), c(0.10, 0.15, 0.24, 0.24, 0.26))$'p.value'
meta_representativite[['CSP']] = prop.test(c(table(data$'CSP')), c(n, n, n), c(0.32, 0.28, 0.40))$'p.value'
meta_representativite[['UDA5']] = prop.test(c(table(data$'UDA5')), c(n, n, n, n, n), c(0.23, 0.23, 0.19, 0.24, 0.11))$'p.value'
meta_representativite = as_tibble(meta_representativite)

#on ne rejette jamais H0 i.e. l'échantillon ne présente pas des quotas 
#significativement différents de ceux présents dans la population.

rm(n)




##Tests de la randomisation de l'expérience :

##on considère les quatre dimensions de chaque scénarios comme des traitrements : 

##pour chaque traitement, on teste si l'assignation au traitement est corrélée 
#ou non aux variables socio-économiques des participants

##on utilise un test du chi-2 et non un t-test (les variables sont catégorielles et non continues)
##l'hypothèse nulle est : "Les distributions sont indépendantes" 

features = c("sexe", "age_recode", "enfants", "adultes", 
             "diplome", "activité", "travail", "CSP", "public", "revenu", 
             "agglo", "region", "departement", "UDA5", "UDA9")


var <- c("C2_RC", "D2_RC", "P2_RC", "R2_RC", 
         "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
         "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", 
         "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", 
         "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat", 
         "C2", "D2", "P2", "R2")

meta_randomisation = list()
meta_randomisation$'Traitement' = var

for (x in features) {p_val <- c()
  for (y in var[1:20]) {
    p_val <- append(p_val, chisq.test(table(data[[x]], data[[y]]))$"p.value")}
  for (y in var[21:24]) {
    p_val <- append(p_val, chisq.test(table(data_compil[[x]], data_compil[[y]]))$"p.value")}
  meta_randomisation[[x]] = round(p_val, 3)
}


#for (x in features) {p_val <- c()
#for (y in var[1:20]) {
  #p_val <- append(p_val, fisher.test(table(data[[x]], data[[y]]), hybrid = TRUE)$"p.value")}
#for (y in var[21:24]) {
  #p_val <- append(p_val, fisher.test(table(data_compil[[x]], data_compil[[y]]), hybrid = TRUE)$"p.value")}
#meta_randomisation[[x]] = p_val
#}
meta_randomisation = as_tibble(meta_randomisation)

style <- 'background-color: coral'
n = ncol(meta_randomisation)
N = nrow(meta_randomisation)
css.cell <- matrix('', N, n )

for (i in 1:N) {for (j in 1:n) {if (meta_randomisation[i,j] < 0.05) {
  css.cell[i,j] <- style}}}

htmlTable(meta_randomisation, css.cell = css.cell)

rm(features, p_val, var, style, x, y, i, j, css.cell, n, N)
