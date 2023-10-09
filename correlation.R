features <- c("sexe", "age_recode", "enfants", "adultes", "diplome", 
              "activité", "travail", "CSP", "revenu", "agglo", "region")

var <- c("nb_ok_RC", "nb_ok_Eol", "nb_ok_Pol", "nb_ok_DPE", "nb_ok_Nat", "moy_ok")

correlation = list()
correlation$'Traitement' = c(var, 'nb_ok')

for (x in features) {
  p_val <- c()
  for (y in var) {
    p_val <- append(p_val, chisq.test(table(data[[x]], data[[y]]))$"p.value")}
  p_val <- append(p_val, chisq.test(table(data_compil$nb_ok, data_compil[[x]]))$"p.value")
  correlation[[x]] = round(p_val, 3)}


correlation = as_tibble(correlation)
x <- features[11]
y <- var[3]
x
table(data_compil$nb_ok, data_compil[[x]])
table(data[[x]], data[[y]])
length(data[[x]])
