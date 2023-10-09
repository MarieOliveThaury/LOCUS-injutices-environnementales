##1
combi <- c( "C1 – D1 – P1 – R1", 
          "C1 – D1 – P1 – R2", "C1 – D1 – P2 – R1", "C1 – D2 – P1 – R1", "C2 – D1 – P1 – R1", 
          "C2 – D2 – P1 – R1", "C1 – D1 – P2 – R2", "C1 – D2 – P2 – R1", "C2 – D1 – P1 – R2", 
          "C2 – D1 – P2 – R1", "C1 – D2 – P1 – R2", 
          "C1 – D2 – P2 – R2", "C2 – D1 – P2 – R2", "C2 – D2 – P1 – R2", "C2 – D2 – P2 – R1", 
          "C2 – D2 – P2 – R2") 

statistiques_descriptives_1 = list()
statistiques_descriptives_1$'Combinaison' = combi

dimension <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat", 
               "combi_RC", "combi_Eol", "combi_Pol", "combi_DPE", "combi_Nat")

for (i in 1:5) {
  prop_injuste = c()
  for (j in combi) {
    prop_injuste  <- append(prop_injuste, round(mean(data[data[[dimension[i+5]]] == j, ][[dimension[i]]]),3))
  }
  
  statistiques_descriptives_1[[dimension[i]]] <- prop_injuste 
}

prop_injuste = c()
for (j in combi) {
  prop_injuste  <- append(prop_injuste, round(mean((data_compil[data_compil$"combi" == j, ])$'Injuste'), 3))}
statistiques_descriptives_1$'Injuste' <- prop_injuste 

statistiques_descriptives_1 <- as_tibble(statistiques_descriptives_1)
statistiques_descriptives_1 <- statistiques_descriptives_1 %>% rename("Risques climatiques" = "Injuste_RC",
                                                                      "Parc éolien" = "Injuste_Eol", 
                                                                      "Pollution de l'air" = "Injuste_Pol",
                                                                      "Performance énergétique" = "Injuste_DPE",
                                                                      "Espaces naturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")

#css.cell <- matrix('', 16, 7)
#css.cell[1,] <- 'background-color: lime'
#for (i in 2:5) {css.cell[i,] <- 'background-color: yellow'}
#for (i in 6:11) {css.cell[i,] <- 'background-color: orange'}
#for (i in 12:15) {css.cell[i,] <- 'background-color: coral'}
#css.cell[16,] <- 'background-color: red'

#htmlTable(statistiques_descriptives_1, css.cell = css.cell)


##2
count <- c(0, 1, 2, 3, 4)
statistiques_descriptives_2 = list()
statistiques_descriptives_2$"Nombre d'inégalités" = count

dimension <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat", 
               "count_RC", "count_Eol", "count_Pol", "count_DPE", "count_Nat")

for (i in 1:5) {
  prop_injuste = c()
  for (j in count) {
    prop_injuste  <- append(prop_injuste, round(mean(data[data[[dimension[i+5]]] == j, ][[dimension[i]]]),3))
  }
  
  statistiques_descriptives_2[[dimension[i]]] <- prop_injuste 
}

prop_injuste = c()
for (j in count) {
  prop_injuste  <- append(prop_injuste, round(mean((data_compil[data_compil$"count" == j, ])$'Injuste'), 3))}
statistiques_descriptives_2$'Injuste' <- prop_injuste 
statistiques_descriptives_2 <- as_tibble(statistiques_descriptives_2)
statistiques_descriptives_2 <- statistiques_descriptives_2 %>% rename("Risques climatiques" = "Injuste_RC",
                                                                      "Parc éolien" = "Injuste_Eol", 
                                                                      "Pollution de l'air" = "Injuste_Pol",
                                                                      "Performance énergétique" = "Injuste_DPE",
                                                                      "Espaces naturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")

#css.cell <- matrix('', 5, 7)
#style <- c('background-color: lime', 'background-color: yellow', 
           #'background-color: orange', 'background-color: coral', 
           #'background-color: red')
#for (i in 1:5) {css.cell[i,] <- style[i]}
#htmlTable(statistiques_descriptives_2, css.cell = css.cell)


##3
dim <- c("C1", "C2", "D1", "D2", "P1", "P2", "R1", "R2")
statistiques_descriptives_3 = list()
statistiques_descriptives_3$"Dimension" = dim

dimension <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat", 
               "C2_RC", "D2_RC", "P2_RC", "R2_RC", "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
               "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol", "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE", 
               "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:4) {
    prop  <- round(mean(data[data[[dimension[5 + i*4 + j]]] == FALSE, ][[dimension[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
    prop  <- round(mean(data[data[[dimension[5 + i*4 + j]]] == TRUE, ][[dimension[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_3[[dimension[i+1]]] <- prop_injuste 
}


dimension <- c("C2", "D2", "P2", "R2")
prop_injuste = c()
for (j in 1:4) {
  prop  <- round(mean(data_compil[data_compil[[dimension[j]]] == FALSE, ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)
  prop  <- round(mean(data_compil[data_compil[[dimension[j]]] == TRUE, ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_3$'Injuste' <- prop_injuste 

statistiques_descriptives_3 <- as_tibble(statistiques_descriptives_3)
statistiques_descriptives_3 <- statistiques_descriptives_3 %>% rename("Risques climatiques" = "Injuste_RC",
                                                                      "Parc éolien" = "Injuste_Eol", 
                                                                      "Pollution de l'air" = "Injuste_Pol",
                                                                      "Performance énergétique" = "Injuste_DPE",
                                                                      "Espaces naturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")

 
#css.cell <- matrix('', 8, 7)
#style <- c('background-color: yellow', 'background-color: aqua',
           #'background-color: pink', 'background-color: lime')
#for (i in 0:3) {
  #css.cell[2*i + 1,] <- style[i+1]
  #css.cell[2*i + 2,] <- style[i+1]}
#htmlTable(statistiques_descriptives_3, css.cell = css.cell)


rm(combi, count, dim, dimension, i, j, prop, prop_injuste, style, css.cell)


##4 : age
var <- c("18-24 ans", "25-34 ans", "35-49 ans", "50-64 ans", "65+")
statistiques_descriptives_4 = list()
statistiques_descriptives_4$"Dimension" = var

cadrage <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:5) {
    prop  <- round(mean(data[data$age_recode == var[j], ][[cadrage[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_4[[cadrage[i+1]]] <- prop_injuste 
}


prop_injuste = c()
for (j in 1:5) {
  prop  <- round(mean(data_compil[data_compil$age_recode == var[j], ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_4$'Injuste' <- prop_injuste

statistiques_descriptives_4 <- as_tibble(statistiques_descriptives_4)
statistiques_descriptives_4 <- statistiques_descriptives_4 %>% rename("Risques\nclimatiques" = "Injuste_RC",
                                                                      "Parc\néolien" = "Injuste_Eol", 
                                                                      "Pollution\nde l'air" = "Injuste_Pol",
                                                                      "Performance\nénergétique" = "Injuste_DPE",
                                                                      "Espaces\nnaturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")
rm(var, cadrage, i, j, prop, prop_injuste)

##5 : sexe
var <- c("Féminin", "Masculin")
statistiques_descriptives_5 = list()
statistiques_descriptives_5$"Dimension" = var

cadrage <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:2) {
    prop  <- round(mean(data[data$sexe == var[j], ][[cadrage[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_5[[cadrage[i+1]]] <- prop_injuste 
}


prop_injuste = c()
for (j in 1:2) {
  prop  <- round(mean(data_compil[data_compil$sexe == var[j], ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_5$'Injuste' <- prop_injuste

statistiques_descriptives_5 <- as_tibble(statistiques_descriptives_5)
statistiques_descriptives_5 <- statistiques_descriptives_5 %>% rename("Risques\nclimatiques" = "Injuste_RC",
                                                                      "Parc\néolien" = "Injuste_Eol", 
                                                                      "Pollution\nde l'air" = "Injuste_Pol",
                                                                      "Performance\nénergétique" = "Injuste_DPE",
                                                                      "Espaces\nnaturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")
rm(var, cadrage, i, j, prop, prop_injuste)

##6 : fonctionnaire
var <- c("Oui", "Non")
statistiques_descriptives_6 = list()
statistiques_descriptives_6$"Dimension" = var

cadrage <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:2) {
    prop  <- round(mean(data[data$public == var[j], ][[cadrage[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_6[[cadrage[i+1]]] <- prop_injuste 
}


prop_injuste = c()
for (j in 1:2) {
  prop  <- round(mean(data_compil[data_compil$public == var[j], ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_6$'Injuste' <- prop_injuste

statistiques_descriptives_6 <- as_tibble(statistiques_descriptives_6)
statistiques_descriptives_6 <- statistiques_descriptives_6 %>% rename("Risques\nclimatiques" = "Injuste_RC",
                                                                      "Parc\néolien" = "Injuste_Eol", 
                                                                      "Pollution\nde l'air" = "Injuste_Pol",
                                                                      "Performance\nénergétique" = "Injuste_DPE",
                                                                      "Espaces\nnaturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")
rm(var, cadrage, i, j, prop, prop_injuste)

##7 : CSP
var <- c("CSP+", "CSP-", "INACTIF")
statistiques_descriptives_7 = list()
statistiques_descriptives_7$"Dimension" = var

cadrage <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:3) {
    prop  <- round(mean(data[data$CSP == var[j], ][[cadrage[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_7[[cadrage[i+1]]] <- prop_injuste 
}


prop_injuste = c()
for (j in 1:3) {
  prop  <- round(mean(data_compil[data_compil$CSP == var[j], ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_7$'Injuste' <- prop_injuste

statistiques_descriptives_7 <- as_tibble(statistiques_descriptives_7)
statistiques_descriptives_7 <- statistiques_descriptives_7 %>% rename("Risques\nclimatiques" = "Injuste_RC",
                                                                      "Parc\néolien" = "Injuste_Eol", 
                                                                      "Pollution\nde l'air" = "Injuste_Pol",
                                                                      "Performance\nénergétique" = "Injuste_DPE",
                                                                      "Espaces\nnaturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")

rm(var, cadrage, i, j, prop, prop_injuste)

##8 : diplome 
var <- c('Diplôme du brevet,\néquivalent ou moins', 'Diplôme du baccalauréat',
         "Premier cycle de\nl'enseignement supérieur", 
         "Deuxième cycle de\nl'enseignement supérieur\nou plus")

statistiques_descriptives_8 = list()
statistiques_descriptives_8$"Variable" = var

cadrage <- c("Injuste_RC", "Injuste_Eol", "Injuste_Pol", "Injuste_DPE", "Injuste_Nat")

for (i in 0:4) {
  prop_injuste = c()
  for (j in 1:4) {
    prop  <- round(mean(data[data$diplome_recode == var[j], ][[cadrage[i+1]]]),3)
    prop_injuste <- append(prop_injuste, prop)
  }
  statistiques_descriptives_8[[dimension[i+1]]] <- prop_injuste 
}


prop_injuste = c()
for (j in 1:4) {
  prop  <- round(mean(data_compil[data_compil$diplome_recode == var[j], ]$'Injuste'),3)
  prop_injuste <- append(prop_injuste, prop)}
statistiques_descriptives_8$'Injuste' <- prop_injuste

statistiques_descriptives_8 <- as_tibble(statistiques_descriptives_8)
statistiques_descriptives_8 <- statistiques_descriptives_8 %>% rename("Risques\nclimatiques" = "Injuste_RC",
                                                                      "Parc\néolien" = "Injuste_Eol", 
                                                                      "Pollution\nde l'air" = "Injuste_Pol",
                                                                      "Performance\nénergétique" = "Injuste_DPE",
                                                                      "Espaces\nnaturels" = "Injuste_Nat", 
                                                                      "Global" = "Injuste")

