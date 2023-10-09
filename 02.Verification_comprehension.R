meta_comprehension = list()
meta_comprehension$'Var' = c("Tx de reussite C", "Tx de reussite D", "Tx de reussite P", 
               "Tx de reussite R", "Nbre moyen bonne réponse")
  
new_var <- c("C_verif_RC", "D_verif_RC", "P_verif_RC", "R_verif_RC", "nb_ok_RC",
             "C_verif_Eol", "D_verif_Eol", "P_verif_Eol", "R_verif_Eol", "nb_ok_Eol",
             "C_verif_Pol", "D_verif_Pol", "P_verif_Pol", "R_verif_Pol", "nb_ok_Pol", 
             "C_verif_DPE", "D_verif_DPE", "P_verif_DPE", "R_verif_DPE", "nb_ok_DPE",
             "C_verif_Nat", "D_verif_Nat", "P_verif_Nat", "R_verif_Nat", "nb_ok_Nat")

new_var_meta <- c("Verif_RC", "Verif_Eol", "Verif_Pol","Verif_DPE","Verif_Nat")

var <- c("Qcompr_RC_C", "Qcompr_RC_D", "Qcompr_RC_P", 
         "Qcompr_RC_R", "C2_RC", "D2_RC", "P2_RC", "R2_RC", 
         "Qcompr_Eol_C", "Qcompr_Eol_D", "Qcompr_Eol_P", 
         "Qcompr_Eol_R", "C2_Eol", "D2_Eol", "P2_Eol", "R2_Eol", 
         "Qcompr_Pol_C", "Qcompr_Pol_D", "Qcompr_Pol_P", 
         "Qcompr_Pol_R", "C2_Pol", "D2_Pol", "P2_Pol", "R2_Pol",
         "Qcompr_DPE_C", "Qcompr_DPE_D", "Qcompr_DPE_P", 
         "Qcompr_DPE_R", "C2_DPE", "D2_DPE", "P2_DPE", "R2_DPE",
         "Qcompr_Nat_C", "Qcompr_Nat_D", "Qcompr_Nat_P", 
         "Qcompr_Nat_R", "C2_Nat", "D2_Nat", "P2_Nat", "R2_Nat")
         

exp_answer <- c("Oui", "Oui", "Oui", "Non", 
                "Oui", "Oui", "Non", "Non", 
                "Oui", "Oui", "Non", "Non",
                "Oui", "Oui", "Non", "Non",
                "Oui", "Oui", "Non", "Non")


for (i in 0:4) { #i is the framing 
  tx_reussite = c()
  for (j in 1:4) { #j is the dimension of the framing 
    #answer reveals what the participant believes the situation to be : 
    answer <- exp_answer[4*i + j] == data[[var[8*i + j]]] 
    
    #here we check if the participant's belief corresponds to the effective situation : 
    data[[new_var[5*i + j]]] <- answer == data[[var[8*i + j + 4]]]
    
    #we compute for each question of the framing, the frequency of good answers amoung participants : 
    tx_reussite <- append(tx_reussite, mean(data[[new_var[5*i + j]]])) 
  }
  
  #for each framing, we compute how many answers each participant had true 
  data[[new_var[(5*(i + 1))]]] <- rowSums(data[new_var[(5*i + 1) : (5*i + 4)]], na.rm=TRUE)
  
  #we compute how many answers participants have right on average for a given framing
  tx_reussite = append(tx_reussite, mean(data[[new_var[(5*(i + 1))]]]))
  meta_comprehension[[new_var_meta[i+1]]] = tx_reussite
}



##Global : 
ok = c("nb_ok_RC", "nb_ok_Eol", "nb_ok_Pol", "nb_ok_DPE", 'nb_ok_Nat')
data$"moy_ok" <- rowMeans(data[ok],na.rm=TRUE)

meta_comprehension = as_tibble(meta_comprehension)
meta_comprehension$"Verif" = rowMeans(meta_comprehension[2:6],na.rm=TRUE)
str(meta_comprehension)

rm(var, new_var, new_var_meta, tx_reussite, i, j, answer, exp_answer, ok)


##Graphs 

graph_verif = function(x, title) {
  color <- brewer.pal(4, "Set2")
  tx_reussite = data.frame(names = c("Richesse","Distribution","Procédure", "Reconnaissance"), value = x)
  barplot(height=tx_reussite$value, names=tx_reussite$names, 
          col=color, ylim = c(0, 1), main = title, 
          xlab = "Dimension de justice", ylab = "Taux de réussite", 
          font.lab=6)
}

graph_verif(meta_comprehension$'Verif_RC'[1:4], "Taux de réussite (Risques Climatiques)")
graph_verif(meta_comprehension$'Verif_Eol'[1:4], "Taux de réussite (Parc eolien)")
graph_verif(meta_comprehension$'Verif_Pol'[1:4], "Taux de réussite (Pollution de l'air)")
graph_verif(meta_comprehension$'Verif_DPE'[1:4], "Taux de réussite (Performance énergétique)")
graph_verif(meta_comprehension$'Verif_Nat'[1:4], "Taux de réussite (Espaces naturels)")
graph_verif(meta_comprehension$'Verif'[1:4], "Taux de réussite (Global)")


class <- c("Richesse","Distribution","Procédure", "Reconnaissance")
val <- meta_comprehension$'Verif'[1:4]
Valeurs <- data.frame(class, val)
ggplot(Valeurs, aes(x = class, y = val)) + 
  geom_bar(aes(fill = class), stat="identity", position="dodge", width=.5, show.legend = FALSE)+
  scale_x_discrete(limits = class) +
  scale_fill_manual(values=c("turquoise", "orange", "purple", "green")) + 
  labs(x = " ") + 
  labs(y = "Taux de réussite") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 26),
        axis.title.y = element_text(size = 28)) + 
  scale_y_continuous(limits=c(0,1))



##another graph
color <- brewer.pal(7, "Set2")
x = c()
for (y in meta_comprehension[5 , 2:7]) {x <- append(x, y)}
tx_reussite = data.frame(names = c("Risques\nclimatiques","Parc\néolien","Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global"), value = x)
barplot(height=tx_reussite$value, names=tx_reussite$names, 
        col=color, ylim = c(0, 3),
        xlab = "Cadrage", ylab = "Nombre moyen de bonnes réponses (/4)")
rm(tx_reussite, x, y, color)

position = c("Risques\nclimatiques","Parc\néolien","Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tx_reussite, aes(x = names, y = value)) + 
  geom_bar(aes(fill = tx_reussite$names), stat="identity", position="dodge", width=.5, show.legend = FALSE)+
  scale_x_discrete(limits = position) +
  scale_fill_manual(values = c("green", "gold", "turquoise", "pink", "purple", "red")) + 
  labs(x = " ") + 
  labs(y = "Nombre moyen de bonnes réponses ( /4)") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 22))
  #scale_y_continuous(limits=c(0,1))
