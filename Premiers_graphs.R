dev.off()

graph <- function(dimension, question, titre1, titre2) 
  #ex : graph(dimension = "R2_Eol", question = "Qjug_Eol", 
  #titre1 = "Jugements pour scénarios sans reconnaissance (parc éolien)", 
  #titre2 = "Jugements pour scénarios avec reconnaissance (parc éolien)")
{tab1 <- data[data[[dimension]] == TRUE, ]
tab2 <- data[data[[dimension]] == FALSE, ]

x1 = c(nrow(tab1[tab1[[question]] == "Très juste", ]), 
       nrow(tab1[tab1[[question]] == "Plutôt juste", ]), 
       nrow(tab1[tab1[[question]] == "Plutôt injuste", ]), 
       nrow(tab1[tab1[[question]] == "Très injuste", ]))

x2 = c(nrow(tab2[tab2[[question]] == "Très juste", ]), 
       nrow(tab2[tab2[[question]] == "Plutôt juste", ]), 
       nrow(tab2[tab2[[question]] == "Plutôt injuste", ]), 
       nrow(tab2[tab2[[question]] == "Très injuste", ]))

judgement1 = data.frame(names = c("Très juste","Plutôt juste",
                                  "Plutôt injuste", "Très injuste"), 
                        value = x1)
judgement2 = data.frame(names = c("Très juste","Plutôt juste",
                                  "Plutôt injuste", "Très injuste"), 
                        value = x2)

color <- brewer.pal(4, "Set2")
par(mfrow = c(1, 2))
barplot(height=judgement1$value, names=judgement1$names, 
        col=color, ylim = c(0, 500), main = titre1, 
        xlab = "Jugement", ylab = "Adhesion des répondants")

barplot(height=judgement2$value, names=judgement2$names, 
        col=color, ylim = c(0, 500), main = titre2, 
        xlab = "Jugement", ylab = "Adhesion des répondants")
}


graph("D2_Eol", "Qjug_Eol", 
      "Jugements pour scénarios sans inégalité distributive (parc éolien)", 
      "Jugements pour scénarios avec inégalité distributive (parc éolien)")
