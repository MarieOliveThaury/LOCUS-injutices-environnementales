# library
library(ggplot2)
library(ggsignif)
library(ggpval)
library(ggpubr)
#install.packages("ggpval")

# create a dataset
Dimension <- c(rep("Richesse" , 2) , rep("Distribution" , 2), rep("Procédure" , 2), rep("Reconnaissance" , 2))
Alternative <- rep(c("1" , "2") , 2)
Pourcentage = c()
for (i in 1:nrow(statistiques_descriptives_3)) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_3$"Global"[i])
}
Valeurs <- data.frame(Dimension,Alternative,Pourcentage)
rm(i)

# Grouped

# n = nrow(data_compil[data_compil$C2 == "TRUE", ])
# N = 8000 - n
# prop.test(c(table(data_compil[data_compil$C2 == "TRUE", ]$Injuste)), c(n, n), c(table(data_compil[data_compil$C2 == "FALSE", ]$Injuste)/N))$'p.value'
# 
# n = nrow(data_compil[data_compil$D2 == "TRUE", ])
# N = 8000 - n
# prop.test(c(table(data_compil[data_compil$D2 == "TRUE", ]$Injuste)), c(n, n), c(table(data_compil[data_compil$D2 == "FALSE", ]$Injuste)/N))$'p.value'
# 
# n = nrow(data_compil[data_compil$P2 == "TRUE", ])
# N = 8000 - n
# prop.test(c(table(data_compil[data_compil$P2 == "TRUE", ]$Injuste)), c(n, n), c(table(data_compil[data_compil$P2 == "FALSE", ]$Injuste)/N))$'p.value'
# 
# n = nrow(data_compil[data_compil$R2 == "TRUE", ])
# N = 8000 - n
# prop.test(c(table(data_compil[data_compil$R2 == "TRUE", ]$Injuste)), c(n, n), c(table(data_compil[data_compil$R2 == "FALSE", ]$Injuste)/N))$'p.value'


positions <- c("Richesse", "Distribution", "Procédure", "Reconnaissance")

ggplot(Valeurs, aes(y=Pourcentage, x=Dimension)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values=c("#7FFF50", "#FF3333")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22)) +
  geom_bar(aes(fill = Alternative), stat="identity", position="dodge", width=.5, show.legend = FALSE) +
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875, 2.875, 3.875), xend=c(1.125, 2.125, 3.125, 4.125),
                              y=c(0.67, 0.67, 0.71, 0.7), annotation=c("**", " *** ", "  ***  ", "***")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation, textsize=10)) +
  scale_y_continuous(limits=c(0,0.85)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))

##statistiques descriptives
colfunc <- colorRampPalette(c("orange", "blue"))

colfunc(4)

tab <- list()
tab$names = c("0", "1", "2", "3", "4")
tab$values = 0
for (i in 1:nrow(statistiques_descriptives_2)) {
  tab$values[i] = statistiques_descriptives_2$"Espaces naturels"[i]
}
tab = data.frame(tab)

ggplot(tab, aes(y=values, x=names)) + 
  #scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#00FF00", "#7FFF00", "#FFFF00", "#FF7F00", "#FF0000")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22)) +
  geom_bar(aes(fill = tab$names), stat="identity", position="dodge", width=.5, show.legend = FALSE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))


barplot(height=tab$values, names=tab$names, 
        col=color, ylim = c(0, 0.8),
        xlab = "Nombre d'inégalités", ylab = "Pourcentage de situations jugées injustes")
rm(tab, color)



Cadrage <- c(rep("Risques\nclimatiques" , 5) , rep("Parc\néolien" , 5), 
               rep("Pollution\nde l'air" , 5), rep("Performance\nénergétique" , 5),
               rep("Espaces\nnaturels" , 5), rep("Global" , 5))
Age <- rep(c("18-24 ans", "25-34 ans", "35-49 ans", "50-64 ans", "65+") , 6)
Pourcentage = c()
for (i in c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_4[[i]])
}

tab <- data.frame(Cadrage,Age,Pourcentage)

positions = c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tab, aes(y=Pourcentage, x=Cadrage)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#FFD700", "#DE6C6D", "#BA34C1", "#8850E8", "#40E0D0")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22), 
        legend.text = element_text(size=22), 
        legend.title = element_text(size=22), 
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  geom_bar(aes(fill = Age), stat="identity", position="dodge", width=.5, show.legend = TRUE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))




Cadrage <- c(rep("Risques\nclimatiques" , 2) , rep("Parc\néolien" , 2), 
             rep("Pollution\nde l'air" , 2), rep("Performance\nénergétique" , 2),
             rep("Espaces\nnaturels" , 2), rep("Global" , 2))
Sexe <- rep(c("Féminin", "Masculin"), 6)
Pourcentage = c()
for (i in c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_5[[i]])
}

tab <- data.frame(Cadrage,Sexe,Pourcentage)

positions = c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tab, aes(y=Pourcentage, x=Cadrage)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#FFD700", "#8850E8")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22), 
        legend.text = element_text(size=22), 
        legend.title = element_text(size=22), 
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  geom_bar(aes(fill = Sexe), stat="identity", position="dodge", width=.5, show.legend = TRUE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))




Cadrage <- c(rep("Risques\nclimatiques" , 2) , rep("Parc\néolien" , 2), 
             rep("Pollution\nde l'air" , 2), rep("Performance\nénergétique" , 2),
             rep("Espaces\nnaturels" , 2), rep("Global" , 2))
Fonctionnaire <- rep(c("Oui", "Non"), 6)
Pourcentage = c()
for (i in c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_6[[i]])
}

tab <- data.frame(Cadrage,Fonctionnaire,Pourcentage)

positions = c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tab, aes(y=Pourcentage, x=Cadrage)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#DE6C6D", "#40E0D0")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22), 
        legend.text = element_text(size=22), 
        legend.title = element_text(size=22), 
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  geom_bar(aes(fill = Fonctionnaire), stat="identity", position="dodge", width=.5, show.legend = TRUE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))





Cadrage <- c(rep("Risques\nclimatiques" , 3) , rep("Parc\néolien" , 3), 
             rep("Pollution\nde l'air" , 3), rep("Performance\nénergétique" , 3),
             rep("Espaces\nnaturels" , 3), rep("Global" , 3))
CSP <- rep(c("CSP+", "CPS-", "INACTIF"), 6)
Pourcentage = c()
for (i in c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_7[[i]])
}

tab <- data.frame(Cadrage,CSP,Pourcentage)

positions = c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tab, aes(y=Pourcentage, x=Cadrage)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#DE6C6D", "gold", "#40E0D0")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22), 
        legend.text = element_text(size=22), 
        legend.title = element_text(size=22), 
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  geom_bar(aes(fill = CSP), stat="identity", position="dodge", width=.5, show.legend = TRUE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))



Cadrage <- c(rep("Risques\nclimatiques" , 4) , rep("Parc\néolien" , 4), 
             rep("Pollution\nde l'air" , 4), rep("Performance\nénergétique" , 4),
             rep("Espaces\nnaturels" , 4), rep("Global" , 4))
Diplome <- rep(c('Diplôme du brevet, équivalent ou moins', 'Diplôme du baccalauréat',
             "Premier cycle de l'enseignement supérieur", 
             "Deuxième cycle de l'enseignement supérieur ou plus"), 6)
Pourcentage = c()
for (i in c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")) {
  Pourcentage = append(Pourcentage, statistiques_descriptives_8[[i]])
}

tab <- data.frame(Cadrage,Diplome,Pourcentage)

positions = c('Risques\nclimatiques', 'Parc\néolien', "Pollution\nde l'air", "Performance\nénergétique", "Espaces\nnaturels", "Global")
ggplot(tab, aes(y=Pourcentage, x=Cadrage)) + 
  scale_x_discrete(limits = positions) +
  scale_fill_manual(values = c("#FFA500", "#BFB845", "#7FCC8A", "#40E0D0")) + 
  labs(x = " ") + 
  labs(y = "Pourcentage de situations jugées injustes\n") +
  theme(axis.title.x = element_text(size = 28), 
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 22), 
        legend.text = element_text(size=10), 
        legend.title = element_text(size=10), 
        #legend.key.height= unit(1, 'cm'),
        #legend.key.width= unit(1, 'cm'), 
        legend.position="bottom") +
  geom_bar(aes(fill = Diplome), stat="identity", position="dodge", width=.5, show.legend = TRUE) +
  scale_y_continuous(limits=c(0,1)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))

