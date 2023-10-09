data_compil$'ordre2' <- data_compil$'ordre' * data_compil$'ordre'
data_compil$time <- data_compil$time/mean(data_compil$time)
fit <- lm(nb_ok ~ time + ordre + ordre2,
          #+ cadrage + time + C2 + D2 + P2 + R2, 
          data=data_compil)
fit <- glm(Verif ~ ordre + Qcompr_ordre + dimension, data=data_compil_2, family = "binomial")
fit <- glm(Verif ~ ordre + ordre2 + Qcompr_ordre + dimension, data=data_compil_2, family = "binomial")
#summary(fit)
test <- coeftest(fit, vcov = vcovCL, cluster = ~ID)
test


data$'ordre_Nat2' <- data$'ordre_Nat' * data$'ordre_Nat'
data$time <- data$time/mean(data$time)
fit <- lm(nb_ok_Nat ~ ordre_Nat + ordre_Nat2 + time,  
            # + revenu + CSP + sexe + age + travail + activité + diplome + UDA5, 
          data=data)
summary(fit)
?glm

str(data_compil$CSP)
data_compil$cadrage <- factor(data_compil$cadrage)
library(xtable)
