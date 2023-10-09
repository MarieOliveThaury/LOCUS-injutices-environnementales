library(lme4)

fit <- polr(Qjug ~ C2 + D2 + P2 + R2  
            + cumul_D + cumul_P + cumul_R + 
              cumul_DP + cumul_DR + cumul_PR, 
            data = data_compil, Hess=TRUE)

fit <- polr(Qjug_Nat ~ C2_Nat + D2_Nat + P2_Nat + R2_Nat, 
            data = data, Hess=TRUE)

ctable <- coef(summary(fit))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, 3)
ctable <- cbind("Coef" = round(ctable[, "Value"], 3), 
                "Std. Error" = round(ctable[, "Std. Error"], 3), 
                "t value" = round(ctable[, "t value"], 3), 
                "p value" = p)
ci <- confint(fit)
ci
ctable
round(exp(coef(fit)), 3)



##checking the proportional odds assumption 
data_ <- data %>%
  mutate(# Y1
    Y1 = fct_collapse(Qjug_RC,
                      ">Très juste"  = c("Plutôt juste", "Plutôt injuste", "Très injuste"),
                      "<=Très juste" = "Très juste"),
    Y1 = relevel(Y1, ref = "<=Très juste"),
    # Y2
    Y2 = fct_collapse(Qjug_RC,
                      ">Plutôt juste"  = c("Plutôt injuste", "Très injuste"),
                      "<=Plutôt juste" = c("Plutôt juste", "Très juste")),
    Y2 = relevel(Y2, ref = "<=Plutôt juste"),
    
    # Y3
    Y3 = fct_collapse(Qjug_RC,
                      ">Plutôt injuste"  = "Très injuste",
                      "<=Plutôt injuste" = c("Plutôt injuste", "Plutôt juste", "Très juste")),
    Y3 = relevel(Y3, ref = "<=Plutôt injuste"))

# Check derivations
table(data_$Qjug_RC, data_$Y3, exclude = F)

fit.ordinal <- MASS::polr(Qjug_RC ~ C2_RC + D2_RC + P2_RC + R2_RC,
                          data = data_,
                          Hess= T)

fit.binary1 <- glm(Y1 ~ C2_RC + D2_RC + P2_RC + R2_RC,
                   family = binomial, data = data_)

fit.binary2 <- glm(Y2 ~ C2_RC + D2_RC + P2_RC + R2_RC,
                   family = binomial, data = data_)


fit.binary3 <- glm(Y3 ~ C2_RC + D2_RC + P2_RC + R2_RC,
                   family = binomial, data = data_)

cbind(
  "binary 1" = fit.binary1$coefficients[-1],
  "binary 2" = fit.binary2$coefficients[-1], 
  "binary 3" = fit.binary3$coefficients[-1])

fit.ordinal




##testing other packages 
fit.ordinal <- vglm(Qjug_Pol ~ C2_Pol + D2_Eol + P2_Eol + R2_Eol, family=cumulative(parallel=TRUE), 
                    data = data)
ctable <- coef(summary(fit.ordinal))
ctable
fit.ordinal

fit.adjacent <- vglm(Qjug_RC ~ C2_RC + D2_RC + P2_RC + R2_RC, family=acat(parallel=TRUE), 
                     data = data)
ctable <- coef(summary(fit.adjacent))
ctable
fit.adjacent

fit.multilevel <- glmer(Qjug_RC ~ (C2_RC|age_recode) + D2_RC + P2_RC + R2_RC, family=cumulative(parallel=TRUE), 
                     data = data)
ctable <- coef(summary(fit.multilevel))
ctable
fit.multilevel
?glmer
fit.lrm <- lrm(Qjug_Eol ~ C2_Eol + D2_Eol + P2_Eol + R2_Eol, data = data)
fit.lrm

fit.lrm <- lrm(Qjug_Eol ~ C2_Eol + D2_Eol + P2_Eol + R2_Eol, data = data)
fit.lrm


fit.oglmx <- ologit.reg(Qjug_Pol ~ C2_Pol + D2_Pol + P2_Pol + R2_Pol, data = data)
summary(fit.oglmx)

fit.oglmx <- ologit.reg(Qjug ~ C2 + D2 + P2 + R2, data = data_compil, robust = TRUE)
summary(fit.oglmx)

fit.linear <- lm(Qjug ~ C2 + D2 + P2 + R2, data = data_compil)
test <- coeftest(fit.linear, vcov = vcovCL, cluster = ~ID)
test
summary(fit.linear)

##Resumé : 
##polr : cumulative ordinal category regression 
##glm binomial : classical logistic regression with only 1/0 categories
##vglm cumultative : same as polr
##vglm adjacent : adjacent category regression
##lmr : linear regression model : seems to be the same as polr
##oglmx : pareil 


###keeping only people who answered well
tab1 <- data_compil[data_compil$'nb_ok' == 4, ]
fit <- polr(Qjug ~ C2 + D2 + P2 + R2 +
              cumul_D + cumul_P + cumul_R +
              cumul_DP + cumul_DR + cumul_PR,
            data = data, Hess=TRUE)

ctable <- coef(summary(fit))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, 3)
ctable <- cbind("Coef" = round(ctable[, "Value"], 3), 
                "Std. Error" = round(ctable[, "Std. Error"], 3), 
                "t value" = round(ctable[, "t value"], 3), 
                "p value" = p)
ctable


meta_representativite_2 = list()

n = nrow(tab1)
meta_representativite_2$'travail' = prop.test(c(table(tab1$'travail')), c(n, n, n, n, n, n, n), c(table(data_compil$'travail'))/8000)$'p.value'
meta_representativite_2$'revenu' = prop.test(c(table(tab1$'revenu')), c(n, n, n, n, n, n, n, n, n, n), c(table(data_compil$'revenu'))/8000)$'p.value'
meta_representativite_2 = as_tibble(meta_representativite_2)


##analyses croisées : 
tab2 <- data[data$diplome %in% c("École primaire ou aucun", "Brevet", "CAP ou BEP"), ]
fit <- polr(Qjug_Eol ~ C2_Eol + D2_Eol + P2_Eol + R2_Eol,
            #+ cumul_D + cumul_P + cumul_R, 
            data = tab2, Hess=TRUE)

ctable <- coef(summary(fit))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, 3)
ctable <- cbind("Coef" = round(ctable[, "Value"], 3), 
                "Std. Error" = round(ctable[, "Std. Error"], 3), 
                "t value" = round(ctable[, "t value"], 3), 
                "p value" = p)
ctable

meta_representativite_2 = list()

n = nrow(tab2)
meta_representativite_2[['revenu']] = prop.test(c(table(tab2$'revenu')), c(n, n, n, n, n, n, n, n, n), c(table(data$'revenu'))/1600)$'p.value'
meta_representativite_2[['travail']] = prop.test(c(table(tab2$'travail')), c(n, n, n, n, n, n, n), c(table(data$'travail'))/1600)$'p.value'
meta_representativite_2 = as_tibble(meta_representativite_2)


###################

models <-  data_compil %>% group_by(public) %>% do(fit = polr(Qjug ~ C2 + D2 + P2 + R2,
                                                       data = ., Hess=TRUE))

for (x in models$fit) {
  ctable <- coef(summary(x))
  p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, 3)
  ctable <- cbind("Coef" = round(ctable[, "Value"], 3), "Std. Error" = round(ctable[, "Std. Error"], 3), 
                  "t value" = round(ctable[, "t value"], 3), "p value" = p)
  print(ctable)}

levels(data$public)
