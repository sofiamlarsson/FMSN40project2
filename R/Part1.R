library(ggplot2)
library(xtable)
data <- read.delim("Data/hospital.txt", sep = ";")

data$hosp_cat <- factor(data$hosp,
                          levels = c(0, 1),
                          labels = c("0 days", "1+ days"))

ggplot(data, aes(x = age,y = hosp)) + geom_point(size = 1)

#### Modelling a logistic regression ####
t1 = table(data$health, data$hosp_cat)
print(xtable(t1, type = "latex"), file = "table1.tex")

t2 = prop.table(table(data$health, data$hosp_cat), margin = 1)
print(xtable(t2, type = "latex"), file = "filename2.tex")

data$health <- factor(data$health,
                        levels = c(1, 2, 3),
                        labels = c("Good", "Bad", "Somewhere in between"))

model.health <- glm(hosp ~ health, family = "binomial", data = data)
# Good is already the reference variable

# beta and confidence intervals
model.health$coefficients
(ci.beta <- confint(model.health))

# odds ratio and confidence intervals
(oddsratio <- exp(model.health$coefficients))
(or.ci <- exp(ci.beta))

#Pseudo R2, AIC, BIC
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data)
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, model.health)
aic <- AIC(nullmodel, model.health)
(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(model.health)[1])
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
collect.AIC

print(xtable(collect.AIC, type = "latex"), file = "R2_AIC_BIC.tex")

# Global likelihood ratio test
(anova.null.health <- anova(nullmodel, model.health))
(D_diff <- anova.null.health$Deviance[2])
(df_diff <- anova.null.health$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Prediction ?????
(data.pred <- cbind(
  data$health,
  phat = predict(model.health, type = "response")
  conf = predic))

#### Hosp and age
ggplot(data, aes(x = age, y = hosp)) + geom_point(size = 0.5) + 
  geom_smooth(size = 0.5) +  
  labs(title = "1+ hospital days (=1) or 0 hospital days (=0) vs age") +
  theme(text = element_text(size = 10))

model.age <- glm(hosp ~ age, family = "binomial", data)
beta <- model.age$coefficients
betas <- data.frame(beta)
betas$expbeta <- c(exp(beta))
betas$ci <- confint(model.age)
betas$expci <- exp(confint(model.age))
betas
