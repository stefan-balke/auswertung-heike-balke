library(xtable)
source('load_data.R')

# ---------------------------------------------------------------------------------------------------
# Chi**2 Vierfeldertest
# ---------------------------------------------------------------------------------------------------
tbl <- table(data$PE, data$bmi_gruppe)
# normalgewichtige gegen übergewichtig + adipös
ctbl = cbind(tbl[,2], tbl[,3] + tbl[,4])
results <- chisq.test(ctbl)

tbl <- table(data$PE, data$alter_gruppe)
ctbl = cbind(tbl[,1] + tbl[,1], tbl[,3])
results <- chisq.test(t(ctbl))

tbl <- table(data$PE, factor(data$hypertonie))
results <- chisq.test(tbl)

tbl <- table(data$PE, factor(data$mehrlinge))
results <- chisq.test(tbl)

# ---------------------------------------------------------------------------------------------------
# Binary Logit Regression
# Abhängige Variable ist binär
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# Hypothese 1:
# besteht ein Zusammenhang zwischen PE, Alter oder BMI?
# Hypothese 2:
# besteht ein Zusammenhang zwischen PE und vorher bestehenden hypertensiven Erkrankungen?
# ---------------------------------------------------------------------------------------------------
glm.fit01 <- glm(PE ~ factor(alter_gruppe) * bmi * factor(hyper_bestehend) + factor(znhypertonie) + factor(mehrlinge) ,
                 data=data, family=binomial(logit))
summary(glm.fit01)
xtable(glm.fit01)

# odds ratios and 95% CI
exp(cbind(OR = coef(glm.fit01), confint(glm.fit01, level = 0.95)))
