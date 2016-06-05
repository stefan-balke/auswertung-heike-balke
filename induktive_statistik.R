source('load_data.R')

# ---------------------------------------------------------------------------------------------------
# Binary Logit Regression
# Abhängige Variable ist binär
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# Hypothese 1:
# besteht ein Zusammenhang zwischen PE, Alter oder BMI?
# ---------------------------------------------------------------------------------------------------
glm.fit01 <- glm(hypertonie ~ alter * bmi, data=data, family=binomial(logit))
summary(glm.fit01)

# Marginal Effect Size
logit.scalar = mean(dlogis(predict(glm.fit01, type="link")));
average_marginal_effects = logit.scalar * coef(glm.fit01);

# ---------------------------------------------------------------------------------------------------
# Hypothese 2:
# besteht ein Zusammenhang zwischen PE und vorher bestehenden hypertensiven Erkrankungen?
# ---------------------------------------------------------------------------------------------------
glm.fit02 <- glm(znhypertonie ~ factor(hypertonie), data=data, family=binomial(logit))
summary(glm.fit02)

# ---------------------------------------------------------------------------------------------------
# Hypothese 3:
# Gibt es einen ansteigenden Trend an PE Erkrankungen in den letzten Jahren…
# ---------------------------------------------------------------------------------------------------
glm.fit03 <- glm(hypertonie ~ AUFNDATUM, data=data, family=poisson)
summary(glm.fit03)