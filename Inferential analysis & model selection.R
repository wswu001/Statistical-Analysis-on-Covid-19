library(MASS) #use to conduct negative binomial analysis
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(pscl)
library(lmtest)
library(sandwich)
library(tibble)
library(jtools)

########## Inferential Analysis ##########

##Poisson Regression 
#create a function to examine the variance and the mean
dispersion_test <- function(x) {
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  invisible(res)}

dispersion_test(covid_trim$total_new_cases_per_million)

#built poisson regression models
model_pois <- glm(total_new_cases_per_million ~ FIRST_VACCINE_DATE + vaccine_type_used + PERSONS_FULLY_VACCINATED_PER100 + stringency_index, data = covid_trim, family = "poisson")
summary(model_pois)

#second way to examine variance and mean
with(covid_trim, tapply(total_new_cases_per_million, vaccine_type_used, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#get the goodness-of-fit
1 - pchisq(summary(model_pois)$deviance, summary(model_pois)$df.residual)

#------------------------------------------------------------

##Negative Binomial Regression
#preliminary model of negative binomial regression
model_nb0 <- glm.nb(total_new_cases_per_million ~ FIRST_VACCINE_DATE + vaccine_type_used + PERSONS_FULLY_VACCINATED_PER100 + stringency_index, data = covid_trim)
summary(model_nb0)

#goodness-of-fit
1 - pchisq(summary(model_nb0)$deviance, summary(model_nb0)$df.residual)

#get the coefficient
round(coef(model_nb0),4)

#test whether to remove FIRST_VACCINE_DATE and stringency_index
model_nb1 <- update(model_nb0, . ~ . - FIRST_VACCINE_DATE - stringency_index)
anova(model_nb0,model_nb1)

#test whether to leave vaccine_type_used
model_nb2 <- update(model_nb1, . ~ . - vaccine_type_used)
anova(model_nb1,model_nb2)

#get the result
summ(model_nb1, exp = T)

#------------------------------------------------------------

## Measure goodness-of-fit
model_pois1 <- glm(total_new_cases_per_million ~ vaccine_type_used + PERSONS_FULLY_VACCINATED_PER100, data = covid_trim, family = "poisson")
summary(model_pois1)

pchisq(2*(logLik(model_nb1)-logLik(model_pois1)), df = 1, lower.tail = FALSE)
qchisq(0.05,1,lower.tail = FALSE)
1 - pchisq(summary(model_nb1)$deviance, summary(model_nb1)$df.residual)
AIC(model_pois1,model_nb2)

#------------------------------------------------------------

##Multiple Linear Regression
covid_trim_remove0 <- covid_trim %>%
  filter(total_new_cases_per_million != 0)

model_lm_pre <- lm(total_new_cases_per_million ~ FIRST_VACCINE_DATE + vaccine_type_used + PERSONS_FULLY_VACCINATED_PER100 + stringency_index,
                   data = covid_trim_remove0)






