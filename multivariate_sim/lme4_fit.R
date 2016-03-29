library(lme4)
library(dplyr)

glmfit <- glmer(results~Sex+Cactiveness-1+(0+Sex|Couple_id)
                , family="binomial"
                , data=univar_df)
                # , control = lmerControl(check.nobs.vs.nRE="ignore"))

print(summary(glmfit))
