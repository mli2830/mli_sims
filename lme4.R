library(lme4)
library(dplyr)

Mdf <- multivar_df %>% 
  mutate(results=Mresults,sex="Male") %>%
    select(c(results,Cactiveness,sex,id))
    

Fdf <- multivar_df %>% 
  mutate(results=Fresults,sex="Female") %>%
    select(c(results,Cactiveness,sex,id))

uni_df <- rbind(Mdf,Fdf)

glmfit <- glmer(results~sex+Cactiveness-1+(0+sex|id)
                , family="binomial"
                , data=uni_df)

print(summary(glmfit))

