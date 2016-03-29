library(lme4)
library(dplyr)

simfun <- function(n=1000,Cprob=0.3,b0=c(-1,1,1),t0=c(1,0,1)) {
  sim0 <- expand.grid(Sex=c("Male","Female"),Couple_id=1:n) 
  ## creates a df from all combos (couple formation)
  sim1 <- data.frame(Couple_id=1:n,Cactiveness=rbinom(n,prob=Cprob,size=1))
  sim2 <- merge(sim0,sim1)
  sim3 <- data.frame(sim2,
                     results=suppressMessages(simulate(form[-2]
                                                       , family="binomial"
                                                       , newdata=sim2
                                                       , newparams=list(beta=b0,
                                                                        theta=t0)))[[1]])
  return(sim3)
}

univar_df <- simfun(n=n_couples,Cprob = Couple_activeness, b0=beta, t0=theta)
mdf <- reshape2::dcast(univar_df,Cactiveness+Couple_id~Sex,value.var="results")
multivar_df <- mdf %>% mutate(Mresults=Male,Fresults=Female) %>% select(-c(Male,Female))
  

# rdsave(multivar_df, univar_df)
