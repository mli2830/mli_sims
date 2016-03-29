library(lme4)
library(dplyr)

univar_df <- multivar_df %>% 
  gather(key="Sex",value="results",Mresults,Fresults) %>%
    mutate(id=1:nrow(.)) %>%
      gather(key="SexT",value="pre_couplingT",Mpre_couplingT,Fpre_couplingT) %>%
        gather(key="SexA",value="activeness",Mactiveness,Factiveness) %>%
          mutate(Sex = ifelse(Sex=="Mresults","Male","Female"),
            SexT = ifelse(SexT=="Mpre_couplingT","Male","Female"),
            SexA = ifelse(SexA=="Mactiveness","Male","Female")) %>%
              group_by(id) %>% summarise_each(funs(first)) %>% select(-c(SexT,SexA))

glmfit <- glmer(results~Sex+Cactiveness-1+(0+Sex|couple_id)
                , family="binomial"
                , data=univar_df)
                # , control = lmerControl(check.nobs.vs.nRE="ignore"))

print(summary(glmfit))

simfun <- function(n=100,Cprob=0.3,beta=c(-1,1,1),theta=c(1,0,1)) {
    sim0 <- expand.grid(Sex=c("Male","Female"),id=1:n) 
    ## creates a df from all combos (couple formation)
    sim1 <- data.frame(id=1:n,Cactiveness=rbinom(n,prob=Cprob,size=1))
    sim2 <- merge(sim0,sim1)
    form <- results~Sex+Cactiveness-1+(0+Sex|id)
    sim3 <- data.frame(sim2,
             results=suppressMessages(simulate(form[-2]
                                              , family="binomial"
                                              , newdata=sim2
                                              , newparams=list(beta=beta,
                                                               theta=theta)))[[1]])
    return(sim3)
}


tmpf <- function() {
    sim3 <- simfun()
    form <- results~Sex+Cactiveness-1+(0+Sex|id)
    glmfit2 <- glmer(form
                   , family="binomial"
                   , data=sim3)
    
    getME(glmfit2,"theta")
}

simres <- plyr::raply(100,tmpf(),.progress="text")

hist(log10(simres[,3]),col="gray")
hist(simres[,3],col="gray")
mean(simres[,3]==0)
summary(simres[,3])

summary(simres)

s3 <- simfun()
s3B <- reshape2::dcast(s3,Cactiveness+id~sex,value.var="results")

mfit <- MCMCglmm(cbind(Male,Female)~trait + Cactiveness - 1
                 ## , random = ~us(trait):id
                 , rcov=~us(trait):units
                 , prior = list(R=list(V=diag(2),nu=5)),
                 , family=c("categorical","categorical")
                 , data=s3B
		 , verbose=FALSE)

tidy(mfit)

library(glmmsr)

fit2 <- glmm(form,
     family="binomial",
     data=s3,
     method="SR")
