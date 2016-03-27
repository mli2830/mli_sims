library(lme4)
library(dplyr)

Mdf <- multivar_df %>% 
  mutate(results=Mresults,sex="Male") %>%
    select(c(results,Cactiveness,sex,id))
    

Fdf <- multivar_df %>% 
  mutate(results=Fresults,sex="Female") %>%
    select(c(results,Cactiveness,sex,id))

## head(reshape2::melt(multivar_df,id.vars=2:ncol(multivar_df))

uni_df <- rbind(Mdf,Fdf)

glmfit <- glmer(results~sex+Cactiveness-1+(0+sex|id)
                , family="binomial"
                , data=uni_df)

print(summary(glmfit))


simfun <- function(n=1000,Cprob=0.3,beta=c(-1,1,1),theta=c(1,0,1)) {
    sim0 <- expand.grid(sex=c("Male","Female"),id=1:n)
    sim1 <- data.frame(id=1:n,Cactiveness=rbinom(n,prob=Cprob,size=1))
    sim2 <- merge(sim0,sim1)
    form <- results~sex+Cactiveness-1+(0+sex|id)
    sim3 <- data.frame(sim2,
             results=suppressMessages(simulate(form[-2]
                                              , family="binomial"
                                              , newdata=sim2
                                              , newparams=list(beta=beta,
                                                               theta=theta)))[[1]])
    return(sim3)
}

form <- results~sex+Cactiveness-1+(0+sex|id)


tmpf <- function() {
    sim3 <- simfun()
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
