library(lme4)

glmfit <- glmer(results~Sex+Cactiveness-1+(0+Sex|couple_id)
                , family="binomial"
                , data=univar_df)
                # , control = lmerControl(check.nobs.vs.nRE="ignore"))

print(summary(glmfit))



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
s3B <- reshape2::dcast(s3,Cactiveness+id~Sex,value.var="results")

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
