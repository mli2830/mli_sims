library(MCMCglmm)

mfit <- MCMCglmm(cbind(Mresults,Fresults)~trait + Cactiveness - 1
            #     , random = ~us(trait):clusterid
                 , rcov=~us(trait):units
                 , family=c("categorical","categorical")
                 , prior = list(R=list(V=diag(2),nu=5))
                 , data=multivar_df
		 , verbose=FALSE)

print(summary(mfit))


