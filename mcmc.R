library(MCMCglmm)

mfit <- MCMCglmm(cbind(Mresults,Fresults)~trait + Cactiveness - 1
            #     , random = ~us(trait):clusterid
                 , rcov=~us(trait):units
                 , family=c("categorical","categorical")
                 , data=multivar_df
		 , verbose=FALSE)

print(summary(mfit))


