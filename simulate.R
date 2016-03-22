set.seed(1221)
pM <- 0.5
pF <- 0.4
pMF <- 0.3

uni <- runif(n=1000)
Mresults <- (uni < pM)
Fresults <- ((uni > (pM-pMF)) & (uni < (pM + pF - pMF)))
Mpre_couplingT <- rbinom(n=1000,size=20,prob=0.3)
Fpre_couplingT <- rbinom(n=1000,size=20,prob=0.5)
couplingT <- rbinom(n=1000,size=20,prob=0.5)
Mactiveness <- rbinom(n=1000,size=1,prob=0.6)
Factiveness <- rbinom(n=1000,size=1,prob=0.4)
Cactiveness <- rbinom(n=1000,size=1,prob=0.3)
clusterid <- rbinom(n=1000,20,prob=0.5)

print(table(Mresults, Fresults))

multivar_df <- data.frame(Mresults, Fresults, 
                          Mpre_couplingT, Fpre_couplingT, 
                          couplingT, Mactiveness,Factiveness,
                          Cactiveness, clusterid,
                          id=1:1000)

# rdsave(multivar_df)


