#lme4 parameters

form <- results~Sex+Cactiveness-1+(0+Sex|Couple_id)

n_couples=5000 ## number of couples
Couple_activeness=0.3  ## couple activeness probability 
beta <- c(-1,1,1) ##
theta <- c(1,0,1) ## random effect sd
