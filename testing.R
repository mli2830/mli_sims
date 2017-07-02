#### packages ----

library(ape)
library(lme4)
library(Matrix)
## additional possibilities for PGLMMs ...
library(MCMCglmm)
library(MASS) ## glmmPQL + ape::corBrownian()
library(pez)
## n.b. at present need 'modular' devel branch:
## devtools::install_github("glmmTMB/glmmTMB/glmmTMB",ref="modular")
library(glmmTMB)
## utils
library(dplyr)
library(coda)
library(lattice)
library(broom) ## need install_github("bbolker/broom")
library(dotwhisker)
library(nlme)

#### pez setup----

set.seed(2830)

# Generate simulated data for nspp species and nsite sites
nspp <- 100
nsite <- 10

# residual variance (set to zero for binary data)
sd.resid <- 0.1

# fixed effects
beta0 <- 0
beta1 <- 0

# magnitude of random effects
sd.B0 <- 2
sd.B1 <- 4

# whether or not to include phylogenetic signal in B0 and B1
signal.B0 <- TRUE
signal.B1 <- TRUE

# simulate a phylogenetic tree
phy <- rtree(n = nspp)
phy <- compute.brlen(phy, method = "Grafen", power = 0.5)

# standardize the phylogenetic covariance matrix to have determinant 1
Vphy <- vcv(phy)
Vphy <- Vphy/(det(Vphy)^(1/nspp))

# Generate environmental site variable
X <- matrix(1:nsite, nrow = 1, ncol = nsite)
X <- (X - mean(X))/sd(X)

# Perform a Cholesky decomposition of Vphy. This is used to
# generate phylogenetic signal: a vector of independent normal random
# variables, when multiplied by the transpose of the Cholesky
# deposition of Vphy will have covariance matrix equal to Vphy.

iD <- t(chol(Vphy))

# Set up species-specific regression coefficients as random effects
if (signal.B0 == TRUE) {
  b0 <- beta0 + iD %*% rnorm(nspp, sd = sd.B0)
} else {
  b0 <- beta0 + rnorm(nspp, sd = sd.B0)
}
if (signal.B1 == TRUE) {
  b1 <- beta1 + iD %*% rnorm(nspp, sd = sd.B1)
} else {
  b1 <- beta1 + rnorm(nspp, sd = sd.B1)
}

# Simulate species abundances among sites to give matrix Y that
# contains species in rows and sites in columns
y <- rep(b0, each=nsite)
y <- y + rep(b1, each=nsite) * rep(X, nspp)
y <- y + rnorm(nspp*nsite) #add some random 'error'
# Y <- rbinom(length(y), size=1, prob=exp(y)/(1+exp(y)))
y <- matrix(outer(b0, array(1, dim = c(1, nsite))), nrow = nspp,
            ncol = nsite) + matrix(outer(b1, X), nrow = nspp, ncol = nsite)
e <- rnorm(nspp * nsite, sd = sd.resid)
y <- y + matrix(e, nrow = nspp, ncol = nsite)
y <- matrix(y, nrow = nspp * nsite, ncol = 1)

# Y <- rbinom(n = length(y), size = 1, prob = exp(y)/(1 + exp(y)))
Y <- y
Y <- matrix(Y, nrow = nspp, ncol = nsite)

# name the simulated species 1:nspp and sites 1:nsites
rownames(Y) <- 1:nspp
colnames(Y) <- 1:nsite

par(mfrow = c(3, 1), las = 1, mar = c(2, 4, 2, 2) - 0.1)
matplot(t(X), type = "l", ylab = "X", main = "X among sites")
hist(b0, xlab = "b0", main = "b0 among species")
hist(b1, xlab = "b1", main = "b1 among species")

#Plot out; you get essentially this from plot(your.pglmm.model)
image(t(Y), ylab = "species", xlab = "sites", main = "abundance",
      col=c("black","white"))

# Transform data matrices into "long" form, and generate a data frame
YY <- matrix(Y, nrow = nspp * nsite, ncol = 1)

XX <- matrix(kronecker(X, matrix(1, nrow = nspp, ncol = 1)), nrow =
               nspp * nsite, ncol = 1)

site <- matrix(kronecker(1:nsite, matrix(1, nrow = nspp, ncol =
                                           1)), nrow = nspp * nsite, ncol = 1)
sp <- matrix(kronecker(matrix(1, nrow = nsite, ncol = 1), 1:nspp),
             nrow = nspp * nsite, ncol = 1)

dat <- data.frame(Y = YY, X = XX, site = as.factor(site), sp = as.factor(sp))

# Format input and perform communityPGLMM()
# set up random effects

# random intercept with species independent
re.1 <- list(1, sp = dat$sp, covar = diag(nspp))

# random intercept with species showing phylogenetic covariances
re.2 <- list(1, sp = dat$sp, covar = Vphy)

# random slope with species independent
re.3 <- list(dat$X, sp = dat$sp, covar = diag(nspp))

# random slope with species showing phylogenetic covariances
re.4 <- list(dat$X, sp = dat$sp, covar = Vphy)

# random effect for site
re.site <- list(1, site = dat$site, covar = diag(nsite))

# The rest of these tests are not run to save CRAN server time;
# - please take a look at them because they're *very* useful!
## Not run: 
pezfit <- communityPGLMM(Y ~ X, data = dat, family = "gaussian",
                           sp = dat$sp, site = dat$site, random.effects = list(re.1, re.2,
                                                                               re.3, re.4), REML = FALSE, verbose = FALSE)

#### lme4 setup ----

phylo.to.Z <- function(r) {
  ntip <- length(r$tip.label)
  Zid <- Matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
  nodes <- (ntip+1):max(r$edge)
  root <- nodes[!(nodes %in% r$edge[,2])]
  for (i in 1:ntip) {
    cn <- i  ## current node
    while (cn != root) {
      ce <- which(r$edge[,2]==cn)   ## find current edge
      Zid[i,ce] <- 1   ## set Zid to 1
      cn <- r$edge[ce,1]            ## find previous node
    }
  }
  Z <- t(r$edge.length * t(Zid))
  return(Z)
}

split_blkMat <- function(M,ind) {
  res <- list()
  if (length(ind)==1) return(list(M))
  for (i in 1:(length(ind)-1)) {
    v <- (ind[i]+1):ind[i+1]
    res[[i]] <- M[v,v]
  }
  return(res)
}

modify_phylo_retrms <- function(rt,phylo,phylonm,
                                phyloZ,sp) {
  rep_phylo <- rt$Zt@Dim[2]/length(unique(sp)) ## number of column (aka same as number of obs)
  ## FIXME: better way to specify phylonm
  ## need to replace Zt, Lind, Gp, flist, Ztlist
  ## we have the same number of parameters (theta, lower),
  ##  same number of obs
  n.edge <- nrow(phylo$edge)
  phylo.pos <- which(names(rt$cnms)==phylonm)
  inds <- c(0,cumsum(sapply(rt$Ztlist,nrow)))
  ## Zt: substitute phylo Z for previous dummy (scalar-intercept) Z
  # for(i in phylo.pos){
  # repterms <- nrow(rt[["Ztlist"]][[i]])/length(unique(sp))
  # rt[["Ztlist"]][[i]] <- KhatriRao(do.call(cbind,replicate(rep_phylo,t(phyloZ),simplify = FALSE)),
  #             matrix(1
  #                    , ncol=ncol(rt[["Ztlist"]][[i]])
  #                    , nrow=repterms)
  # )
  ## reconstitute Zt from new Ztlist
  # }
  # rt[["Zt"]] <- do.call(rbind,rt[["Ztlist"]])
  ## Gp: substitute new # random effects (n.edge) for old # (n.phylo)
  Gpdiff <- diff(rt$Gp)  ## old numbers
  Gpdiff_new <- Gpdiff
  # for(i in phylo.pos){
  # Gpdiff_new[i] <- n.edge  ## replace
  # }
  rt[["Gp"]] <- as.integer(c(0,cumsum(Gpdiff_new)))          ## reconstitute
  ## Lind: replace phylo block with the same element, just more values
  Lind_list <- split(rt[["Lind"]],rep(seq_along(Gpdiff),Gpdiff))
  # for(i in phylo.pos){
  # Lind_list[[i]] <- rep(Lind_list[[i]][1],n.edge)
  # }
  # rt[["Lind"]] <- unlist(Lind_list)
  ## Lambdat: replace block-diagonal element in Lambdat with a
  ##   larger diagonal matrix
  Lambdat_list <- split_blkMat(rt[["Lambdat"]],inds)
  # for(i in phylo.pos){
  # Lambdat_list[[i]] <- KhatriRao(Diagonal(n.edge,1.0),
  #             matrix(1
  #                    , ncol=n.edge
  #                    , nrow=repterms))
  # }
  
  for(i in phylo.pos){
    repterms <- nrow(rt[["Ztlist"]][[i]])/length(unique(sp))
    #     rt[["Ztlist"]][[i]] <- KhatriRao(do.call(cbind,replicate(rep_phylo,t(phyloZ),simplify = FALSE)),
    #                 matrix(1
    #                        , ncol=ncol(rt[["Ztlist"]][[i]])
    #                        , nrow=repterms)
    #     )
    ## reconstitute Zt from new Ztlist
    rt[["Ztlist"]][[i]] <- t(phyloZ) %*% rt[["Ztlist"]][[i]]
    Gpdiff_new[i] <- n.edge  ## replace
    Lind_list[[i]] <- rep(Lind_list[[i]][1],n.edge)
    Lambdat_list[[i]] <- Diagonal(n.edge,1.0)
  }
  rt[["Zt"]] <- do.call(rbind,rt[["Ztlist"]])
  rt[["Lind"]] <- unlist(Lind_list)
  rt[["Lambdat"]] <- Matrix::.bdiag(Lambdat_list)
  ## flist: 
  rt[["flist"]] <- as.list(rt[["flist"]])
  rt[["flist"]][[phylonm]] <- factor(paste0("edge_",seq(n.edge)))
  return(rt)
}

phylo_glmm <- function(formula,data,family,phylo,phylonm,phyloZ,sp) {
  glmod <- glFormula(formula=formula,data = data, family = family)
  glmod$reTrms <- modify_phylo_retrms(glmod$reTrms,phylo,
                                      phylonm,phyloZ,sp)
  devfun <- do.call(mkGlmerDevfun, glmod)
  opt <- optimizeGlmer(devfun)
  devfun <- updateGlmerDevfun(devfun, glmod$reTrms)
  opt <- optimizeGlmer(devfun, stage=2)
  mkMerMod(environment(devfun), opt, glmod$reTrms, fr = glmod$fr)
}

phylo_lmm <- function(formula,data,phylo,phylonm,phyloZ,control,sp) {
  lmod <- lFormula(formula=formula,data = data,control=control)
  lmod$reTrms <- modify_phylo_retrms(lmod$reTrms,phylo,
                                     phylonm,phyloZ,sp)
  devfun <- do.call(mkLmerDevfun, lmod)
  opt <- optimizeLmer(devfun)
  # devfun <- updateLmerDevfun(devfun, lmod$reTrms)
  # opt <- optimizeLmer(devfun, stage=2)
  mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)
}

phyZ <- phylo.to.Z(phy)

dat$obs <- dat$sp

lme4fit <- phylo_lmm(Y ~ X + (1|sp) + (1|obs) + (0+X|sp) + (0+X|obs) 
                                 , data=dat
                                 , phylonm = "sp"
                                 , sp = dat$sp
                                 , phylo = phy
                                 , phyloZ=phyZ
                                 , control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"))

#### Results ----

summary(pezfit)
summary(lme4fit)
