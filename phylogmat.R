
library(ape)
set.seed(101)
r <- makeNodeLabel(rtree(3))
plot(r)
phylo.to.Z2 <- function(r) {
  ntip <- length(r$tip.label)
  TEmat <- matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
  colnames(TEmat) <- pos <- r$edge[,2]
  rownames(TEmat) <- 1:ntip
  secondnode <- r$edge[r$edge[,1]==ntip+1,]
  for (i in 1:ntip) {
    ifelse(which(pos==secondnode[2,2])>which(pos==i),
    TEmat[i,1:which(pos==i)] <- as.numeric((secondnode[1,2]<=pos & pos > i) |pos==i)[1:which(pos==i)],
    TEmat[i,1:which(pos==i)] <- as.numeric((secondnode[2,2]<=pos & pos != secondnode[1,2])|pos==i)[1:which(pos==i)]
    )
  }
  colnames(TEmat) <- rownames(TEmat) <- NULL
  Z <- t(r$edge.length * t(TEmat))
  return(Z)
}


phylo.to.Z <- function(r) {
  ntip <- length(r$tip.label)
  Z <- matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
  nodes <- (ntip+1):max(r$edge)
  root <- nodes[!(nodes %in% r$edge[,2])]
  for (i in 1:ntip) {
    cn <- i  ## current node
    while (cn != root) {
      ce <- which(r$edge[,2]==cn)   ## find current edge
      Z[i,ce] <- r$edge.length[ce]  ## set Z to branch length
      cn <- r$edge[ce,1]            ## find previous node
    }
  }
  return(Z)
}
phylo.to.Z(r)

aa <- phylo.to.Z2(r)
bb <- phylo.to.Z(r)
# colnames(bb) <- rownames(bb) <- NULL

all.equal(unname(aa),bb)
