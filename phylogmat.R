
library(ape)
library(Matrix)
set.seed(101)
r <- makeNodeLabel(rtree(7))
# r <- makeNodeLabel(rtree(6))
# print(r$edge)
## my previous code failed with rtree(6)
## trying to reorder/change the phylogenetic tree
plot(r,show.node.label=TRUE)
phylo.to.Z2 <- function(r) {
  ntip <- length(r$tip.label)
  print(ntip)
  ee <- r$edge
  TEmat <- Matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
  oee <- r$edge[order(r$edge[,1],r$edge[,2]),]
  pos <- ee[,2]
  colnames(TEmat) <- opos <- oee[,2]
  rownames(TEmat) <- 1:ntip
  sn <- r$edge[r$edge[,1]==ntip+1,2]
  for (i in 1:ntip) {
    ifelse(
      which(pos==sn[2])>=which(pos==i)
      , TEmat[i,1:which(opos==i)] <- as.numeric((sn[1]<=opos & opos <= oee[which(opos==i),1]) | opos==i)[1:which(opos==i)]
      , TEmat[i,1:which(opos==i)] <- as.numeric((sn[2]<=opos & opos <= oee[which(opos==i),1]) | opos==i)[1:which(opos==i)]
    )
  }
  TEmat <- TEmat[,as.character(r$edge[,2])]
  colnames(TEmat) <- rownames(TEmat) <- NULL
  Z <- t(r$edge.length * t(TEmat))
  return(Z)
}


phylo.to.Z <- function(r) {
  ntip <- length(r$tip.label)
  Z <- Matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
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
# phylo.to.Z(r)

ta<-system.time(aa <- phylo.to.Z2(r))

tb<-system.time(bb <- phylo.to.Z(r))
# colnames(bb) <- rownames(bb) <- NULL
aa
bb

all.equal(unname(aa),bb)
ta
tb