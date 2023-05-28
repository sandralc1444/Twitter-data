

mpd.by.clade = function(taxa, pool.tree){
  #Check if object is a phylogeny
  if(!is.phylo(pool.tree)){stop('pool.tree is not an object of class Phylo')}
  
  #Make nodes numeric for ease -> This is what I described in the last email, this way we iterate over nodes easily
  pool.tree$node.label = 1:pool.tree$Nnode
  
  #Prune a tree and inherit the node identifiers -> Get a phylogeny from a list of taxa. All node labels in pool.tree is inherited to phy.
  phy = drop.tip(pool.tree, setdiff(pool.tree$tip, taxa))
  
  
  #Here we iterate over all the now numbered labels and calculate MPD for each of them
  return(sapply(phy$node.label, function(x) mpd.clade(phy, pool.tree, x)))}





mpd.clade = function(phy, pool.tree, node,  null.model = c("taxa.labels", "richness", "frequency", "sample.pool", "phylogeny.pool")){
  
  # Here we prune the pool.tree to contain only descendants of the parameter node
  pool.tree = drop.tip(pool.tree, setdiff(pool.tree$tip, tips(pool.tree, node + length(pool.tree$tip))))
  
  #Index must be ofset by the number of tips in phy. Here we find all the TAXA in the community phylogeny that are descendants from parameter node
  
  taxa = tips(phy, which(phy$node.label == node)+length(phy$tip))
  
  #Create presence/absence vector
  pres = sapply(pool.tree$tip, function(x) if(x %in% taxa) return(1) else return(0))
  empty = 0
  
  #Create the "community matrix"
  HabMat = data.frame(pres, empty)
 
  HabMat = t(as.matrix(HabMat))

  # Here calculate the SES.mpd for the pool.tree that has been pruned to contain only descendants of the parameter node    
  return(mpd.query(pool.tree, HabMat, standardize = TRUE)) 
} 


#para hacer




