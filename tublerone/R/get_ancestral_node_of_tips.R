get_ancestral_node_of_tips = function(tree,
                                      tiplabels) {
  ntips = length(tree$tip.label)
  nnodes = tree$Nnode
  internal_node_numbers = (ntips+1):(ntips+nnodes)
  other_tiplabels = tree$tip.label[-which(tree$tip.label %in% tiplabels)]
  for (i in internal_node_numbers) {
    descendants = phytools::getDescendants(tree, i)
    descendant_tips = tree$tip.label[descendants[descendants <= ntips]]
    if (!any(other_tiplabels %in% descendant_tips) & all(tiplabels %in% descendant_tips)) {
      return(i)
    }
  }
  stop("No node has those, and only those descendant tips.")
}
