get_ancestral_node_of_tips = function(tree,
                                      tiplabels,
                                      strict = T) {

  if(!strict) {
    print("tublerone::get_ancestral_node_of_tips :: `strict` is OFF")
  }
  ntips = length(tree$tip.label)
  nnodes = tree$Nnode
  internal_node_numbers = (ntips+1):(ntips+nnodes)
  other_tiplabels = tree$tip.label[-which(tree$tip.label %in% tiplabels)]

  # Only pertinent if strict is OFF
  if (!strict) {
    node_desc_clade_sizes = list()
  }

  for (i in internal_node_numbers) {
    descendants = phytools::getDescendants(tree, i)
    descendant_tips = tree$tip.label[descendants[descendants <= ntips]]

    if (all(tiplabels %in% descendant_tips)) {
      if (any(other_tiplabels %in% descendant_tips)) {
        if (strict) {
          stop("No node has those, and only those descendant tips.")
        } else {
          node_desc_clade_sizes[as.character(i)] = length(descendant_tips)
        }
      } else {
        return(i)
      }
    }
  }

  if (!strict) {
    return(names(node_desc_clade_sizes)[which(unlist(node_desc_clade_sizes) == min(unlist(node_desc_clade_sizes)))])
  } else {
    stop("Unhandled exception.")
  }
}
