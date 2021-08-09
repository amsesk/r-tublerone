phylo_split_brlen_at_root = function(tree,
                                     outgroup) {

  n_internal_nodes = tree$Nnode
  ntips = length(tree$tip.label)
  internal_start_idx = ntips+1
  internal_end_idx = ntips+n_internal_nodes
  outgroup_node = NULL
  for (internal_id in internal_start_idx:internal_end_idx) {
    desc_nodes = phytools::getDescendants(tree, internal_id)
    desc_tips = desc_nodes[desc_nodes <= ntips]
    desc_tip_labels = tree$tip.label[desc_tips]
    if ( (length(desc_tip_labels) == length(outgroup)) &
         (all(desc_tip_labels %in% outgroup))) {
      outgroup_node = internal_id
      break
    }
  }
  outgroup_parent_node = tree$edge[tree$edge[,2] == outgroup_node,1]
  outgroup_parent_edge_row_to_outgroup = which(tree$edge[,1] == outgroup_parent_node & tree$edge[,2] == outgroup_node)
  outgroup_parent_edge_row_to_ingroup = which(tree$edge[,1] == outgroup_parent_node & tree$edge[,2] != outgroup_node)
  ingroup_node = tree$edge[outgroup_parent_edge_row_to_ingroup,2]
  brlen_to_split = tree$edge.length[outgroup_parent_edge_row_to_ingroup]
  split_brlen = brlen_to_split/2
  tree$edge.length[outgroup_parent_edge_row_to_ingroup] = split_brlen
  tree$edge.length[outgroup_parent_edge_row_to_outgroup] = split_brlen

  # If we're splitting the brlens around the root, that means we're resolving the root
  # which means that we need to move the support valuse to the new resolved root node
  outgroup_node_idx_in_nodelabels = outgroup_node - ntips
  ingroup_node_idx_in_nodelabels = ingroup_node - ntips

  support_to_move = tree$node.label[ingroup_node_idx_in_nodelabels]

  tree$node.label[outgroup_node_idx_in_nodelabels] = support_to_move
  tree$node.label[ingroup_node_idx_in_nodelabels] = ""


  return(tree)

}
