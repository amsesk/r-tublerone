#' @importFrom phytools getDescendants
#' @export

insert_clade = function(clade,
                        tree,
                        node,
                        one_later_diverging_tip) {

  tree$edge.length = rep(1, length(tree$edge.length))

  tip_id = NULL
  tip_id = which(tree$tip.label == one_later_diverging_tip)

  if (length(tip_id) == 0) {
    stop("Tip not found in tree.")

  }

  node_edge = tree$edge[which(tree$edge[,1] == node),]

  print(node_edge)

  pos = NULL
  for (r in 1:dim(node_edge)[1]) {
    if (tip_id %in% phytools::getDescendants(tree, node_edge[r,2])) {
      pos = r
    }
  }
  insert_loc = which(tree$edge[,1] == node)[pos]

  print(insert_loc)

  orig_parent = tree$edge[insert_loc,1]
  orig_child = tree$edge[insert_loc, 2]

  new_node_id = dim(tree$edge)[1] + 2
  new_node_internal_number = new_node_id - length(tree$tip.label)

  new_edge = matrix(nrow = (length(tree$edge[,1]) + 1), ncol = 2)

  new_edge[-insert_loc,] = tree$edge
  new_edge[insert_loc,] = c(new_node_id, orig_child)
  new_edge[insert_loc+1,] = c(orig_parent, new_node_id)

  print(new_edge)

  new_tree = tree
  new_tree$edge = new_edge

  new_tree$edge.length = rep(1, (new_node_id-1) )
  new_tree$node.label[new_node_internal_number] = ""
  new_tree$Nnode = new_tree$Nnode + 1

  new_tree = bind.tip(new_tree, "NewTip", edge.length = 1, where=new_node_id)

  new_tree = bind.tree(new_tree, clade, where = which(new_tree$tip.label == "NewTip"))

  return(new_tree)
}
