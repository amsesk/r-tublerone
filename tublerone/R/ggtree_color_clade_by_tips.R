ggtree_color_clade_by_tips = function(tree, ggtree, df, clade_column, tip_column, strict = T) {
  unique_clades = df %>%
    select(!!sym(clade_column)) %>%
    unique %>%
    pull(!!sym(clade_column))

  print(unique_clades)

  ntips = length(tree$tip.label)

  ggtree$data = ggtree$data %>%
    mutate(edgecol = NA)

  for (u in unique_clades) {
    clade_tips = df %>%
      filter(!!sym(clade_column) == u) %>%
      pull(!!sym(tip_column))

    #print(clade_tips)
    clade_node = tublerone::get_ancestral_node_of_tips(tree, clade_tips, strict = strict)

    clade_node_desc = phytools::getDescendants(tree, clade_node)

    ggtree_data_indices = which(ggtree$data$node %in% clade_node_desc)

    print(ggtree_data_indices)

    ggtree$data$edgecol[ggtree_data_indices] = u
  }

  return(ggtree$data)
}
