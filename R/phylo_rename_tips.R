phylo_rename_tips = function(phylo, old_to_new) {
  new_tiplabs = c()
  for (tip in phylo$tip.label) {
    if(tip %in% names(old_to_new)) {
      new_tiplabs = c(new_tiplabs, old_to_new[[tip]])
    } else {
      print((paste("Unable to rename tip, so leaving it the same:", tip)))
      new_tiplabs = c(new_tiplabs, tip)
    }
  }
  phylo$tip.label = new_tiplabs

  return(phylo)
}
