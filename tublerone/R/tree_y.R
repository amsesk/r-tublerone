### Credit to https://thackl.github.io/ggtree-composite-plots, thackl on GitHub
### With minor adaptation

tree_y <-  function(ggtree, data){
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  left_join(select(data, label), select(ggtree$data, label, y)) %>%
    pull(y)
}
