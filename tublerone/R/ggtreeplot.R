### Credit to https://thackl.github.io/ggtree-composite-plots, thackl on GitHub
### With minor adaptation

ggtreeplot <- function(ggtree, data = NULL, mapping = aes(), flip=FALSE,
                       expand_limits=c(0,.6), ...){

  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")

  # match the tree limits
  print(expand_limits)
  limits <- range(ggtree$data$y, na.rm = T)
  limits[1] <- limits[1] + (limits[1] * expand_limits[1]) - expand_limits[2]
  limits[2] <- limits[2] + (limits[2] * expand_limits[3]) + expand_limits[4]

  print(limits)

  if(flip){
    mapping <- modifyList(aes_(x=~x), mapping)
    data <- mutate(data, x=tublerone::tree_y(ggtree, data))
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_x_continuous(limits=limits, expand=c(0,0))
  }else{
    mapping <- modifyList(aes_(y=~y), mapping)
    data <- mutate(data, y=tublerone::tree_y(ggtree, data))
    print(data)
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_y_continuous(limits=limits, expand=c(0,0))
  }
  gg
}
