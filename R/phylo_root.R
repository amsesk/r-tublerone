phylo_root = function(tree,
                      node = NULL,
                      outgroup = NULL,
                      protect_edgelabel = TRUE,
                      split_root_brlen = TRUE,
                      resolve.root = TRUE,
                      position = NULL) {

  # Make sure node OR outgroup is specified - not neither and not both
  if (is.null(node) & is.null(outgroup)) {

    stop("Value for node or outgroup must be specified.")

    } else if (!is.null(node) & !is.null(outgroup)) {

    stop("You can't specify values for node and outgroup - pick one.")

    } else {

    # If outgroup is specified
    if (!is.null(outgroup)) {

      # Root the tree
      tree = ape::root(tree, outgroup = outgroup, edgelabel = protect_edgelabel, resolve.root = resolve.root)

      # Split the branch lengths around the root, if requested
      if (split_root_brlen) {
        if (!resolve.root) {

          stop("Can't split branch lengths around root if resolve.root == FALSE")

          } else {

          tree = tublerone::phylo_split_brlen_at_root(tree, outgroup)

          }

      # Return the tree
      return(tree)

      }
    }

    # If node is specified
    else {

      stop("Not yet implemented.")

      }
  }
}
