#' @title dedup_clones
#'
#' @description Deduplicates clones which have a minimum distance between them. 
#'
#' @param snp_pairwise Two-dimensional named matrix of pairwise-distances.
#' @param minimum_dist Minimum distance to consider two sequences a clone.
#'
#' @return matrix
#'
#' @examples
#' test = rbind(c(0,1,5,6,5),
#'              c(2,0,5,6,7),
#'              c(4,5,0,0,0),
#'              c(4,5,0,0,0),
#'              c(4,5,0,0,0),
#' rownames(test) = paste0(rep('seq', 5), seq(1,5))
#' colnames(test) = paste0(rep('seq', 5), seq(1,5))
#' dedup_clones(test)
#'
#' @export 

dedup_clones = function(snp_pairwise, minimum_dist = 0) {

  identicals = list()
  for (row in rownames(snp_pairwise)) {
    identicals[[row]] = sort(colnames(snp_pairwise)[which(snp_pairwise[row, ] == minimum_dist)])
  }

  # Get ride of duplicate vectors so that each list entry is a unique group of identicals
  unique_identicals = unique(identicals)

  # Arbitrarily pick the first one in each group of identicals
  to_keep = lapply(unique_identicals, function(x) x[[1]])

  # Reform distance matrix with only one of each group of identicals
  pairwise_dedup = snp_pairwise[unlist(to_keep), unlist(to_keep)]

  return(pairwise_dedup)
}
