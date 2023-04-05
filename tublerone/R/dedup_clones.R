dedup_clones = function(snp_pairwise) {

identicals = list()
for (row in rownames(snp_pairwise)) {
  identicals[[row]] = sort(colnames(snp_pairwise)[which(snp_pairwise[row, ] == 0)])
  }

# Get ride of duplicate vectors so that each list entry is a unique group of identicals
unique_identicals = unique(identicals)

# Arbitrarily pick the first one in each group of identicals
to_keep = lapply(unique_identicals, function(x) x[[1]])

# Reform distance matrix with only one of each group of identicals
pairwise_dedup = snp_pairwise[unlist(to_keep), unlist(to_keep)]

return(pairwise_dedup)
}
