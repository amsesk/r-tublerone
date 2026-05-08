
#' Split a vector into chunks of given size.
#'
#' @param vec The vector to be chunked.
#' @param chunk_size The size of chunks.
#' @return A list of vector chunks indexed by chunk number.
#' @export

vec_split = function(vec, chunk_size) {
  vlen = length(vec)
  chunks = list()
  p = 1
  for (i in seq(1, vlen, chunk_size)) {
    chunk_max = i+(chunk_size-1)
    if (chunk_max > vlen) {
      chunks[[p]] = seq(i, vlen, 1)
    } else {
      chunks[[p]] = seq(i, i+(chunk_size-1), 1)
    }
    p = p+1
  }
  chunks
}

# %%
split_range_size = function(r, split_size) {
  split(r, ceiling(r/split_size))
}

# %% BUG: Errors when length(r) == n_splits, which wouldn't really be a useful split anyways
split_range_n = function(r, n_splits=NULL) {
  n = length(r)
  g = n_splits
  if (g>=n) {
    rlang::abort("n_splits must be < length(r)")
  }
  n_groups = list(
    n%%g,
    g-(n%%g)
  )
  group_sizes = list(
    (n-(n%%g)+g)/g,
    (n-(n%%g))/g
  )
  check = purrr::reduce2(n_groups, group_sizes, \(.sum, .ng, .gs) {
    .sum + (.ng*.gs)
  }, .init=0)
  stopifnot(check==n)

  breaks = list(
    r[min(r):((min(r)+(n_groups[[1]]*group_sizes[[1]]))-1)],
    r[((min(r)+(n_groups[[1]]*group_sizes[[1]]))):max(r)]
  )
  # print(group_sizes)
  # print(n_groups)
  out = map2(breaks, group_sizes, \(.b, .gs) {
    map(split_size(1:length(.b), .gs), \(.i) {
      .b[.i]
    })
  }) %>%
    list_flatten() %>%
    set_names(1:reduce(n_groups, sum))
  stopifnot( (out %>% unlist %>% sum) == sum(r) )
  # stopifnot( (out %>% map(length) %>% unlist %>% sum) == length(r) )
  return(out)
}
