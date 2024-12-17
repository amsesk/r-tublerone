
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
