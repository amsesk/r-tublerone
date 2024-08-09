library(GenomicRanges)

expandGRange = function(gr, upstream = 2000, downstream = 1000) {
  if (length(gr) > 1) {
    message("Length of GRanges object passed to expandGRange must be 1.")
    stop()
  } else {
    chrom = as.character(chrom(gr))
    chrom_start = 0
    chrom_end = unname(seqlengths(seqinfo(gr))[chrom])
    if (as.character(strand(gr)) == "-") {
      new_start = start(gr) - downstream
      new_end = end(gr) + upstream
    } else {
      new_start = start(gr) - upstream
      new_end = end(gr) + downstream
    }
    if (new_start < chrom_start) {
      new_start = chrom_start
    }
    if (new_end > chrom_end) {
      new_end = chrom_end
    }
    start(gr) = new_start
    end(gr) = new_end
    gr
  }
}
grapply = function(granges, func) {
  unlist(GRangesList(lapply(seq_along(granges), function(i) func(granges[i]))))
}
