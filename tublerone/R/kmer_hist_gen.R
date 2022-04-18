KmerHistGen <- function(prefix, coverages = NULL, title = basename(prefix), xmax = NULL, ymax = NULL) {
  if (!is.null(coverages)) {
    mean_coverage = round(coverages[coverages$strain == basename(prefix),]$mean_coverage, 2)
    median_coverage = round(coverages[coverages$strain == basename(prefix),]$median_coverage, 2)
  }
  hist = read.table(paste(prefix, ".khist", sep=""), col.names = c("depth", "rawcount", "count"), sep="\t")

  #hist_quartiles = quantile(hist$count, prob = c(0.25, 0.5, 0.75))
  #hist_iqr = hist_quartiles[["75%"]] - hist_quartiles[["25%"]]

  #y_max = hist_quartiles[["75%"]] + ((1.5)*(hist_iqr))
  peaks = read.table(paste(prefix, ".peaks", sep=""), comment.char = "#", col.names=c("start","center","stop","max","volume"), sep="\t")
  lower_peak_center = min(peaks$center)
  print(lower_peak_center)
  print(peaks)
  peaks = peaks %>%
    arrange(desc(volume)) %>%
    top_n(2, wt = volume) %>%
    filter(center <= 10*lower_peak_center)

  if(is.null(xmax)) {
    xmax = max(peaks$center)*3.0
  }
  if(is.null(ymax)) {
    ymax = hist[peaks[1,2],3]*1.5
  }

  #if ( (y_max < max(peaks$max)) | (y_max > max(peaks$max)*10 )) {
  #  y_max = max(peaks$max)*1.25
  #}
  ggplot(hist) +
    geom_point(aes(x=depth, y=count)) +
    geom_line(aes(x=depth, y=count)) +
    ylim(0,ymax) +
    xlim(0,xmax) +
    geom_vline(xintercept = peaks[1,]$center, colour = "blue", alpha=0.5) +
    geom_vline(xintercept = peaks[2,]$center, colour = "blue", alpha=0.5) +
    xlab("Depth") +
    ylab("Count") +
    #annotate("text", x=Inf, y=Inf, label=paste("mean coverage = ", mean_coverage, "x", sep=""), vjust=1.2, hjust=1.2, cex=5) +
    #annotate("text", x=Inf, y=Inf, label=paste("median coverage = ", median_coverage, "x", sep=""), vjust=3.0, hjust=1.2, cex=5) +
    ggtitle(title) +
    theme_bw()+
    theme (
      panel.grid = element_blank(),
      plot.title = element_text(size=8)
    )
}
