GatkAfHistGen <- function(df, isolate, approx_norm_binom_stdev = NULL, stats = NULL, title = isolate) {

  orig_len = length(df$p)+length(df$q)
  p = df[df$p >= 0.5,]$p
  q = df[df$q < 0.5,]$q
  pq = c(p,q)
  final_len = length(pq)
  pq = as_tibble(cbind(seq(1,length(pq),1), pq))
  out_plt = ggplot(data=pq) +
    geom_density(aes(x=pq), fill = "black", colour="black", alpha=0.5) +
    #geom_density(aes(x=q), fill = "black", colour="black", alpha=0.5) +
    #geom_histogram(aes(x=combined), fill = "darkgreen", colour="black", alpha=0.25, binwidth=0.01) +
    #scale_fill_manual(name = "Coverage Bins", values=c("x < 60" = "blue", "60 < x < 120" = "red", "x > 120" = "green")) +
    xlab("Allele Frequency") +
    ylab("Density") +
    ggtitle(title) +
    theme_bw() +
    theme (
      panel.grid = element_blank(),
      plot.title = element_text(size=8)
    )

  if (!is.null(approx_norm_binom_stdev)) {
    # Put the approximated normal distribution (of binomial) on the AF plot
    x = seq(0, 1, 0.01)
    y = dnorm(x = x, mean = 0.5, sd = approx_norm_binom_stdev)
    dist_points = as_tibble(cbind(x,y))
    dist_hist = hist(df$p, breaks = x, plot = F)
    max_dist_hist = max(dist_hist$counts)
    #scaling_factor = max_dist_hist/max(y)
    scaling_factor = 1.0
    print(dist_points)
    dist_points = dist_points %>%
      mutate(y_scaled = y * scaling_factor)
    expect_range = c(0.5-approx_norm_binom_stdev, 0.5+approx_norm_binom_stdev)

    out_plt = out_plt +
      geom_line(data = dist_points, aes(x,y_scaled), color = "red") +
      geom_area(data = subset(dist_points, x>=expect_range[1] & x<=expect_range[2]), aes(x=x, y=y_scaled), fill = "red", alpha = 0.15)
  }

  yrange = ggplot_build(out_plt)$layout$panel_scales_y[[1]]$range$range[2]
  out_plt = out_plt +
    ylim(0, yrange*1.1)

  if (!is.null(stats)) {
    p_in_binom_expect = stats %>%
      filter(isolate == ploidy_file_prefix) %>%
      pull(p_in_binom_expect)

    snp_density = stats %>%
      filter(isolate == ploidy_file_prefix) %>%
      pull(snp_density)

    mean_coverage = stats %>%
      filter(isolate == ploidy_file_prefix) %>%
      pull(mean_coverage)

    out_plt = out_plt +
      annotate("text", x=Inf, y=Inf, label=paste(round(mean_coverage,1), "x coverage", sep =""), sep=" ", vjust=1.3, hjust=1.2, cex=2.5) +
      annotate("text", x=Inf, y=Inf, label=paste(dim(df)[1], "SNPs", sep=" "), vjust=2.7, hjust=1.2, cex=2.5) +
      annotate("text", x=Inf, y=Inf, label=paste(round(p_in_binom_expect,2), "in expect"), sep=" ", vjust=4.1, hjust=1.2, cex=2.5) +
      annotate("text", x=Inf, y=Inf, label=paste(round(snp_density,8), "SNP/bp"), sep=" ", vjust=5.5, hjust=1.1, cex=2.5)
  }

  out_plt
}
