# %%
plot_volcanoes_combinatorial = function(results,
                                        indepvar,
                                        metadata,
                                        output_path= NULL,
                                        n_label = 10,
                                        lfccut = 0.5,
                                        alpha = 0.05,
                                        width = 4.25,
                                        height = 4.25,
                                        dotsize = 0.2,
                                        labelsize = 2,
                                        return_figs = FALSE
                                        ) {
  combs = t(combn(unique(metadata[,indepvar]), m=2))
  plts = plot_volcanoes_multi(results = results,
                       combinations = combs,
                       output_path = output_path,
                       n_label = n_label,
                       lfccut = lfccut,
                       alpha = alpha,
                       width = width,
                       height = height,
                       dotsize = dotsize,
                       labelsize = labelsize,
                       return_figs = return_figs
                       )
  if (return_figs) {
    return(plts)
  } else {
    return (NULL)
  }
}

# %%
plot_volcanoes_multi = function(results,
                                combinations,
                                output_path = NULL,
                                n_label = 10,
                                lfccut = 0.5,
                                alpha = 0.05,
                                width = 4.25,
                                height = 4.25,
                                dotsize = 0.2,
                                labelsize = 2,
                                return_figs = FALSE
                                ) {

  plts = list()
  for (row in seq_len(nrow(combinations))) {
    n = combinations[row,1]
    d = combinations[row,2]
    sub = subset(results, fc_numerator == n & fc_denominator == d)
    title = glue("{n} vs {d}")
    sub = sub %>% mutate(display_label = ifelse(sub$gene %in% pull(sub %>% filter(is_sig) %>% slice_min(padj, n=n_label), gene), TRUE, FALSE))
    plts[[row]] = ggplot(data = sub,  aes(x = log2FoldChange, y = -log10(padj), color = is_sig)) +
      geom_point(size = dotsize) +
      geom_text_repel(data = subset(sub, display_label), aes(label = gene), color='black', size = labelsize) +
      geom_hline(yintercept = -log10(alpha), linetype = 'dashed') +
      geom_vline(xintercept = -lfccut, linetype = 'dashed') +
      geom_vline(xintercept = lfccut, linetype = 'dashed') +
      scale_color_manual(values = c("black", "red")) +
      guides(color = "none") +
      ggtitle(title) +
      theme_classic()
  }
  if (return_figs) {
    return(plts)
  } else {
    pdf(output_path, width = width, height = height)
    for (p in plts) {
      print(p)
    }
    dev.off()
  }
}
