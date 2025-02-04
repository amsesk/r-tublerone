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
    plts[[row]] = plot_volcano(reults = sub,
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

#' @importFrom glue glue
#' @importFrom ggrepel geom_text_repel
#' @import ggplot2
#' @import dplyr
#' @export
plot_volcano = function(
                        results,
                        group1,
                        group2,
                        gene_name_column = "gene",
                        fc_numerator_column = "fc_numerator",
                        fc_denominator_column = "fc_denominator",
                        output_path = NULL,
                        n_label = 10,
                        lfccut = 0.5,
                        alpha = 0.05,
                        width = 4.25,
                        height = 4.25,
                        dotsize = 0.2,
                        labelsize = 2,
                        return_figs = FALSE,
                        ylab = "-log10(padj)",
                        xlab = "log2FoldChange",
                        draw_cutoff_lines = TRUE,
                        y_transform_func = function(y) -1*log10(y)
                        ) {
  title = glue("{group1} vs {group2}")
  results = results %>%
    filter(fc_numerator %in% c(group1, group2) & fc_denominator %in% c(group1, group2)) %>%
    mutate(is_sig = ifelse(padj <= alpha & abs(log2FoldChange) >= lfccut, TRUE, FALSE))
  labels_to_show = results %>% 
    filter(is_sig) %>% 
    slice_min(padj, n=n_label) %>% 
    pull(gene)
  results = results %>% 
    mutate(display_label = ifelse(gene %in% labels_to_show, TRUE, FALSE))
  plt = ggplot(data = results,  aes(x = log2FoldChange, y = y_transform_func(padj), color = is_sig)) +
    geom_point(size = dotsize) +
    geom_text_repel(data = subset(results, display_label), aes(label = gene), color='black', size = labelsize, max.overlaps = 100) +
    scale_color_manual(values = c("black", "red")) +
    ylab(ylab) +
    xlab(xlab) +
    guides(color = "none") +
    ggtitle(title) +
    theme_classic()
  if (draw_cutoff_lines) {
    plt = plt +
      geom_hline(yintercept = -log10(alpha), linetype = 'dashed') +
      geom_vline(xintercept = -lfccut, linetype = 'dashed') +
      geom_vline(xintercept = lfccut, linetype = 'dashed')
  }
  if (return_figs) {
    return(plt)
  } else {
    pdf(output_path, width = width, height = height)
    print(plt)
    dev.off()
  }
}
