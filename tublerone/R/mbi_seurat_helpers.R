library(ggplot2)
library(dplyr)
library(patchwork)

pmito = function(counts, mtpat = "^human[-]MT") {
    nMito = apply(counts[which(grepl(mtpat,rownames(counts))),], 2, sum)
    return(nMito/nrna(counts))
}

nfeatures = function(counts) {
    return(apply(counts, 2, function(x) length(which(x != 0))))
}

nrna = function(counts) {
    return(apply(counts, 2, sum))
}

auto_filter_cells = function(so) {
    pMito_autocut = quantile(so[["percent.mito"]][,1], c(0.95))
    nFeature_autocuts = quantile(so[["nFeature_RNA"]][,1], c(0.10, 0.95))
    nRna_autocuts = quantile(so[["nCount_RNA"]][,1], c(0.10, 0.95))
    subset(so, cells = WhichCells(so, expression = 
                                        percent.mito < pMito_autocut[1] &
                                        nFeature_RNA > nFeature_autocuts[1] & 
                                        nFeature_RNA < nFeature_autocuts[2] &
                                        nCount_RNA > nRna_autocuts[1] &
                                        nCount_RNA < nRna_autocuts[2]
                   )
    )
}

### Plot the diagnostic plots across all samples together
multiqc_plt = function(SOs) {
    diag_gg = tibble(sampleid = NULL, metric = NULL, value = NULL, ncells = NULL)
    for (s in names(SOs)) {
        pMito_gg = tibble(sampleid = s, metric = "pMito", value = SOs[[s]][['percent.mito']][,1], ncells = dim(SOs[[s]][[]])[1])
        nFeature_gg = tibble(sampleid = s, metric = "nFeature", value = SOs[[s]][['nFeature_RNA']][,1], ncells = dim(SOs[[s]][[]])[1])
        nRna_gg = tibble(sampleid = s, metric = "nRna", value = SOs[[s]][['nCount_RNA']][,1], ncells = dim(SOs[[s]][[]])[1])
        diag_gg = bind_rows(diag_gg, pMito_gg, nFeature_gg, nRna_gg)
        #diag_gg$sampleid = factor(diag_gg$sampleid, levels=levels(as.factor(diag_gg$sampleid))[c(1,11,12,13,14,15,16,17,18,2,3,4,5,6,7,8,9,10)])
    }
    print(diag_gg)
    lims = c(2,max(log10(diag_gg$ncells)))
    pMito_plt = ggplot(diag_gg %>%
                       filter(metric == "pMito"), aes(x = sampleid, y=value, fill=log10(ncells))) +
        geom_violin(scale="width") +
        ylab("pMitochondrial") +
        scale_fill_gradient(low = "white", high=scales::muted("blue"), limits=lims)
    nFeature_plt = ggplot(diag_gg %>%
                       filter(metric == "nFeature"), aes(x = sampleid, y=value, fill=log10(ncells))) +
        geom_violin(scale="width")+
        ylab("nFeatures")+
        scale_fill_gradient(low = "white", high=scales::muted("blue"), limits=lims)
    nRna_plt = ggplot(diag_gg %>%
                       filter(metric == "nRna"), aes(x = sampleid, y=value, fill=log10(ncells))) +
        geom_violin(scale="width")+
        ylab("nRna")+
        scale_fill_gradient(low = "white", high=scales::muted("blue"), limits=lims)

    
    pMito_plt + nFeature_plt + nRna_plt + plot_layout(nrow=3)

}


transfer_labels_multi = function(query_list, ref.integrated, groupby, query.integrated = NULL) {
    ref.integrated = FindVariableFeatures(ref.integrated)
    ref.integrated = RunPCA(ref.integrated, npcs = 30, verbose = TRUE)
    ref.integrated = FindNeighbors(object = ref.integrated, dims = 1:20)
    ref.integrated = RunUMAP(ref.integrated, reduction = "pca", dims = 1:20, return.model = TRUE)
    pi = 1
    plts = list()
    for (i in 1:length(query_list)) {
        query <- query_list[[i]]
        anchors <- FindTransferAnchors(reference = ref.integrated, query = query, dims = 1:20, reference.reduction = "pca")
        message("1")
        #print(ref.integrated[[groupby]])
        predictions_pca <- TransferData(anchorset = anchors, refdata = ref.integrated[[]][,groupby], dims = 1:20, k.weight = 10)
        message("2")
        query<- MapQuery(anchorset = anchors, reference = ref.integrated, query = query, refdata=ref.integrated[[]][,groupby], reference.reduction = "pca", reduction.model = "umap", transferdata.args = list(k.weight=10))
        query <- AddMetaData(query, metadata = predictions_pca)
        unique_groupby = unique(ref.integrated[[]][,groupby])
        pt_colors = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique_groupby))
        names(pt_colors) = unique_groupby
        refumap = DimPlot(ref.integrated, reduction = "umap", group.by = groupby, label = TRUE, label.size = 3, repel = TRUE) + NoLegend() + ggtitle("Reference annotations") + scale_color_manual(values = pt_colors)
        print(colnames(query[[]]))
        queryumap = DimPlot(query, reduction = "ref.umap", group.by = "predicted.id", label = TRUE, label.size = 3, repel = TRUE) + NoLegend() + ggtitle("query transferred labels") + xlim(layer_scales(refumap)$x$range$range) + ylim(layer_scales(refumap)$y$range$range) + scale_color_manual(values = pt_colors)
        plts[[pi]] = list(refumap,queryumap)
        query = FindVariableFeatures(query)
        query = RunPCA(query, npcs = 30, verbose = TRUE)
        query = FindNeighbors(object = query, dims = 1:20)
        query = FindClusters(object = query, resolution = c(0.1, 0.2, 0.3,  0.4, 0.5, 0.6, 0.8))
        query = RunUMAP(query, reduction = "pca", dims = 1:20, verbose = TRUE, umap.method = "umap-learn", metric = "correlation")

        score_colnames=colnames(query[[]])[which(startsWith(colnames(query[[]]), "prediction.score"))]
        nonzero_score_colnames=score_colnames[which(!sapply(score_colnames, function(c) all(query@meta.data[,c] == 0)))]
        nonzero_score_colnames
        plts[[pi+1]] = FeaturePlot(query, features = nonzero_score_colnames, combine = FALSE, reduction = "umap")
        plts[[pi+1]] = lapply(plts[[pi+1]], function(p) p+scale_color_gradient(low = "lightgrey", high = "blue", limits=c(0,1)))
        umap_pred_annot = query[["predicted.id"]]
        rownames(umap_pred_annot) = paste0(rownames(umap_pred_annot), sprintf("_%d", i))
        print(rownames(umap_pred_annot))
        
        if (!is.null(query.integrated)) {
            query.integrated <- AddMetaData(query.integrated, metadata = umap_pred_annot)
            plts[[pi+2]] = DimPlot(query.integrated, reduction = "umap", group.by = "predicted.id", cols = DiscretePalette(n=8, palette="glasbey"))
            pi = pi+3
        } else {
            pi = pi+2
        }
    }
    return(plts)
}

