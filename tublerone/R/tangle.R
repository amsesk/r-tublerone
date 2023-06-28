library(ggtree)
library(ggplot2)
library(gtable)
library(phytools)
library(tidyverse)

setwd("/home/ubuntu/mnt/general/paris")
S = read.newick("NewHanta_S.trim.contree")
M = read.newick("NewHanta_M.trim.contree")
SM = read.newick("NewWorld_Hanta_SM.msa.contree")

S$tip.label = sapply(strsplit(S$tip.label, "_", fixed=T), "[[", 1)
M$tip.label = sapply(strsplit(M$tip.label, "_", fixed=T), "[[", 1)
SM$tip.label = sapply(strsplit(SM$tip.label, "_", fixed=T), "[[", 1)

S = ape::root(S, outgroup = "Seoul")
M = ape::root(M, outgroup = "Seoul")
SM = ape::root(SM, outgroup = "Seoul")

names(S)
S$edge.length = rep(1, length(S$edge.length))
M$edge.length = rep(1, length(M$edge.length))
SM$edge.length = rep(1, length(SM$edge.length))

Sgt = ggtree(S)
Mgt = ggtree(M)
SMgt = ggtree(SM)

ts = list(Sgt, Mgt, SMgt)
get_tip_coords = function(ggtrees, tip_label) {
    coords = tibble()
    for (ggtree in ggtrees) {
        these_coords = as_tibble(ggtree$data[which(ggtree$data$label == tip_label),c("label", "x", "y", "branch.length")])
        these_coords$max_width = max(ggtree$data$x)
        coords = bind_rows(coords, these_coords)
    }
    return(coords)
}

apply_offsets = function(coords) {
    coords = coords %>% 
        group_by(label) %>%
        mutate(offset = cumsum(max_width)-max_width) %>%
        mutate(x = x+offset)
    #print(coords)
    #curr_offset = 0
    #for (i in 1:dim(coords)[1]) {
    #    if (i == 1) {} else {
    #        coords[i,'x'] = coords[i,'x'] + coords[i-1,'max_width'] + 1.0 + curr_offset
    #        curr_offset = curr_offset + coords[i,'max_width'] +  1.0
    #    }
    #}
    return (coords)
}

tangletime = function(ggtrees, coords) {
    plt = ggplot()
    curr_offset = 0
    for (ggtree in ggtrees) {
        max_width = max(ggtree$data$x)
        ggtree$data$x = ggtree$data$x + curr_offset
        plt = plt + geom_tree(data = ggtree$data, aes(x=x, y=y))
        curr_offset = curr_offset + max_width
    }

    coord_pairs = c()
    for (l in seq_along(coords$label)) {
        if (l == dim(coords)[1]) {
            break
        }
        this_label = coords[l,1]
        if(this_label == coords[l+1,1]) {
            print(pull(coords[l,],'x'))
            coord_pairs = rbind(coord_pairs, cbind(coords[l,'x', drop = TRUE][1], coords[l+1,'x',drop = TRUE][1], coords[l,'y',drop=TRUE][1], coords[l+1,'y',drop=TRUE][1]))
            print(coord_pairs)
            #plt = plt + geom_segment(x = coords[l,'x', drop = TRUE][0], xend = coords[l+1,'x',drop = TRUE][0], y = coords[l,'y',drop=TRUE][0], yend = coords[l+1,'y',drop=TRUE][0], color = 'red')
        } else {message('skip')}

    }
    coord_pairs=as_tibble(coord_pairs)
    colnames(coord_pairs) = c("x", "xend", "y", "yend")
    plt = plt + geom_segment(data = coord_pairs, aes(x=x, xend =xend, y=y, yend=yend), color="red")
    print(coord_pairs)
    return(plt)
}
split_segments = function(coords) {
    sample=as.vector(coords)[1]
    cv = as.numeric(as.vector(coords[-1]))
    splseg = as.numeric(c(cv[1]-cv[3], cv[2], cv[3], cv[4], cv[1], cv[2], cv[3], cv[4]))
    splseg = bind_cols(tibble(label=rep(sample, 8)),tibble(id=c(1,1,1,1,2,2,2,2)), tibble(value=splseg))
    print(splseg)
    splseg$value_type = rep(c("x", "y", "branch.length", "max_width"),2)
    #splseg
    splseg %>% pivot_wider(names_from = "value_type", values_from="value")
}

coords = lapply(S$tip.label, get_tip_coords, ggtrees = ts) 
coords = do.call(bind_rows, lapply(coords, apply_offsets)) 
coords
coords=do.call(bind_rows, apply(coords, 1, split_segments))
cairo_pdf()
tangletime(ts, coords)
dev.off()

