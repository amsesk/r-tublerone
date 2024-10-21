#' @title plots2pane
#'
#' @description Generates an aligned, multi-plot pane from a list of ggplots objects.
#'
#' @param plt_list List of ggplot objects.
#' @param rows Rows of pane.
#' @param cols Columns of pane.
#'
#' @return NULL
#'
#' @examples
#' x = c(1,2,3)
#' y = c(1,2,3)
#' dataset = data.frame(cbind(x,y))
#' plots = list()
#' plots[[1]] = ggplot(dataset, aes(x=x, y=y)) + geom_line()
#' plots[[2]] = ggplot(dataset, aes(x=x, y=y)) + geom_line()
#' plots2pane(plots, 1, 2)
#'
#' @importFrom ggplot2 ggplot ggplotGrob theme geom_blank
#' @importFrom grid grid.newpage grid.draw
#' @export

plots2pane<-function(plt_list, rows, cols) {
  n = length(plt_list)
  nper_row = n/rows
  sqftage = rows*cols
  blank_cell<-ggplot()+geom_blank()+theme(plot.background = element_rect(colour="white"),
                                          panel.background = element_rect(colour="white"))
  grob_list = lapply(plt_list, ggplotGrob)
  if (length(grob_list) < sqftage) {
    last<-length(grob_list)
    print(last)
    for (idx in (last+1):sqftage) {
      sprintf("added_blank")
      grob_list[[idx]]<-ggplotGrob(blank_cell)
    }
  }
  binding_expr = "rbind("
  i=1
  for (p in 1:rows) {
    columns<-rep(sprintf("grob_list[[%d]]",seq(i,(i+cols-1),1)))
    binding_expr<-str_c(binding_expr,
                        "cbind(",
                        str_c(columns,collapse=","),
                        ")"
    )
    if (p != rows) {
      binding_expr<-str_c(binding_expr, ",")
    }
    i=i+cols
  }
  binding_expr<-str_c(binding_expr,")")
  print(binding_expr)
  grid::grid.newpage()
  grid::grid.draw(eval(parse(text=binding_expr)))
}
