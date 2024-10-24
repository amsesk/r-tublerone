#' @importFrom Matrix readMM
#' @importFrom Seurat reateSeuratObject
#' @export

mtx_to_seurat = function(
                        mtx,
                        obs,
                        var,
                        barcode_column = "barcode",
                        feature_column = "feature"
                        ) {

  counts = Matrix::t(Matrix::readMM(mtx))
  cell.metadata = read.table(
                         obs,
                         sep = ",",
                         header = T
  )
  feature.metadata = read.table(
                         var,
                         sep = ",",
                         header = T
                         )
  colnames(counts) = cell.metadata[,barcode_column]
  rownames(counts) = feature.metadata[,feature_column]
  
  rownames(cell.metadata) = cell.metadata[,barcode_column]

  ref_so = Seurat::CreateSeuratObject(counts = counts, meta.data = cell.metadata)

  ref_so

}
