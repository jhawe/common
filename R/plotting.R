#-------------------------------------------------------------------------------
#' Helper ot quickly create a heatmap for a 'probability' like data matrix,
#' i.e. where the values range between 0 and 1
#'
#' @param data The data matrix for which the values are to be plotted
#' @param cluster Whether to perform clustering for rows and cols. Default: FALSE
#' @param dilution Dilution of colors. Think of it as 'how many colors to use'. Default: 20
#' @param ... Additional parameters to be passed to pheatmap()
#'
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#-------------------------------------------------------------------------------
probability_map <- function(data, cluster = F,
                            dilution = 20, ...) {
  require(pheatmap)
  require(RColorBrewer)

  if(dilution < 2) stop("Dilution value must be larger than 1")

  # get custom color definition
  bk <- seq(0, 1, length=dilution)
  bp <- brewer.pal(9, "Purples")
  cols <- colorRampPalette(bp[c(1,5,9)])(dilution)
  pheatmap(data, cluster_cols=cluster, cluster_rows = cluster,
           breaks=bk, color = cols, ...)

}

#-------------------------------------------------------------------------------
#' Helper ot quickly create a heatmap for a 'correlation' like data matrix,
#' i.e. where the values range between -1 and 1
#'
#' @param data The data matrix for which the values are to be plotted
#' @param cluster Whether to perform clustering for rows and cols
#' @param dilution Dilution of colors. Think of it as 'how many colors to use'
#' @param ... Additional parameters to be passed to pheatmap()
#'
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#-------------------------------------------------------------------------------
correlation_map <- function(data, cluster = F,
                            dilution = 20, ...) {
  require(pheatmap)
  require(RColorBrewer)

  if(dilution < 2) stop("Dilution value must be larger than 1")

  # get custom color definition
  bk <- seq(-1, 1, length=dilution)
  bp <- brewer.pal(7, "RdYlBu")
  cols <- colorRampPalette(bp[c(1,4,7)])(dilution)
  pheatmap(data, cluster_cols=cluster, cluster_rows = cluster,
           breaks=bk, color = cols, ...)
}

#-------------------------------------------------------------------------------
#' Creates a straight forward t-SNE plot using the supplied dimensions and 
#' annotation
#'
#' @param dim1 The first dimension obtained from the t-SNE output
#' @param dim2 The second dimension obtained from the t-SNE output
#' @param annotation The annotation for the individual samples. Must be same 
#' length as dim1 and dim2
#' @param title The title to be used
#' @param theme The ggplot theme to be used. Default: theme_bw()
#' @param dim3 Optional third dimension from the tSNE output. Will be used for
#' coloring the points and the annotation information will be shown as shapes.
#' Default: NULL
#'
#' @author Johann Hawe <johann.hawe@gmail.com>
#'
#-------------------------------------------------------------------------------
plot_tsne <- function(dim1, dim2, annotation, 
                      title, theme=theme_bw(), dim3=NULL) {
  require(ggplot2)

  toplot <- cbind.data.frame(dim1, dim2, label=annotation)
  toplot$dim3 <- dim3

  gp <- ggplot(toplot, aes(x=dim1, y=dim2))
  
  if(!is.null(dim3)) {
    gp <- gp + aes(color=dim3, shape=label)
  } else {
    gp <- gp + aes(color=label)
  }
  gp <- gp + geom_point() + ggtitle(title) + theme
  print(gp)
}
