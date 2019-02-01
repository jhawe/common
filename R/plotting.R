probability_map <- function(data, cluster = F,
                            dilution = 20, cellwidth = NA) {
  require(pheatmap)
  require(RColorBrewer)

  if(diluation < 2) stop("Dilution value must be larger than 1")

  # get custom color definition
  bk <- seq(0, 1, length=dilution)
  bp <- brewer.pal(9, "Purples")
  cols <- colorRampPalette(bp[c(1,5,9)](dilution)
  pheatmap(data, cluster_cols=cluster, cluster_rows = cluster,
           breaks=bk, color = cols, cellwidth=cellwidth)

}

correlation_map <- function(data, cluster = F,
                            dilution = 20, cellwidth = NA) {
  require(pheatmap)
  require(RColorBrewer)

  if(diluation < 2) stop("Dilution value must be larger than 1")

  # get custom color definition
  bk <- seq(0, 1, length=dilution)
  bp <- brewer.pal(7, "RdYlBu")
  cols <- colorRampPalette(bp[c(1,4,7)](dilution)
  pheatmap(data, cluster_cols=cluster, cluster_rows = cluster,
           breaks=bk, color = cols, cellwidth=cellwidth)
}
