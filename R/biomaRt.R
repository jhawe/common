#' -----------------------------------------------------------------------------
#' Collection of some handy routines to query biomart.
#'
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#'
#' @date Thu Dec 13 23:12:49 2018
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Method to query biomart for snp information by genomic region.
#' Currently only gets the rsIDs.
#'
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#'
#' -----------------------------------------------------------------------------
get_snps_biomart <- function(ranges, grch=37, additional.info=F) {

  require(biomaRt)

  # create filter values
  chrs <- gsub("chr", "", as.character(seqnames(ranges)))
  start <- start(ranges)
  end <- end(ranges)

  # set filter names
  filt <- c("chr_name", "start", "end")

  # set attributes to get
  attr <- c("refsnp_id", "chr_name", "chrom_start", "chrom_end")
  if(additional.info) {
    attr <- c(attr, "consequence_type_tv")
  }

  # query biomart
  mart <- useEnsembl(biomart="snp", GRCh=grch, dataset="hsapiens_snp")
  snps <- getBM(attr, filt,
                values=list(chrs, start, end),
                mart=mart,
                uniqueRows=T)

  # set new colnames
  if(additional.info) {
    colnames(snps) <- c("snp", "chr", "start", "end", "variant_consequence")
  } else {
    colnames(snps) <- c("snp", "chr", "start", "end")
  }

  return(snps)
}

#' -----------------------------------------------------------------------------
#' Method to query biomart for snp position information by their rsIds.
#'
#' @param rsIds Vector of rsIds for which to get the genomic position info
#' @param grch The genome assembly version. Default: 37
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#'
#' -----------------------------------------------------------------------------
get_snpPos_biomart <- function(rsIds, grch=37, additional.info=F) {

  require(biomaRt)

  # create filter values
  vals <- unique(as.character(rsIds))

  # set filter names
  filt <- c("snp_filter")

  # set attributes to get
  attr <- c("refsnp_id", "chr_name", "chrom_start", "chrom_end")

  # check whether to get some additional variant information
  if(additional.info) {
    attr <- c(attr,  "consequence_type_tv")
  }
  # query biomart
  mart <- useEnsembl(biomart="snp", GRCh=grch, dataset="hsapiens_snp")
  snps <- getBM(attr, filt,
                values=list(vals),
                mart=mart,
                uniqueRows=T)
  # set new colnames
  if(additional.info) {
    colnames(snps) <- c("snp", "chr", "start", "end", "variant_consequence")
  } else {
    colnames(snps) <- c("snp", "chr", "start", "end")
  }

  return(snps)
}

#' -----------------------------------------------------------------------------
#' Method to query biomart for snp+flanking region sequences.
#'
#' @param rsIds Vector of rsIds for which to get the genomic position info
#' @param downstr 20. Size of downstream flanking region to get sequence for
#' @param upstr 20. Size of upstream flanking region to get sequence for
#' @param grch The genome assembly version. Default: 37
#'
#' @author Johann Hawe <johann.hawe@helmholtz-muenchen.de>
#'
#' -----------------------------------------------------------------------------
get_snp_flanking_biomart <- function(rsIds, downstr=20, upstr=20, grch=37) {
  require(biomaRt)

  # create filter values
  vals <- list(snp_filter=unique(as.character(rsIds)),
               upstream_flank=downstr,
               downstream_flank=upstr)

  # set filter names
  filt <- c("snp_filter", "upstream_flank", "downstream_flank")

  # set attributes to get
  attr <- c("refsnp_id", "snp")

  # query biomart
  mart <- useEnsembl(biomart="snp",
                     GRCh=grch,
                     dataset="hsapiens_snp")
  snps <- getBM(attr, filt, values=vals, mart=mart, uniqueRows=T, checkFilters=F)

  return(snps)
}
