#' Linked Entries
#'
#' Find related entries by using database cross-references.
#' @export
#' @include parse_KEGG_URL.R
#' @usage keggLink("target_db", "source_db")
#' keggLink("target_db", "dbentries")
#' @param target_db,source_db string. database I
#' @param  dbentries string. KEGG database entries involving the database II
#' @details
#' database I = pathway | brite | module | ko | genome | <org> |
#' compound | glycan | reaction | rclass | enzyme | disease | drug |
#' dgroup | environ
#'
#' database II = pathway | brite | module | ko | genome | <org> |
#' compound | glycan | reaction | rclass | enzyme | disease |
#' drug | dgroup | environ | genes
#' @note
#' The database name "genes" may be used only in the second form.
#' @examples
#' # KEGG pathways linked from each of the human genes
#' keggLink("pathway", "hsa")
#'
#' # human genes linked from each of the KEGG pathways
#' keggLink("hsa", "pathway")
#'
#' # KEGG pathways linked from a human gene and an E. coli O157 gene
#' keggLink("pathway", "hsa:10458+ece:Z5100")
#'
#' # List of genes with the KO assignment of K00500
#' keggLink("genes", "K00500")
#'
#' # List of human genes in pathway hsa00010
#'
#' keggLink("genes", "hsa00010")
#' # or
#' keggLink("hsa", "hsa00010")
#'
#' # List of KO entries in pathway map00010 or ko00010
#' keggLink("ko", "map00010")
#' keggLink("ko", "ko00010")
#'
#' # List of reaction entries in pathway map00010 or rn00010
#' keggLink("rn", "map00010")
#' keggLink("rn", "rn00010")
#'
#' # List of EC number entries in pathway map00010 or ec00010
#' keggLink("ec", "map00010")
#' keggLink("ec", "ec00010")
#'
#' # List of compound entries in pathway map00010
#' keggLink("cpd", "map00010")
#'

keggLink <- function(target_db, source_dbentr, ...){
  # coerce source_dbentr into a single string pasted with "+"
  if(length(source_dbentr) > 1){
    source_dbentr = str_c(source_dbentr, collapse = "+")
  }
  url = str_c("http://rest.kegg.jp/link/", target_db, "/", source_dbentr)
  # judge source_dbentr is a database or some entries
  if(str_detect(source_dbentr, "\\d")){
    colNames = c("target_db", "entry")
  }
  else{
    colNames = c("target_db", "souece_db")
  }

  dtframe = parse_KEGG_URL(url, colNames = colNames, ...)

}
