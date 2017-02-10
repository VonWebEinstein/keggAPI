#' Get KEGG Database or Entry List
#'
#' Returns a list of entry identifiers and associated definition
#' for a given database or a given set of database entries.
#' @include parse_kEGG_URL.R
#' @export

#' @param x string. Database(s) or entry[ies], using '+' to join.
#' @return dataframe. The table of particular contents.
#' @examples
#' # returns the list of reference pathways
#' keggList('pathway')
#'
#' # returns the list of human pathways
#' keggList('pathway/hsa')
#'
#' # returns the list of a human gene and an E.coli O157 gene
#' keggList('hsa:10458+ece:Z5100')
#'

keggList = function(keywords){
  url = str_c('http://rest.kegg.jp/list/', keywords)
  parse_KEGG_URL(url)
}
