#' Search Data in KEGG Database
#'
#' Finds entries with matching query keywords
#' or other query data in a given database
#' @include parse_KEGG_URL.R
#' @export
#' @param keywords string. Query keywords or other query data with given database.
#' @details
#' URL Form 1:
#'
#' keywords = '<database>/<query>'
#'
#' <database> = pathway | module | ko | genome | <org> | compound | glycan |
#' reaction | rclass | enzyme | disease | drug | dgroup | environ |
#' genes | ligand
#'
#' <org> = KEGG organism code or T number
#'
#' This form searches entry identifier and associated fields.
#'
#' URL Form 2:
#'
#' keywords = '<database>/<query>/<option>'
#'
#' <database> = compound | drug
#'
#' <option> = formula | exact_mass | mol_weight
#'
#' In this form, the chemical formula search is a partial
#' match irrespective of the order of atoms given.
#' The exact mass (or molecular weight) is checked by rounding off
#' to the same decimal place as the query data.
#' A range of values may also be specified with the minus(-) sign.
#'
#' See \href{http://www.kegg.jp/kegg/rest/keggapi.html}{kegg api} for more details.
#' @return
#' A dataframe.
#' @examples
#' # for keywords "shiga" and "toxin"
#' keggFind('genes/shiga+toxin')
#'
#' # for keywords "shiga toxin"
#' keggFind('genes/"shiga toxin"')
#'
#' # for chemical formula "C7H10O5"
#' keggFind('compound/C7H10O5/formula')
#'
#' # for chemical formula containing "O5" and "C7"
#' keggFind('compound/O5C7/formula')
#'
#' # for 174.045 =< exact mass < 174.055
#' # equal in the sense of rounding to proper precision
#' keggFind('compound/174.05/exact_mass')
#'
#' # for 300 =< molecular weight =< 310
#' keggFind('compound/300-310/mol_weight')
#'

keggFind <- function(keywords){
  url = str_c('http://rest.kegg.jp/find/', keywords)
  if(any(grepl('(exact_mass|mol_weight)', keywords ))){
    isNumeric = c(FALSE, TRUE)
  }
  else{
    isNumeric = NULL
  }
  colNames = c('ID', str_extract(keywords, '[^\\/]+$'))
  dtframe = parse_KEGG_URL(url, isNumeric = isNumeric, colNames = colNames)
  return(dtframe)

}
