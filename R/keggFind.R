#' Search Data in KEGG Database
#'
#' Finds entries with matching query keywords
#' or other query data in a given database
#' @export
#' @param keywords string. Query keywords or other query data with given database.
#' @details
#' URL Form 1:
#'
#' http://rest.kegg.jp/find/<database>/<query>
#'
#' <database> = pathway | module | ko | genome | <org> | compound | glycan |
#' reaction | rclass | enzyme | disease | drug | dgroup | environ |
#' genes | ligand
#'
#' <org> = KEGG organism code or T number
#'
#' URL Form 2:
#'
#' http://rest.kegg.jp/find/<database>/<query>/<option>
#'
#' <database> = compound | drug
#'
#' <option> = formula | exact_mass | mol_weight
#'
#'
keggFind <- function(){

}
