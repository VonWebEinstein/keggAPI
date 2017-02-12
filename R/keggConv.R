#' Convert IDs
#'
#' Convert KEGG identifiers to/from outside identifiers.
#' @include parse_kEGG_URL.R
#' @export
#' @usage keggConv(target_db, source_db)
#' keggConv(target_db, dbentries)
#' @param target_db string. The database convert to.
#' @param source-db string. The database convert from.
#' @param dbentries string/char array. The IDs of entry we want to convert.
#' @details
#' When converting from database to database,
#' \code{(target_db, source_db) = (kegg_db, outside_db) | (outside_db, kegg_db)}.
#' For gene identifiers: \code{kegg_db = org, org = KEGG organism code or T number,
#' outside_db = ncbi-proteinid | ncbi-geneid | uniprot}.
#'
#' For chemical substance identifiers: \code{kegg_db = drug | compound | glycan
#' outside_db = pubchem | chebi}.
#'
#' When converting entries to target_db, For gene identifiers:
#' \code{<dbentries> = database entries involving the following <database>,
#' <database> = <org> | genes | ncbi-proteinid | ncbi-geneid | uniprot,
#' <org> = KEGG organism code or T number}
#'
#' For chemical substance identifiers:
#' \code{<dbentries> = database entries involving the following <database>
#' <database> = drug | compound | glycan | pubchem | chebi}
#'
#' @note
#' The database name "genes" may be used only in the second form
#' (convert entries to target_db).
#' @examples
#' # conversion from NCBI GeneID to KEGG ID for E. coli genes
#' gene_E.coli = keggConv('eco', 'ncbi-geneid')
#' # opposite direction
#' gene_E.coli = keggConv('ncbi-geneid', 'eco')
#'
#' # conversion from KEGG ID to NCBI ProteinID
#' keggConv('ncbi-proteinid', 'hsa:10458+ece:Z5100')
#'
#' # conversion from NCBI GeneID to KEGG ID when the organism code is not known
#' keggConv('genes', 'ncbi-geneid:19831932')
#' # same as above
#' keggConv('lpn', 'ncbi-geneid:19831932')

keggConv <- function(target_db, str_from){

  if(grepl('\\:', str_from)){
    # set colNames
    colNames = c('target_ID', 'source_ID')
    # coreced str_from to one string
    if(length(str_from) > 1)
      str_from = str_c(str_from, collapse = '+')
  }
  else{
    colNames = c('target_db', 'source_db')
  }

  url = str_c('http://rest.kegg.jp/conv/', target_db, '/', str_from)
  dtframe = parse_KEGG_URL(url, colNames = colNames)

  return(dtframe)
}
