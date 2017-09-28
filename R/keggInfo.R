#' Get KEGG Database Infomation
#'
#' Displays the current statistics of a given database.
#' @return A dataframe of how many entries in each sub-database.
#' @examples
#' keggInfo()
#' @import stringr
#' @export
#'


keggInfo <- function(){
  url = "http://rest.kegg.jp/info/kegg"
  lines = readLines(url)
  lines = grep('\\w+[0-9 ,]+entries', lines, value = TRUE)
  DBName = str_extract(lines, '\\w+')
  numstr = str_extract_all(lines, '\\d')
  Entries = as.numeric(lapply(numstr, function(s) paste(s, collapse = "")))
  return(data.frame(DBName = DBName, Entries = Entries, row.names = DBName))
}
