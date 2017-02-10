# parse kegg returned result, and transform them to a dataframe

parse_KEGG_URL <- function(url,                    # completed url
                            splitPattern = '\\t',  # regular expression in split
                            isNumeric = NULL,      # which columns are numeric
                            colNames = NULL,       # colnames of returned dataframe
                            ...){                  # additional parameters to readLines()

  lines = readLines(url, ...)
  dtframe = str_split(lines, splitPattern, simplify = T)
  dtframe = as.data.frame(dtframe)
  # coerced to numeric
  if(!is.null(isNumeric)){
    isNumeric = as.logical(isNumeric)
    dtframe[,isNumeric] = lapply(dtframe, FUN = as.numeric)
  }

  if(!is.null(colNames)){
    colnames(dtframe) = colNames
  }
  return(dtframe)

}
