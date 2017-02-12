
# parse kegg returned result, and transform them to a dataframe

#' @include trick.R
parse_KEGG_URL <- function(url,                    # completed url
                            splitPattern = '\\t',  # regular expression in split
                            isNumeric = NULL,      # which columns are numeric
                            colNames = NULL,       # colnames of returned dataframe
                            ...){                  # additional parameters to readLines()

  lines = readLines(url, ...)
  if(length(lines) == 1 && !nzchar(lines)){
    return(NULL)
  }
  dtframe = str_split(lines, splitPattern, simplify = T)
  dtframe = as.data.frame(dtframe)
  # coerced to numeric
  if(!is.null(isNumeric)){
    isNumeric = as.logical(isNumeric)
    dtframe = colApply(dtframe, cols = isNumeric)
    # dtframe[,isNumeric] = apply(dtframe[,isNumeric], 2, FUN = as.numeric)
  }

  if(!is.null(colNames)){
    colnames(dtframe) = colNames
  }
  return(dtframe)

}



