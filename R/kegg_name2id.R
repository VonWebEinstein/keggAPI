#' Convert gene(compound, pathway, drug, etc.) symbol or name to kegg id
#'
#' As the symbols and names are not unique in a sense. Here we refer to the characters
#' that wo can match in kegg database strictly. And if \code{database} is specified to concrete
#' sub-database, conversion is faster.
#' @param names list of names to match.
#' @param database string. Database to search in.
#' @param ignore.case logical. Case sensitivity or not.
#' @return dataframe. \code{found} represents different status. \code{0}: doesn't
#' find something matched. \code{1}: found only 1 entry match. \code{2}: found more
#' than 1 entries matched, and we only take the first matched id. \code{-1}:
#' Bad request.
#' @include parse_KEGG_URL.R
#' @export
#' @examples
#' # "Rb1" and "pRb" are the same gene of mouse(mmu)
#' kegg_name2id(c("aaaa","Rb1","pRb"), "mmu")

kegg_name2id <- function(names = "",
                         database = "ko",
                         ignore.case = FALSE){
  dt = do.call(rbind, lapply(names, FUN = name2id,
                            database = database,
                            ignore.case = ignore.case))
  dt$id = as.character(dt$id)
  # dt = sapply(names, FUN = name2id,
  #             database = database,
  #             ignore.case = ignore.case)
  return(dt)
}
name2id <- function(name = "",              # single entry to find
                    database = "ko",
                    ignore.case = FALSE){    # in which database

  # check params
  if(!str_detect(name, "\\w")){
    return(data.frame(found = -1, id = ""))
  }
  result = parse_KEGG_URL(
             url=str_c("http://rest.kegg.jp/find/", database, "/", name))
  # empty result or not
  if(is.null(result)){
    # 0 means find nothing, 1 means find one only,
    # 2 means find more than 1, just take the first
    # "" is the matched id
    return(data.frame(found = 0, id = ""))
  }
  # ignore case
  if(ignore.case){
    name = str_to_lower(name)
    result = str_to_lower(result)
  }
  # find exact name
  # foundResult = result[str_detect(result[,2], str_c("\\)\\s", name, "[,;<]")), ]
  splited = str_split(result[,2], '[,;]\\s')
  whichHas = sapply(splited, function(s)name %in% s)
  foundResult = result[which(whichHas), ]
  if(nrow(foundResult) == 0)
    return(data.frame(found = 0, id = ""))
  if(nrow(foundResult) == 1)
    return(data.frame(found = 1, id = as.character(foundResult[1,1])))
  if(nrow(foundResult) > 1)
    return(data.frame(found = 2, id = as.character(foundResult[1,1])))
}

