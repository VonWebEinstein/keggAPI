

#' @include trick.R




keggColor <- function(mapid, default = "white", dataset){
  str = translateParam(mapid = mapid, default = default, dataset = dataset)
  colorWithRipeParam(str)
}


# translate parameters
translateParam <- function(mapid, default, dataset){

}

# color pathway directly with worked(translated) parameters
colorWithRipeParam <- function(str){
  validateAndCreatDic(str_c(path, "/keggImage/"))

}
