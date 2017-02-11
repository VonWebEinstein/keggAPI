# set of vary usefull tricks

typeofEntry <- function(entry){
  # read only the 1th line
  tmp = readLines(str_c('http://rest.kegg.jp/get/', entry), n = 1)
  return(str_extract(tmp, '\\b\\w+$'))
}
