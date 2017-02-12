# set of vary usefull tricks

# what is type of the given entry, 'Pathway' or something
typeofEntry <- function(entry){
  # read only the 1th line
  tmp = readLines(str_c('http://rest.kegg.jp/get/', entry), n = 1)
  return(str_extract(tmp, '\\b\\w+$'))
}

# apply function to each columns
colApply <- function(dat, cols = colnames(dat),
                     func = function(x) as.numeric(as.character(x))) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
