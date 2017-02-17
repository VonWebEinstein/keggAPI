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

# creat directory if doesn't exist
# dic, completed directory
validateAndCreatDir <- function(dir){
  if(!dir.exists(dir))
    dir.create(dir)
}

# gradual colors in RGB
gradientRGB <- function(start = "#000000",
                        end = "#ffffff",
                        N = 10){
  l =nchar(start)
  start = str_sub(start, l-5, l)
  end = str_sub(end, l-5, l)
  start = as.numeric(str_c("0x",c(str_sub(start,1,2),
                                  str_sub(start,3,4),
                                  str_sub(start,5,6))))
  end = as.numeric(str_c("0x",c(str_sub(end,1,2),
                                  str_sub(end,3,4),
                                  str_sub(end,5,6))))
  colors = matrix(0, nrow = N, ncol = 3)
  colors[,1] = seq(start[1], end[1], length.out = N)
  colors[,2] = seq(start[2], end[2], length.out = N)
  colors[,3] = seq(start[3], end[3], length.out = N)
  # convert to string
  colors = matrix(as.character(as.hexmode(round(colors))), nrow = N, ncol = 3)
  colors = str_c("#", str_c(colors[,1], colors[,2], colors[,3]))

  return(colors)
}
