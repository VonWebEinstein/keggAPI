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
gradientRGB <- function(numbers,
                        start = "#000000",
                        end = "#ffffff"){
  l =nchar(start)
  start = str_sub(start, l-5, l)
  end = str_sub(end, l-5, l)
  start = as.numeric(str_c("0x",c(str_sub(start,1,2),
                                  str_sub(start,3,4),
                                  str_sub(start,5,6))))
  end = as.numeric(str_c("0x",c(str_sub(end,1,2),
                                  str_sub(end,3,4),
                                  str_sub(end,5,6))))
  N = length(numbers)
  min_number = min(numbers)
  max_number = max(numbers)
  colors = matrix(0, nrow = N, ncol = 3)
  colors[,1] = (numbers-min_number)/(max_number-min_number)*(end[1]-start[1])+start[1]
  colors[,2] = (numbers-min_number)/(max_number-min_number)*(end[2]-start[2])+start[2]
  colors[,3] = (numbers-min_number)/(max_number-min_number)*(end[3]-start[3])+start[3]
  # convert to string
  colors = matrix(as.character(as.hexmode(round(colors))), nrow = N, ncol = 3)
  colors = str_c("#", str_c(colors[,1], colors[,2], colors[,3]))

  return(colors)
}
