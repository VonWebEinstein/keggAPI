#' Color Pathway
#'
#' Color Pathway is an advanced version of the KEGG pathway mapping tool,
#' where given objects (genes, proteins, compounds, glycans, reactions, drugs, etc.)
#' are searched against KEGG pathway maps and found objects are marked in any background
#' and foreground colors (bgcolor and fgcolor).
#' @include trick.R
#' @export
#' @import RCurl
#' @usage
#' keggColor(map, mapping_list = NULL, mode = "color", log = 0,
#'           numericalType = "mm", minColor = "#ffff00", maxColor = "#ff0000",
#'           negativeColor = "#00ff00", zeroColor = "#ffff00", positiveColor = "#ff0000")
#'
#' keggColor(map, filename = NULL, mode = "color", log = 0,
#'           numericalType = "mm", minColor = "#ffff00", maxColor = "#ff0000",
#'           negativeColor = "#00ff00", zeroColor = "#ffff00", positiveColor = "#ff0000")
#' @param map string. Pathway ID.
#' @param mapping_list string. The coded entries and colors/numbers.
#' @param filename string. File that contains entries and colors/numbers, same effect as mapping_list.
#' @param mode "color" or "number". Use "Color specification" or "Numerical value".
#' @param log 1 or 0. Convert to log scale or not.
#' @param numericalType "mm" or "nzp". Gradation with minimum-maximum or negative-zero-positive.
#' @param minColor,maxColor Give the colors at minimum and maixmum.
#' @param negativeColor,zeroColor,positiveColor Give the colors at negative, zero and positive.
#' @param path string. Path to save images.
#' @details
#' For each entry, the format is \code{/keggid\%09bgcolor,fgcolor}
#' or \code{/keggid\%09number} in mapping_list, the format is \code{keggid bgcolor,fgcolor}
#' or \code{keggid number} in each line in the file.
#'
#' When mode = "number" and numericalType = "mm"(Gradation with minimum-maximum),
#' we just need minColor and maxColor.
#' When mode = "number" and numericalType = "nzp"(Gradation with negative-zero-positive),
#' we just need negativeColor, zeroColor and positiveColor.
#' When mode = "color", we need none of them.
#' @examples
#' # "%09" represents TAB in ASCII code
#' # Use "%23" in ASCII code instead of "#" for color specification.
#' keggColor(map = "map00400",
#'           mapping_list = "/1.14.16.1%09,blue/C00079%09,red/C00166%09%23005050")
#'
#'
keggColor <- function(map,
                      mapping_list = NULL,
                      filename = NULL,
                      mode = "color",
                      log = 0,
                      numericalType = "mm",
                      minColor = "#ffff00",
                      maxColor = "#ff0000",
                      negativeColor = "#00ff00",
                      zeroColor = "#ffff00",
                      positiveColor = "#ff0000",
                      path = getwd()
                      ){
  # validate the parameters
  if((is.null(mapping_list) && is.null(filename)) ||
     (!is.null(mapping_list) && !is.null(filename))){
    stop("We need and only need one of mapping_list and filename.")
  }

  # give mapping_list
  if(!is.null(mapping_list)){
    # Color specification
    if(mode == "color"){

      downloadPNG(map = map, mapping_list = mapping_list, path = path)

    }
    # Numerical value
    else{
      # convert mapping_list if needed
      if(str_detect(mapping_list, "/[a-zA-Z0-9\\.]+%09[0-9\\.]")){
        # extract numbers
        mapping_list = str_split(mapping_list, "/", simplify = TRUE)[-1]
        numbers = str_match(mapping_list, "%09(\\d*.?\\d*)")[,2]
        numbers = as.numeric(numbers)

        colors = num2Color(numbers, log = log,
                             numericalType = numericalType,
                             minColor = minColor,
                             maxColor = maxColor,
                             negativeColor = negativeColor,
                             zeroColor = zeroColor,
                             positiveColor = positiveColor)

        mapping_list = str_replace(mapping_list, "%09(\\d*.?\\d*)",
                                     str_c("%09", colors))
        mapping_list = str_c("/", mapping_list, collapse = "")
      }

      downloadPNG(map = map, mapping_list = mapping_list, path = path)
    }
  }
  # give filename
  else{
    # Color specification
    if(mode == "color"){
      mapping_list = file2mapping_list(filename)
      downloadPNG(map = map, mapping_list = mapping_list, path = path)

    }
    # Numerical value
    else{
      entry = read.table(filename)
      colors = num2Color(entry[,2], log = log,
                         numericalType = numericalType,
                         minColor = minColor,
                         maxColor = maxColor,
                         negativeColor = negativeColor,
                         zeroColor = zeroColor,
                         positiveColor = positiveColor)
      mapping_list = str_c("/",
                           str_c(entry[,1], colors, sep = "%09", collapse = "/"))

      downloadPNG(map = map, mapping_list = mapping_list, path = path)
    }
  }
}





# convert file to mapping_list
file2mapping_list <- function(filename){
  lines = readLines(filename)
  lines = str_replace(lines, "\\s", "%09")
  mapping_list = str_c("/", lines, collapse = "")
}

downloadPNG <- function(map, mapping_list, path = getwd()){
  str = getURL(str_c("http://www.kegg.jp/kegg-bin/show_pathway?", map, mapping_list))
  pngURL = str_match(str, '<img src="([^"]+)"')[,2]
  pngURL = str_c("http://www.kegg.jp", pngURL)

  # creat directory if doesn't exist
  validateAndCreatDir(str_c(path, "/keggImage/"))
  # download image
  fileName = str_c(path, '/keggImage/', map, ".png")
  download.file(pngURL, fileName, mode = 'wb')
}

num2Color <- function(numbers,
                        log,
                        numericalType,
                        minColor,
                        maxColor,
                        negativeColor,
                        zeroColor,
                        positiveColor){
  # Gradation with negative-zero-positive
  if(numericalType == "nzp"){
    colors = rep(positiveColor, time = length(numbers))
    colors[numbers < 0] = negativeColor
    colors[numbers == 0] = zeroColor
  }
  # Gradation with minimum-maximum
  else{
    if(log == 1 && any(numbers < 0)){
      stop("Numbers must be positive, when take log scale.")
    }
    if(log == 1){
      numbers = log(numbers)
    }
    colors = gradientRGB(start = minColor, end = maxColor, N = lengh(numbers))
  }

  return(colors)
}

