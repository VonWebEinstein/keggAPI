#' Get Images from KEGG
#'
#' Get Images of Pathways, Compounds, etc, from KEGG
#' @include trick.R
#' @export
#' @import utils
#' @param keywords string, or character array. Corresponding IDs.
#' @param path string. The path to save images.
#' @param dispaly logical. Display images in R window or not.
#'
#' @details
#' Using '+' to add multiple IDs in keywords string.
#'
#' Images of pathway won't dispaly in R window, causing it's usually too large.
#' They will be saved in path \code{/image} by default. Of course, all images from
#' \code{keggGetImage()} will be saved.
#' @examples
#' # retrieves the png image file of a pathway map
#' keggGetImage('hsa05130')
#'
#' # retrieves the gif image file of a compound
#' keggGetImage('C00002')
#'
#' # retrieves image files of multiple pathway maps or compoumnds.
#' keggGetImage(c('hsa05130', 'C00002'))
#' # same as above
#' keggGetImage('hsa05130+C00002')
#'

keggGetImage <- function(keywords, path = getwd(), display = FALSE){
  # corece keywords to an array
  if(length(keywords) == 1 && grepl('\\+', keywords[1])){
    keywords = str_split(keywords, '\\+', simplify = TRUE)
  }
  lapply(keywords, FUN = function(keyword)
                    keggGetSingleImage(keyword = keyword,
                                       path = path,
                                       display = display))
  return(0)
}

keggGetSingleImage <- function(keyword, path = getwd(), display = FALSE){
  url = str_c('http://rest.kegg.jp/get/', keyword, '/image')
  # suffix of image
  if(typeofEntry(keyword) == 'Pathway'){
    imageSuffix = '.png'
  }
  else{
    imageSuffix = '.gif'
  }

  # creat directory if doesn't exist
  validateAndCreatDir(str_c(path, "/keggImage/"))
  # download image
  fileName = str_c(path, '/keggImage/', keyword, imageSuffix)
  download.file(url, fileName, mode = 'wb')
  # display image

  return(0)
}

