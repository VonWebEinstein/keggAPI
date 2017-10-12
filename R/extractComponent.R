#' extract all components from a kegg pathway
#'
#' extract all components from a kegg pathway
#' @param url string. String of pathway ID or detailed page.
#'
#' @export
#' @examples
#' c1 = extractComponent("mmu04933")
#' # equivalent to
#' url = "http://www.kegg.jp/kegg-bin/highlight_pathway?map=mmu04933"
#' c2 = extractComponent(url)
#'
#'
extractComponent <- function(url){
  if(!grepl("^http", url)){
    url0= 'http://www.kegg.jp/kegg-bin/highlight_pathway?map='
    url = paste0(url0, url)
  }
  # read url
  str = content(GET(url), "text")
  unique(str_match_all(str, '\\d\\s\\((.*?)\\)[,"&]')[[1]][,2])
}
