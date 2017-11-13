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
  m = str_match_all(str, '([A-Z]?\\d+)\\s\\(([^\\)]*?)\\)[,"]')[[1]]
  index = order(m[,2])
  m = m[index,]

  # wipe our redundance
  len = nrow(m)
  m_c2 = c(m[, 2], "XXOO")
  index = which(sapply(1:(length(m_c2)-1), function(k)m_c2[k] != m_c2[k+1]))

  dt = as.data.frame(m[index,-1], stringsAsFactors = FALSE)
  colnames(dt) = c("ID", "symbol")

  return(dt)

}
