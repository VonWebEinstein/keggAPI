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
  m = str_match_all(str, '([A-Z]?\\d{3,})\\s\\(([^\\)]*?)\\)[,"]')[[1]]
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

#' @export
#' @rdname extractComponent
#' @param s1,s2 string
#' @param ... optional arguments in \code{grep}
#' @details \code{matchSymbol} matches each item of \code{s1} in
#' \code{s2}, optional suffix \code{optional} appended. Output symbols comes from \code{s2}.

#' @examples
#' ############
#' s2 = c("ab","ab1","ab2", "bb1","vv")
#' matchSymbol(c("ab", "bb"), s2)


matchSymbol <- function(s1, s2, optional = '[0-9]?',...){
  # search each of s1 in s2
  # strings can be appended a suffix in OPTIONAL
  t1 = str_c("^", s1, optional, "$")
  tmp1 = unlist(lapply(t1, grep, s2, value = TRUE, ...))
  
  t2 = str_c("^", s2, optional, "$")
  tmp2l = sapply(t2, grepl, s1, ...)
  if(!is.matrix(tmp2l))
	tmp2l = matrix(tmp2l, nrow = 1)
  tmp2 = s2[apply(tmp2l, 2, any)]
  tmp = unique(c(tmp1, tmp2))

  return(tmp)
}
