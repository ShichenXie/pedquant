# China securities index, csindex

#' get constituent of securities index
#' 
#' \code{getmd_index_cons} provides an interface to get the current constituent of securities index. 
#' 
#' @param symbol the symbol of securities index. It supports Chinese securities index only at this moment.
#' 
#' @source \url{http://www.csindex.com.cn/zh-CN}
#' 
#' @examples 
#' \dontrun{
#' dt50 = getmd_index_cons("000016")
#' 
#' dt300 = getmd_index_cons("000300")
#' 
#' dt500 = getmd_index_cons("000905")
#' }
#' 
#' @import data.table
#' @export
#' 
getmd_index_cons = function(symbol) {
    exchange = NULL
    
    path = sprintf("http://www.csindex.com.cn/uploads/file/autofile/cons/%scons.xls",symbol)
    dat = load_read_xl(path)
    setDT(dat)
    setnames(dat, c("date","index_symbol","index_name","index_name_eng","constituent_symbol","constituent_name", "constituent_name_eng","exchange"))
    dat = dat[, exchange := ifelse(exchange=="SHH","sse", ifelse(exchange=="SHZ","szse", exchange))]
    
    cat("For more detials go to:", sprintf("\nhttp://www.csindex.com.cn/zh-CN/indices/index-detail/%s", symbol), "\n")
    return(dat)
}
