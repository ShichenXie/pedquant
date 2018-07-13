# China securities index, csindex

#' get current constituent of China securities index.
#' 
#' \code{getmd_csi_cons} provides an API to get the current constituent of China securities index. 
#' 
#' @param symbol the code of China securities index.
#' @source \url{http://www.csindex.com.cn/zh-CN}
#' 
#' @examples 
#' \dontrun{
#' dt50 = getmd_csi_cons("000016")
#' 
#' dt300 = getmd_csi_cons("000300")
#' 
#' dt500 = getmd_csi_cons("000905")
#' }
#' 
#' @import data.table
#' @export
#' 
getmd_csi_cons = function(symbol) {
    exchange = NULL
    
    path = sprintf("http://www.csindex.com.cn/uploads/file/autofile/cons/%scons.xls",symbol)
    dat = load_read_xl(path)
    setDT(dat)
    setnames(dat, c("date","index_code","index_name","index_name_eng","constituent_code","constituent_name", "constituent_name_eng","exchange"))
    dat = dat[, exchange := ifelse(exchange=="SHH","sse", ifelse(exchange=="SHZ","szse", exchange))]
    
    cat("For more detials go to:", sprintf("\nhttp://www.csindex.com.cn/zh-CN/indices/index-detail/%s", symbol), "\n")
    return(dat)
}
