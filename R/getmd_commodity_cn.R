#' @importFrom readr read_lines
getmd_commodity1_sina = function(symbol) {
    doc = V1 = NULL
    
    urli = sprintf("http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesDailyKLine?symbol=%s", symbol)
    
    cols_names = c("date", "open", "high", "low", "close", "volume")
    
    dt = data.table(doc = read_lines(urli))[
        , strsplit(gsub("\"|\\]\\]|\\[\\[","",doc), "\\]\\,\\[")
    ][, (cols_names) := tstrsplit(V1, ",", fixed=TRUE)
    ][, (cols_names[-1]) := lapply(.SD, as.numeric), .SDcols = cols_names[-1]][, `:=`(
        date = as.Date(date), V1 = NULL
    )]
    
    return(dt)
}


#' @import data.table 
getmd_commodity_cn = function(symbol, print_step=1) {
    . = exchange = board = name = md_list = NULL
    
    symbol_len = length(symbol)
    for (i in 1:symbol_len) {
        si = symbol[i]
        # print
        if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i,symbol_len)),collapse = "/"), si,"\n")
        
        md_list[[si]] = getmd_commodity1_sina(si)
    }
    return(md_list)
}
#' get commodity market data
#' 
#' \code{getmd_commodity} gets Chinese commodity future data.
#' 
#' @param symbol symbols of Chinese commodity future. For more commodity symbols go to \url{http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html}. 
#' @param print_step A non-negative integer, which will print variable names by each print_step-th iteration. Default is 1. 
#' @param region only cn (China) is available.
#' 
#' 
#' @examples 
#' \dontrun{
#' dat = getmd_commodity(symbol='RB0')
#' 
#' dat = getmd_commodity(symbol=c("AG0","AU0"))
#' }
#' 
#' @export
getmd_commodity = function(symbol, print_step=1, region="cn") {
    if (region == "cn") return(getmd_commodity_cn(symbol, print_step))
}

#' get commodity symbols
#' 
#' \code{getmd_commodity_symbol} gets the symbols commodity future in Dalian Commodity Exchange (dce), Shanghai Futures Exchange (shfe), Shanghai Gold Exchange (sge), Zhengzhou Commodity Exchange (zce). For more commodity symbols go to \url{http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html}.
#' 
#' @examples 
#' \dontrun{
#' dt = getmd_commodity_symbol()}
#' 
#' @import data.table
#' @export
getmd_commodity_symbol = function() {
    .=exchange=board=symbol=name=NULL
    
    df_symbol = setDT(copy(symbol_commodity_sina))[,.(
        market='commodity', submarket=NA, region="cn", exchange, board, symbol, name)]
    
    return(df_symbol)
}
