#' @importFrom readr read_lines
#' @importFrom curl curl
getmd_commodity1_sina = function(symbol, frequency, from, to, handle) {
    doc = V1 = NULL
    
    urli = sprintf("http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesDailyKLine?symbol=%s", symbol)
    # ref: http://blog.sina.com.cn/s/blog_53d5ab970102vjj7.html
    
    cols_names = c("date", "open", "high", "low", "close", "volume")
    
    dt = data.table(doc = read_lines(curl(urli, handle=handle)))[
        , strsplit(gsub("\"|\\]\\]|\\[\\[","",doc), "\\]\\,\\[")
    ][, (cols_names) := tstrsplit(V1, ",", fixed=TRUE)
    ][, (cols_names[-1]) := lapply(.SD, as.numeric), .SDcols = cols_names[-1]][, `:=`(
        date = as.Date(date), V1 = NULL
    )][date >= from & date <= to,]
    
    return(dt)
}

# get Chinese commodity future data
#' @import data.table 
getmd_sina = function(symbol, frequency="daily", from = "1900-01-01", to = Sys.time(), print_step=1L) {
    
    handle = handle_new_session(url="http://sina.com")
    
    dat_list = load_dat_loop(symbol, "getmd_commodity1_sina", args = list(handle = handle, frequency=frequency, from = from, to = to), print_step=print_step)
    
    return(dat_list)
}


# get commodity symbols
# 
# \code{getmd_commodity_symbol} gets the symbols commodity future in Dalian Commodity Exchange (dce), Shanghai Futures Exchange (shfe), Shanghai Gold Exchange (sge), Zhengzhou Commodity Exchange (zce). For more commodity symbols go to \url{http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html}.
#
#' @import data.table
getmd_symbol_sina = function() {
    # cat("More commodity symbols go to\n", "http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html\n\n")
    
    .=exchange=board=symbol=name=NULL
    
    df_symbol = setDT(copy(symbol_commodity_sina))[,.(
        market='commodity', submarket=NA, region="cn", exchange, board, symbol, name)]
    
    return(df_symbol)
}
