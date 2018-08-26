# get future data from sina
# ref: http://blog.sina.com.cn/s/blog_53d5ab970102vjj7.html

#' @importFrom readr read_lines
#' @importFrom curl curl
getmd_commodity1_sina = function(symbol, freq, from, to, handle) {
    doc = V1 = name = . = high = low = volume = NULL
    
    # symbol and name
    syb = toupper(symbol) 
    nam = setDT(copy(symbol_future_sina))[
        grepl(sub("^([A-Z]+)\\d*","\\1",syb), symbol), name]
    
    # url
    url0 = "http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFutures%s?symbol=%s"
    if (grepl("IF\\d+|TF\\d+|T\\d+|IH\\d+|IC\\d+", symbol))
        url0 = "http://stock2.finance.sina.com.cn/futures/api/json.php/CffexFuturesService.getCffexFutures%s?symbol=%s"
    urli = sprintf(url0, freq, syb)
    
    
    cols_names = c("date", "open", "high", "low", "close", "volume")
    # data 
    dt = data.table(doc = read_lines(curl(urli, handle=handle)))[
        , strsplit(gsub("\"|\\]\\]|\\[\\[","",doc), "\\]\\,\\[")
    ][, (cols_names) := tstrsplit(V1, ",", fixed=TRUE)
    ][, (cols_names[-1]) := lapply(.SD, as.numeric), .SDcols = cols_names[-1]]
    if (freq == "DailyKLine") {
        dt = dt[, date := as.Date(date)]
    } else {
        dt = dt[, date := as.POSIXct(date)]
    }
    
    dt = dt[, `:=`(
        V1 = NULL, symbol=syb, name=nam
    )][date >= from & date <= to,
     ][,.(date, symbol, name, open, high, low, close, volume)]
    
    setkeyv(dt, "date")
    return(dt)
}

# get Chinese commodity future data
#' @import data.table 
getmd_sina = function(symbol, freq="daily", from = "1900-01-01", to = Sys.Date(), print_step=1L) {
    
    handle = handle_new_session(url="http://sina.com")
    # from to
    from = check_fromto(from)
    to = check_fromto(to)
    
    # frequency
    freq = check_arg(freq, c("5m","15m","30m","60m","daily"))
    if (freq == "daily") {
        freq = "DailyKLine"
    } else {
        freq = paste0("MiniKLine",freq)
        from = check_fromto(from, type="time")
        to = check_fromto(to+1, type="time")
    }
    
    
    dat_list = load_dat_loop(symbol, "getmd_commodity1_sina", args = list(handle = handle, freq = freq, from = from, to = to), print_step=print_step)
    
    return(dat_list)
}


# get commodity symbols
# 
# \code{getmd_commodity_symbol} gets the symbols commodity future in Dalian Commodity Exchange (dce), Shanghai Futures Exchange (shfe), Shanghai Gold Exchange (sge), Zhengzhou Commodity Exchange (zce). For more commodity symbols go to \url{http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html}.
#
#' @import data.table
getmd_symbol_sina = function(market=NULL) {
    # cat("More commodity symbols go to\n", "http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html\n\n")
    
    .=exchange=board=symbol=name=NULL
    
    df_symbol = setDT(copy(symbol_future_sina))[,.(
        market='future', submarket=NA, region="cn", exchange, board, symbol, name)]
    
    return(df_symbol)
}
