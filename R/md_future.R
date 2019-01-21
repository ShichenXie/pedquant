# query future data from sina
# ref: http://blog.sina.com.cn/s/blog_53d5ab970102vjj7.html

#' symbols of Chinese future
#' 
#' @export
md_future_symbol = function() {
    # cat("More commodity symbols go to\n", "http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html\n\n")
    syb = setDT(copy(symbol_future_sina))[,.(exchange, board, symbol, name)]
    return(syb)
}

#' @importFrom readr read_lines
#' @importFrom curl curl
md_future1_sina = function(symbol, freq, from, to, handle, ...) {
    doc = V1 = name = . = high = low = volume = NULL
    
    # symbol and name
    syb = toupper(symbol)
    symbol_sina = md_future_symbol()
    nam = symbol_sina[symbol == syb, name]
        # setDT(copy(symbol_sina))[
        # grepl(sub("^([A-Z]+)\\d*","\\1",syb), symbol), name]
        # 
    # url
    url0 = "http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFutures%s?symbol=%s"
    if (grepl("IF\\d+|TF\\d+|T\\d+|IH\\d+|IC\\d+", syb))
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

#' get future data
#' 
#' @import data.table 
#' @export
md_future = function(symbol=NULL, freq="daily", date_range='3y', from=NULL, to=Sys.Date(), print_step=1L) {
    # arguments
    syb = toupper(symbol)
    symbol_sina = md_future_symbol()
    ## symbol
    if (is.null(symbol)) {
        syb = select_rows_df(symbol_sina[,.(symbol,name)], column='symbol')[,symbol]
    } else if (length(symbol)==1) {
        syb = select_rows_df(symbol_sina[,.(symbol,name)], column='symbol', input_string=syb)[,symbol]
    }
    syb = intersect(syb, symbol_sina$symbol)
    ## from
    date_range = check_date_range(date_range, default = "max")
    from = get_from_daterange(date_range, to, min_date = "1000-01-01")
    ## handle
    hd = new_handle()#handle_new_session(url="http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html")
    ## frequency
    freq = check_arg(freq, c("5m","15m","30m","60m","daily"))
    if (freq == "daily") {
        freq = "DailyKLine"
    } else {
        freq = paste0("MiniKLine",freq)
        from = check_fromto(from, type="time")
        to = check_fromto(to+1, type="time")
    }
    
    # data
    dat_list = load_dat_loop(syb, "md_future1_sina", args = list(handle = hd, freq = freq, from = from, to = to), print_step=print_step)
    return(dat_list)
}


# yahoo future
# 62: commodity      GC=F                            Gold
# 63: commodity      ZG=F             Gold 100 oz. Feb 19
# 64: commodity      SI=F                          Silver
# 65: commodity      ZI=F          Silver 5000 oz. Mar 19
# 66: commodity      PL=F       Platinum Futures,Apr-2019
# 67: commodity      HG=F         Copper Futures,Mar-2019
# 68: commodity      PA=F                Palladium Mar 19
# 69: commodity      CL=F                       Crude Oil
# 70: commodity      HO=F NY Harbor ULSD Futures,Feb-2019
# 71: commodity      NG=F              Natural Gas Feb 19
# 72: commodity      RB=F  RBOB Gasoline Futures,Feb-2019
# 73: commodity      BZ=F Brent Crude Oil Last Day Financ
# 74: commodity      B0=F Mont Belvieu LDH Propane (OPIS)
# 75: commodity       C=F                     Corn Jul 19
# 76: commodity       O=F                     Oats May 19
# 77: commodity      KW=F future for KW, March, 2019, tra
# 78: commodity      RR=F               Rough Rice Jan 19
# 79: commodity      SM=F Soybean Meal Futures,Mar-2019,C
# 80: commodity      BO=F Soybean Oil Futures,Mar-2019,Co
# 81: commodity       S=F                 Soybeans May 19
# 82: commodity      FC=F            Feeder Cattle Jan 19
# 83: commodity      LH=F                Lean Hogs Apr 19
# 84: commodity      LC=F              Live Cattle Feb 19
# 85: commodity      CC=F Cocoa Futures,Mar-2019,Composit
# 86: commodity      KC=F Coffee 'C' Futures,Mar-2019,Com
# 87: commodity      CT=F Cotton No. 2 Futures,May-2019,C
# 88: commodity      LB=F Random Length Lumber Futures,Ma
# 89: commodity      OJ=F Frozen Concentrated OJ (FCOJ-A)
# 90: commodity      SB=F Sugar No. 11 (World)  Futures,M