# query future data from sina
# ref: http://blog.sina.com.cn/s/blog_53d5ab970102vjj7.html

#' symbol of future market data
#' 
#' \code{md_future_symbol} search the symbols in future market indicators that provided by sina finance only currently.
#' 
#' @examples
#' \dontrun{
#' # interactivly search future market symbols
#' sybs = md_future_symbol()
#' 
#' }
#' 
#' @export
md_future_symbol = function() {
    .=board=symbol=name=exchange=NULL
    
    # type = check_arg(type, c('financial', 'energy', 'metal', 'grain', 'soft', 'other'), default = NULL, arg_name = 'future type')
    
    # cat('More commodity symbols go to\n', 'http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html\n\n')
    syb_dt = setDT(copy(symbol_future_sina))[, .(board, symbol, name, exchange)][order(board, exchange, symbol)]
    syb = select_rows_df(syb_dt, column='symbol')
    return(syb)
}

#' @importFrom readr read_lines
#' @importFrom curl curl
md_future1_sina = function(symbol, freq, from, to, handle, ...) {
    doc = V1 = name = . = high = low = volume = NULL
    
    # symbol and name
    syb = toupper(symbol)
    nam = setDT(copy(symbol_future_sina))[symbol == syb, name]
        # setDT(copy(symbol_sina))[
        # grepl(sub('^([A-Z]+)\\d*','\\1',syb), symbol), name]
        # 
    # url
    url0 = 'http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFutures%s?symbol=%s'
    if (grepl('IF\\d+|TF\\d+|T\\d+|IH\\d+|IC\\d+', syb))
        url0 = 'http://stock2.finance.sina.com.cn/futures/api/json.php/CffexFuturesService.getCffexFutures%s?symbol=%s'
    urli = sprintf(url0, freq, syb)
    
    
    cols_names = c('date', 'open', 'high', 'low', 'close', 'volume')
    # data 
    dt = data.table(doc = read_lines(curl(urli, handle=handle)))[
        , strsplit(gsub('\'|\\]\\]|\\[\\[','',doc), '\\]\\,\\[')
    ][, (cols_names) := tstrsplit(V1, ',', fixed=TRUE)
    ][, lapply(.SD, function(x) gsub('\\"', '', x))
    ][, (cols_names[-1]) := lapply(.SD, as.numeric), .SDcols = cols_names[-1]]
    if (freq == 'DailyKLine') {
        dt = dt[, date := as.Date(date)]
    } else {
        dt = dt[, date := as.POSIXct(date)]
    }
    
    dt = dt[, `:=`(
        V1 = NULL, symbol=syb, name=nam
    )][date >= from & date <= to,
     ][,.(symbol, name, date, open, high, low, close, volume)]
    
    setkeyv(dt, 'date')
    return(dt[,unit := 'CNY'])
}

#' query future market data
#' 
#' \code{md_future} query future market prices data. Only Chinese future market has been considered currently.
#' 
#' @param symbol symbols of future market data. It is available via function \code{md_future_symbol} or its website. Default is NULL. 
#' @param source the data source is sina finance (\url{https://finance.sina.com.cn/futuremarket/}). 
#' @param freq the frequency of NBS indicators, including '5m','15m','30m','60m','daily'. Default is 'daily'.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is '3y'.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L. 
#' 
#' @examples 
#' \dontrun{
#' dt1 = md_future(symbol = c('J0', 'RB0', 'M0', 'CF0', 'IH0', 'IF0', 'IC0'))
#' 
#' # interactivly choose symbols
#' dt2 = md_future()
#' }
#' 
#' 
#' @import data.table 
#' @export
md_future = function(symbol=NULL, source='sina', freq='daily', date_range='3y', from=NULL, to=Sys.Date(), print_step=1L) {
    check_internet('www.sina.com.cn')
    # arguments
    ## symbol
    syb = c()
    while (length(syb) == 0) {
        if (is.null(symbol)) symbol = md_future_symbol()[, symbol]
        syb = intersect(toupper(symbol), setDT(copy(symbol_future_sina))$symbol)
    }
    
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = '1000-01-01', default_date_range = '3y')
    from = ft$f
    to = ft$t
    ## handle
    hd = new_handle()#handle_new_session(url='http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html')
    ## frequency
    freq = check_arg(freq, c('5m','15m','30m','60m','daily'))
    if (freq == 'daily') {
        freq = 'DailyKLine'
    } else {
        freq = paste0('MiniKLine',freq)
        from = check_fromto(from, type='time')
        to = check_fromto(to+1, type='time')
    }
    
    # data
    dat_list = load_dat_loop(syb, 'md_future1_sina', args = list(handle = hd, freq = freq, from = from, to = to), print_step=print_step)
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