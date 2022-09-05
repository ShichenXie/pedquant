# query future data from sina
# ref: http://blog.sina.com.cn/s/blog_53d5ab970102vjj7.html

symbol_future_sina_xchg = function() {
    exchange = type = NULL
    syb_dt = setDT(copy(symbol_future_sina))[
        , c('exchange', 'board', 'symbol', 'name'), with = FALSE
    ][exchange %in% c('DCE', 'ZCE', 'SHFE', 'CFFEX'), type := 'inner'
    ][is.na(type), type := 'global'
    ]
    syb_lst = split(syb_dt, by = 'type', keep.by = FALSE)
    syb_lst = c(split(syb_lst$inner, by = 'exchange'), syb_lst[2])
    
    return(syb_lst)
}
#' symbol of future market data
#' 
#' \code{md_future_symbol} returns all future symbols that provided by sina finance, see details on \url{http://vip.stock.finance.sina.com.cn/quotes_service/view/qihuohangqing.html} or \url{http://vip.stock.finance.sina.com.cn/mkt/#global_qh})
#' 
#' @param ... ignored parameters
#' 
#' @examples
#' \dontrun{
#' sybs = md_future_symbol()
#' }
#' 
#' @export
md_future_symbol = function(...) {
    syb_dt = symbol_future_sina_xchg()
    return(syb_dt)
}

# main future symbol name
future_symbols_sybnam = function(symbols) {
    symbol = name = exchange = NULL
    
    sybs = toupper(symbols)
    # symbol and name
    symbol_future = setDT(copy(symbol_future_sina))[
        sub('[0-9]+', '0', sybs), on = 'symbol'  
    ][, symbol := sybs
    ][exchange %in% c('DCE', 'ZCE', 'CFFEX', 'SHFE') & sub('[A-Z]+', '', symbol) != '0', 
      name := paste0(name, sub('[A-Z]+', '', symbol)) ]
    
    return(symbol_future)
}

# future infomation
md_future1_info_sina = function(symbol, ...) {
    . = name = NULL
    
    dat = read_html(sprintf('https://finance.sina.com.cn/futures/quotes/%s.shtml', symbol), encoding = 'GBK') 
    dt = html_table(dat)[[7]]
    setnames(setDT(dt), rep(c('variable','value'), 3))
    
    dtret = cbind(
        setDT(future_symbols_sybnam(symbol))[,.(symbol, name)], 
        rbind(dt[,1:2], dt[,3:4], dt[, 5:6])
    )
    return(dtret[])
}


#' @importFrom readr read_lines
#' @importFrom curl curl
md_future1_history_sina = function(symbol, name, freq, from, to, handle, ...) {
    
    syb = symbol
    sybnam = future_symbols_sybnam(symbol)
    
    # url
    # http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesDailyKLine?symbol=M0
    url0 = 'https://stock2.finance.sina.com.cn/futures/api/jsonp.php/var=/InnerFuturesNewService.get%s?symbol=%s'
    if (!(sybnam$exchange %in% c('DCE', 'ZCE', 'CFFEX', 'SHFE')))  url0 = 'https://stock2.finance.sina.com.cn/futures/api/jsonp.php/var=/GlobalFuturesService.getGlobalFutures%s?symbol=%s'
    urli = sprintf(url0, freq, syb)
    dat = try(read_lines(urli), silent = TRUE)
    
    # data 
    cols_names = c("date", "open", "high", "low", "close", "volume", "position", "settle")
    dt = setDT(fromJSON(sub('.+?(\\[.+\\]).+', '\\1', dat[2])))
    dt = setnames(
        dt, cols_names
    )[, (cols_names[-1]) := lapply(.SD, as.numeric), .SDcols = cols_names[-1]]
    
    if (freq == 'DailyKLine') {
        dt = dt[, date := as.Date(date)]
    } else {
        dt = dt[, date := as.POSIXct(date)]
    }
    
    dt = dt[, `:=`(
        symbol = syb, name = sybnam$name, unit = sybnam$unit
    )][date >= from & date <= to,
     ][, c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'settle', 'volume', 'position', 'unit'), with = FALSE]
    
    setkeyv(dt, 'date')
    return(dt[])
}


md_future_real = function(symbols, source = 'sina', ...) {
    name = syb = syb_type = symbol = exchange = time = NULL
    
    # symbols = c('CU0', 'T0', 'XAU', 'ES')
    sybnam = future_symbols_sybnam(symbols)[
        , syb_type := ''
    ][!(exchange %in% c('DCE', 'ZCE', 'CFFEX', 'SHFE')), syb_type := 'hf_'
    ][exchange == 'CFFEX', syb_type := 'CFF_'
    ][, syb := paste0(syb_type, symbol)]
    
    datlst = lapply(
        split(sybnam, by = 'syb_type'), 
        function(x) {
            # print(x)
            url = sprintf('http://hq.sinajs.cn/list=%s', x[, paste0(syb, collapse = ',')])
            # print(url)
            
            cols_name = c('name', '-', 'open', 'high', 'low', 'close_prev', 'buy', 'sell', 'close', 'settle', 'settle_prev', 'buy_vol', 'sell_vol', 'position', 'volume', 'exch_abbr', 'name_abbr', 'date', paste0('v_', 1:10))
            if (unique(x$syb_type) == 'hf_') cols_name = c('close', 'volume', 'buy', 'sell', 'high', 'low', 'time', 'close_prev', 'open', 'position', 'buy_vol', 'sell_vol', 'date', 'name', '-')
            if (unique(x$syb_type) == 'CFF_') cols_name = c('open', 'high', 'low', 'close', 'volume', paste0('v1_', 1:8), 'close_prev', paste0('v2_', 1:22), 'date', 'time', '-') 
            
            dat = read_apidata_sina(
                url, sybs = x$symbol, cols_name = cols_name)
            if (!('name' %in% names(dat))) dat = dat[, name := x$name]
            
            return(dat)
        }
    )
    
    dat = rbindlist(datlst, fill = TRUE)
    cols_num = intersect(c('open', 'high', 'low', 'close', 'close_prev', 'settle', 'settle_prev', 'volume', 'position'), names(dat))
    selcols_name = intersect(c('symbol', 'name', 'date', cols_num, 'time'), names(dat))
    
    dat = dat[
        nchar(time)==8, time := paste(date, time)
    ][, time := as.POSIXct(time)
    ][, date := as.Date(date)
    ][, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num
    ][, selcols_name, with = FALSE]
    
    return(dat[])
}


#' query future market data
#' 
#' \code{md_future} query future market data from sina finance, \url{https://finance.sina.com.cn/futuremarket/}.
#' 
#' @param symbol future symbols It is available via function \code{md_future_symbol} or its website. 
#' @param type the data type, including history, real and info. Default is history.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param freq data frequency, default is daily.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L. 
#' @param ... Additional parameters.
#' 
#' @examples 
#' \dontrun{
#' # history data
#' df_hist = md_future(symbol = c('IF0', 'A0', 'CU0', 'CF0', 'XAU'))
#' 
#' # real data
#' df_real = md_future(symbol = c('IF0', 'A0', 'CU0', 'CF0', 'XAU'), 
#'                     type = 'real')
#' }
#' 
#' 
#' @import data.table 
#' @export
md_future = function(symbol, type='history', date_range='max', from=NULL, to=Sys.Date(), freq='daily', print_step=1L, ...) {
    check_internet('www.sina.com.cn')
    # arguments
    ## symbol
    # if (is.null(symbol)) symbol = select_rows_df(md_future_symbol(), column='symbol')[, symbol]
    syb = future_symbols_sybnam(symbol)[,symbol]
    
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
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
    if (type %in% c('history', 'info')) {
        # function name
        funcname = sprintf('md_future1_%s_sina', type)
        # load data by symbol
        dat_list = load_dat_loop(
            syb, funcname, 
            args = list(handle = hd, freq = freq, from = from, to = to), print_step=print_step, ...)
        dat_list = rm_error_dat(dat_list)
    } else if (type %in% c('real')) {
        dat_list = md_future_real(syb, source = source)
    }
    
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