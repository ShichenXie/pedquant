# interbank offered rate, IBOR ------
# http://www.shibor.org/


func_ibor_symbol = function() {
    # london
    libor_symbol = setDT(list(
        symbol =paste0('uko', rep(c('usd','eur','gbp','jpy','chf'),each=7), c('on','1w','1m','2m','3m','6m','1y')),
        name = paste('libor', rep(c('usd','eur','gbp','jpy','chf'),each=7), c('overnight','1w','1m','2m','3m','6m','1y')),
        symbol_fred = paste0(rep(c('usd','eur','gbp','jpy','chf'),each=7), c('ont','1wk','1mt','2mt','3mt','6mt','12m'), 'd156n')
    ))
    
    # shanghai
    shibor_symbol = setDT(list(
        symbol = paste0('cnocny', c('on','1w','2w','1m','3m','6m','9m','1y')),
        name = paste('shibor cny', c('overnight','1w','2w','1m','3m','6m','9m','1y'))
    ))
    
    # ibor symbol list
    ibor_symbol = rbindlist(list(libor_symbol, shibor_symbol), fill = TRUE)
    return(ibor_symbol[])
}
        



# euribor
# https://www.euribor-rates.eu/interesting-websites.asp

# london interbank offered rate, libor
md_libor1_last5 = function(currency) {
    symbol = symbol_fred = value = . = name = NULL
    
    # libor in recent 5 days
    # from # https://www.global-rates.com
    libor_symbol = func_ibor_symbol()[!is.na(symbol_fred)]
    #c('usd','eur','gbp','jpy','chf')
    url_lst = list(
        usd='/american-dollar/american-dollar.aspx', 
        gbp='/british-pound-sterling/british-pound-sterling.aspx', 
        eur='/european-euro/euro.aspx', 
        jpy='/japanese-yen/japanese-yen.aspx', 
        chf='/swiss-franc/swiss-franc.aspx')
    url = paste0('https://www.global-rates.com/interest-rates/libor', url_lst[currency])
    
    # scrap
    wb = read_html(url)
    
    dt_libor_5 = setDT(html_table(wb, fill = TRUE, header = TRUE)[[14]])
    dt_libor_5 = dt_libor_5[,-1][,symbol := paste0('uko',currency,c('on','1w','2w',paste0(1:11,'m'),'1y'))][]
    dt_libor_5[dt_libor_5=='-'] <- NA
    dt_libor_5 = melt(dt_libor_5, id.vars = 'symbol', na.rm = TRUE, variable.name = 'date')[, `:=`(
        value = as.numeric(gsub('[^0-9\\.]+', '', value)), 
        date = as.Date(date, format='%m-%d-%Y')
    )][][libor_symbol[,.(symbol, name)], on='symbol', nomatch=0]
    return(dt_libor_5)
}

# libor history data from FRED
md_libor1_hist = function(syb, from=Sys.Date()-365, to=Sys.Date(), ...) {
    symbol = symbol_fred = . = name = value = geo = unit = NULL
    
    # libor in history
    libor_symbol = func_ibor_symbol()[!is.na(symbol_fred)]
    dt_libor_hist = ed_fred(
        libor_symbol[symbol == syb, symbol_fred], from=from, to=to, print_step=0L
    )[[1]][,`:=`(symbol_fred = symbol, symbol = NULL, name = NULL
    )][libor_symbol, on='symbol_fred', nomatch=0
     ][, .(symbol, name, date, value, geo, unit)
     ][!is.na(value)]
    # return
    return(dt_libor_hist)
}

md_libor = function(symbol, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    # arguments
    ## symbols
    syb_len = length(symbol)
    if (syb_len == 0) return(invisible())
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    # libor
    dat_list = load_dat_loop(
        symbol, 'md_libor1_hist', 
        args = list(from = from, to = to, ...), 
        print_step=print_step, ...)
    dat_list = rm_error_dat(dat_list)
    
    return(dat_list)
}



#' query interbank offered rate
#' 
#' \code{md_money} query libor from FRED or shibor from chinamoney.
#' 
#' @param symbol ibor symbols. Default is NULL. 
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is 3y.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L.
#' 
#' @export
md_money = function(symbol=NULL, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L) {
    
    # syb = intersect(symbol, ibor_symbol$symbol)
    if (is.null(symbol)) symbol = select_rows_df(md_money_symbol(), column='symbol')[,symbol]
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    # load data by symbol
    dat_list = load_dat_loop(
        symbol, 'md_stooq1', 
        args = list(from = from, to = to), 
        print_step=print_step)
    dat_list = rm_error_dat(dat_list)
    
    return(dat_list)
}

    
md_money_symbol = function(...) {
    market = NULL
    
    setDT(copy(symbol_stooq))[market == 'money', c('symbol', 'name'), with = FALSE]
    # func_ibor_symbol()[, c('symbol', 'name'), with = FALSE]
}
    
    
