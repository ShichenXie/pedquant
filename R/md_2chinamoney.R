# https://www.chinamoney.com.cn/chinese/bkccpr/
# https://www.shibor.org/shibor/

date_ft1y = function(from, to) {
    yearft = seq(year(from), year(to))
    
    ft = data.table(
        f = paste0(yearft, '-01-01'), 
        t = paste0(yearft, '-12-31')
    )[, lapply(.SD, as.Date)]
    ft[.N, t := to]
    
    return(ft)
}

load_data_chinamoney = function(url, ft) {
    datlst = lapply(
        split(ft, by = 'f'),
        function(x) {
            dat = load_read_xl(sprintf(url, x$f, x$t))
            
            setnames(dat, names(dat)[1], 'date')
            return(dat)
        }
    )
    
    # rbind
    dat = rbindlist(datlst, fill=TRUE)[, date := as.Date(date)][!is.na(date)][order(date)]
    
    # as.numeric
    numcols = names(dat)[-1]
    dat = dat[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
    
    return(dat[])
}

md_moneycn_symbol = function() {
    fread(
        'symbol,name
         cnocnyOm,Shibor CNY OVERNIGHT - daily
         cnocny1w,Shibor CNY 1W - daily
         cnocny2w,Shibor CNY 2W - daily
         cnocny1m,Shibor CNY 1M - daily
         cnocny3m,Shibor CNY 3M - daily
         cnocny6m,Shibor CNY 6M - daily
         cnocny9m,Shibor CNY 9M - daily
         cnocny1m,Shibor CNY 1Y - daily
         cn1ylpr,China 1year Loan Prime Rate - monthly
         cn5ylpr,China 5year Loan Prime Rate - monthly
         cn1ypr_d,China 1year Policy Rate Deposit - daily
         cn1ypr_l,China 1year Policy Rate Lending - daily
         cn1ydy_b,China 1Y Bond Yield - daily
         cn10ydy_b,China 10Y Bond Yield - daily
         rmbx_cfets,CFETS RMB Index - weekly
         rmbx_bis,BIS Currency Basket RMB Index - weekly
         rmbx_sdr,SDR Currency Basket RMB Index - weekly
        '
    )
}
md_moneycn_widelong = function(dt) {
    . = name = symbol = NULL
    
    melt(dt, id.vars = 'date', variable.name = 'symbol', value.name = 'close')[
        md_moneycn_symbol()[], on = 'symbol', nomatch = 0
    ][, .(symbol, name, date, open=close, high=close, low=close, close)]
}

# shanghai interbank offered rate, shibor
#' @import data.table
md_cnshibor = function(date_range = '3y', from=NULL, to=Sys.Date(), ...) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1997-01-01", default_date_range = '3y')
    
    ## download shibor history data
    # https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2022-10-18&endDate=2022-11-17
    # https://www.shibor.org/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2012-01-01&endDate=2012-12-31
    dat = load_data_chinamoney('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=%s&endDate=%s', date_ft1y(from, to))
    
    setnames(dat, c('date', paste0('cnocny',c('on', '1w', '2w', '1m', '3m', '6m', '9m', '1y'))))
    dat = md_moneycn_widelong(dat)
    return(dat[])
}

# loan prime rate, lpr
md_cnlpr = function(date_range = '3y', from=NULL, to=Sys.Date(), ...) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2013-01-01", default_date_range = '3y')
    
    # 'https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=%s&strEndDate=%s'
    # https://www.shibor.org/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=2013-01-01&strEndDate=2013-12-31
    ## download data
    dat = load_data_chinamoney('https://www.shibor.org/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=%s&strEndDate=%s', date_ft1y(from, to))
    
    setnames(dat, c('date', 'cn1ylpr', 'cn5ylpr'))
    dat = md_moneycn_widelong(dat)
    return(dat[])
}

# policy rate
md_cnpr = function(date_range = '3y', from=NULL, to=Sys.Date(), ...) {
    date_range = 'max'
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1997-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/SddsIntrRatePlRatHisExcel?lang=CN&startDate=%s&endDate=%s&t=', date_num(Sys.time(), 'ms')), data.table(f=from, t=to))
    
    setnames(dat, c('date', "cn1ybdr", "cn1yblr"))
    dat2 = merge(
        data.table(date=seq(dat[,min(date)], Sys.Date(), by=1)),
        dat, by = 'date', all.x=TRUE
    )[, lapply(.SD, fillna)
    ]
    
    dat2 = md_moneycn_widelong(dat2)
    return(dat2[])
}

# yields of bond 
md_cnbond = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1997-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/SddsIntrRateGovYldHisExcel?lang=CN&startDate=%s&endDate=%s&t=', date_num(Sys.time(), 'ms')), date_ft1y(from, to))
    
    setnames(dat, c('date', "cn1ydy_b", "cn10ydy_b"))
    dat = md_moneycn_widelong(dat)
    
    return(dat[])
}


# forex cny
md_cnyforex = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2014-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-ccpr/CcprHisExcelNew?startDate=%s&endDate=%s&currency=', 'USD/CNY,EUR/CNY,100JPY/CNY,HKD/CNY,GBP/CNY,AUD/CNY,NZD/CNY,SGD/CNY,CHF/CNY,CAD/CNY,CNY/MYR,CNY/RUB,CNY/ZAR,CNY/KRW,CNY/AED,CNY/SAR,CNY/HUF,CNY/PLN,CNY/DKK,CNY/SEK,CNY/NOK,CNY/TRY,CNY/MXN,CNY/THB'), date_ft1y(from, to))
    
    # dat = md_moneycn_widelong(dat)
    return(dat[])
}


# cny index
md_cnrmbx = function(date_range = '3y', from=NULL, to=Sys.Date(), ...) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2015-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-fx/RmbIdxHisExcel?lang=CN&startDate=%s&endDate=%s&t=', date_num(Sys.time(), 'ms')), date_ft1y(from, to))
    
    setnames(dat, c('date', "rmbx_cfets", "rmbx_bis", "rmbx_sdr"))
    dat = md_moneycn_widelong(dat)
    return(dat[])
}


#' query chinese benchmark rates
#' 
#' \code{md_moneycn} query benchmark rates from chinamoney.com.cn.
#' 
#' @param symbol benchmarks, available values including 'rmbx', 'shibor', 'lpr', 'pr', 'yb'. Default is NULL, 
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is 3y.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L.
#' 
#' @export
md_moneycn = function(symbol=NULL, date_range = "3y", from = NULL, to = Sys.Date(), print_step = 1L) {
    # syb = intersect(symbol, ibor_symbol$symbol)
    if (is.null(symbol)) symbol = select_rows_df(data.table(
        symbol = c('rmbx', 'shibor', 'lpr', 'pr', 'bond'), 
        name = c('RMB Index', 'Shanghai Interbank Offered Rate', 'Loan Prime Rate', 'Policy Deposit/Lending Rate', 'Yields of Gov. Bond'), 
        type = c('FX Market', 'RMB Market', 'RMB Market', 'Interest Rate', 'Interest Rate')
    ), column='symbol')[,symbol]
    
    
    # load data by symbol
    dat_list = lapply(
        as.list2(symbol), 
        function(s) {
            if ((print_step>0)) cat(sprintf('%s %s\n', paste0(format(c(which(symbol %in% s), length(symbol))), collapse = '/'), s))
            dat = do.call(paste0('md_cn',s), list(date_range = date_range, from = from, to = to))
        }
    )
    dat_list = rm_error_dat(dat_list)
    
    return(dat_list)
}


