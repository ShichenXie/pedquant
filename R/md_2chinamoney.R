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

# shanghai interbank offered rate, shibor
#' @import data.table
md_shibor = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1997-01-01", default_date_range = '3y')
    
    ## download shibor history data
    # https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2022-10-18&endDate=2022-11-17
    # https://www.shibor.org/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2012-01-01&endDate=2012-12-31
    dat = load_data_chinamoney('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=%s&endDate=%s', date_ft1y(from, to))
    
    setnames(dat, c('date', paste0('cnocny',c('on', '1w', '2w', '1m', '3m', '6m', '9m', '1y'))))
    return(dat[])
}

# loan prime rate, lpr
md_lpr = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2013-01-01", default_date_range = '3y')
    
    # 'https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=%s&strEndDate=%s'
    # https://www.shibor.org/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=2013-01-01&strEndDate=2013-12-31
    ## download data
    dat = load_data_chinamoney('https://www.shibor.org/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=%s&strEndDate=%s', date_ft1y(from, to))
    
    setnames(dat, c('date', 'lpr1y', 'lpr5y'))
    return(dat[])
}

# benchmark/policy rate
md_br = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1997-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/SddsIntrRatePlRatHisExcel?lang=CN&startDate=%s&endDate=%s&t=', date_num(Sys.time(), 'ms')), data.table(f=from, t=to))
    
    setnames(dat, c('date', "br1y_deposit", "br1y_lending"))
    dat2 = merge(
        data.table(date=seq(dat[,min(date)], Sys.Date(), by=1)),
        dat, by = 'date', all.x=TRUE
    )[, lapply(.SD, fillna)
    ]
    return(dat2[])
}
# forex cny
md_cnyforex = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2014-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-ccpr/CcprHisExcelNew?startDate=%s&endDate=%s&currency=', 'USD/CNY,EUR/CNY,100JPY/CNY,HKD/CNY,GBP/CNY,AUD/CNY,NZD/CNY,SGD/CNY,CHF/CNY,CAD/CNY,CNY/MYR,CNY/RUB,CNY/ZAR,CNY/KRW,CNY/AED,CNY/SAR,CNY/HUF,CNY/PLN,CNY/DKK,CNY/SEK,CNY/NOK,CNY/TRY,CNY/MXN,CNY/THB'), date_ft1y(from, to))
    
    return(dat[])
}


# cny index
md_cnyindex = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2015-01-01", default_date_range = '3y')
    
    ## download data
    dat = load_data_chinamoney(paste0('https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-fx/RmbIdxHisExcel?lang=CN&startDate=%s&endDate=%s&t=', date_num(Sys.time(), 'ms')), date_ft1y(from, to))
    
    setnames(dat, c('date', "cnyx_cfets", "cnyx_bis", "cnyx_sdr"))
    return(dat[])
}
