# shanghai interbank offered rate, shibor
#' @import data.table
md_shibor = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    . = name = value = X1 = f = cnocny1m = symbol_fred = NULL
    
    # arguments
    shibor_symbol = func_ibor_symbol()[is.na(symbol_fred)]
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2007-01-01", default_date_range = '3y')
    # from to 
    ft = data.table(f = unique(c(seq(from, to, by = 360), to)))[
        , t := shift(f, type = 'lead')
    ][!is.na(t)]
    
    # download shibor history data
    # https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2022-10-18&endDate=2022-11-17
    # https://www.shibor.org/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=2012-01-01&endDate=2012-12-31
    cols_num = paste0('cnocny',c('on', '1w', '2w', '1m', '3m', '6m', '9m', '1y'))
    datlst = lapply(
        split(ft, by = 'f'),
        function(x) {
            dat = load_read_xl(sprintf(
                'https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-shibor/ShiborHisExcel?lang=cn&startDate=%s&endDate=%s', x$f, x$t
            ))
            
            setnames(dat, c('date', cols_num))
            return(dat[!is.na(cnocny1m)])
        }
    )
    
    
    dat = melt(
        unique(rbindlist(datlst))[,`:=`(
            date = as_date(date)
        )][, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num], 
        id.vars = 'date', variable.name = 'symbol'
    )[shibor_symbol, on='symbol'
    ][, c('symbol', 'name', 'date', 'value'), with = FALSE]
    
    # return data list
    data_list = split(dat, by = 'symbol')
    return(data_list)
}

# forex cny
md_fxcny = function(date_range = '3y', from=NULL, to=Sys.Date()) {
    f = NULL

    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "2007-01-01", default_date_range = '3y')
    
    ft = data.table(f = unique(c(seq(from, to, by = 360), to)))[
        , t := shift(f, type = 'lead')
    ][!is.na(t)]
    
    # download data
    datlst = lapply(
        split(ft, by = 'f'),
        function(x) {
            dat = load_read_xl(sprintf(
                'https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-ccpr/CcprHisExcelNew?startDate=%s&endDate=%s&currency=%s', x$f, x$t, 
                'USD/CNY,EUR/CNY,100JPY/CNY,HKD/CNY,GBP/CNY,AUD/CNY,NZD/CNY,SGD/CNY,CHF/CNY,CAD/CNY,CNY/MYR,CNY/RUB,CNY/ZAR,CNY/KRW,CNY/AED,CNY/SAR,CNY/HUF,CNY/PLN,CNY/DKK,CNY/SEK,CNY/NOK,CNY/TRY,CNY/MXN,CNY/THB'
            ))
            setnames(dat, names(dat)[1], 'date')
            dat = dat[, date := as.Date(date)][!is.na(date)]
            return(dat)
        }
    )
    
    rbindlist(datlst)
}


# https://www.chinamoney.com.cn/dqs/rest/cm-u-bk-fx/RmbIdxHisExcel?lang=CN&startDate=2021-11-18&endDate=2022-11-17&t=1668699891251