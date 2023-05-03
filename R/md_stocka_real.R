fidnam = function(col) {
    data.table(
        fi = c("f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f20", "f21", "f23", "f26", "f29", "f102", "f103", "f124", "f62"), 
        ni = c('close', 'change_pct', 'change', 'volume', 'amount', 'amplitude_pct', 'turnover', 'pe', 'sybcode', 'exchangecode', 'name', 'high', 'low', 'open', 'close_prev', 'cap_total', 'cap_market', 'pb', 'date', 'mktcode', 'prov', 'desc', 'ticktime', 'pe_ttm')
    )[[col]]
}
md_stocka_eastmoney = function(symbol1 = 'a') {
    mktcode = sybcode = symbol = rid = value = variable = time = exchange = ticktime = NULL
    
    if (symbol1 == 'a') {
        urlcode = '82'
        fscode = 'm:0%20t:6,m:0%20t:80,m:1%20t:2,m:1%20t:23,m:0%20t:81%20s:2048'
    } else if (symbol1 == 'b') {
        urlcode = '28'
        fscode = 'm:0%20t:7,m:1%20t:3'
    }
    fid = 
        c(1, 12, 14, 124, 17, 15, 16, 2, 5, 6,8, 29, 62, 23, 20, 21)
        # paste0('f', 1:300, collapse = ',')
        # "f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f26,f29,f98,f102,f103,f124,f62,f128,f136,f115"
    # f1,f11,f19,f22,f24,f25,f128,f152,
    # f29: 1 stock; 2 index; 4 bond; 8 fund 
    
    url = sprintf(
        "http://%s.push2.eastmoney.com/api/qt/clist/get?fields=%s&pn=1&pz=50000&po=1&np=1&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=%s&_=%s", 
        urlcode, paste0('f', fid, collapse = ','), fscode, date_num(Sys.time(), 'ms'))
    dtmp = read_apidata_eastmoney(url, type = 'real_cn')

    
    dtmp = setnames(
        dtmp, fidnam('fi'), fidnam('ni'), skip_absent=TRUE
    )[, `:=`(
        mktcode = as.character(mktcode), 
        ticktime = as.POSIXct(as.numeric(ticktime), origin='1970-01-01')
    )][, date := as_date(ticktime)
     ][data.table(mktcode = as.character(c(1,2,4,8,512)), market=c('stock', 'index', 'bond', 'fund', '512')),
       on = 'mktcode'
     ][mktcode == '1', symbol := syb_fmt_output(sybcode, mkt='stock')
     ][mktcode == '2', symbol := syb_fmt_output(sybcode, mkt='index')
     ][mktcode == '8', symbol := syb_fmt_output(sybcode, mkt='fund')
     ][grepl('SS$', symbol), exchange := 'sse'
     ][grepl('SZ$', symbol), exchange := 'szse'
     ][grepl('BS$', symbol), exchange := 'bse']
    
    datlst = split(dtmp, by = 'market')
    # dtmp[,.N, keyby=.(mktcode, f19, market, exchange)]
    return(datlst)
}