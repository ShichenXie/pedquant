md_stocka_eastmoney = function(symbol1 = 'a') {
    rid=value=variable=time=NULL
    
    if (symbol1 == 'a') {
        urlcode = '82'
        fscode = 'm:0%20t:6,m:0%20t:80,m:1%20t:2,m:1%20t:23,m:0%20t:81%20s:2048'
    } else if (symbol1 == 'b') {
        urlcode = '28'
        fscode = 'm:0%20t:7,m:1%20t:3'
    }
    fieldscode = 
        # paste0('f', 1:300, collapse = ',')
        "f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f26,f29,f98,f102,f103,f124,f62,f128,f136,f115"
    # f1,f11,f19,f22,f24,f25,f128,f152,
    # f29: 1 stock; 2 index; 4 bond; 8 fund
    
    systime_ms = date_num(Sys.time(), 'ms')
    
    url = sprintf(
        "http://%s.push2.eastmoney.com/api/qt/clist/get?fields=%s&pn=1&pz=50000&po=1&np=1&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=%s&_=%s", 
        urlcode, fieldscode, fscode, systime_ms)
    dtmp = read_api_eastmoney(url)

    dtmp = setnames(dtmp, 
             unlist(strsplit(fieldscode,',')), 
             c('close', 'change_pct', 'change', 'volume', 'value', 'amplitude_pct', 'turnover', 'pe', 'f9', 'symbol', 'exchange', 'name', 'high', 'low', 'open', 'close_prev', 'cap_total', 'cap_market', 'pb', 'date', 'market', 'f20', 'prov', 'desc', 'time', 'pe_ttm', 'f25', 'f26', 'f27'))
    dtmp[, `:=`(
        date = as_date(date),
        time = as.POSIXct(as.numeric(time), origin='1970-01-01')
    )]
    
    return(dat)
}