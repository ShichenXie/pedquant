fidnam = function(col) {
    #qrr: quantity relative ratio
    fread(
         'fi              ni
          f2           close
          f3      change_pct
          f4          change
          f5          volume
          f6          amount
          f7   amplitude_pct
          f8        turnover
          f9      pe_forward
         f10             qrr
         f12         sybcode
         f13    exchangecode
         f14            name
         f15            high
         f16             low
         f17            open
         f18      close_prev
         f20       cap_total
         f21      cap_market
         f23              pb
         f24          pledge        
         f26            date
         f29         mktcode
         f33    commission_ratio
         f37             roe
        f102            prov
        f103            desc
        f124        ticktime
        f115          pe_ttm
        f114          pe_lyr
        f221 date_lastupdate'
    )[[col]]
}
md_stocka_eastmoney = function(symbol1 = 'stocka', sleep = 1.86) {
    mktcode = sybcode = symbol = rid = value = variable = time = exchange = ticktime = date_lastupdate = NULL
    
    if (symbol1 == 'stocka') {
        urlcode = '82'
        fscode = 'm:0%20t:6,m:0%20t:80,m:1%20t:2,m:1%20t:23,m:0%20t:81%20s:2048'
    } else if (symbol1 == 'stockb') {
        urlcode = '28'
        fscode = 'm:0%20t:7,m:1%20t:3'
    }
    # bj "fs": "m:0%20t:81%20s:2048",
    # sz "fs": "m:0%20t:6,m:0%20t:80",
    # sh "fs": "m:1%20t:2,m:1%20t:23",
    # zh "fs": "m:0%20t:6,m:0%20t:80,m:1%20t:2,m:1%20t:23,m:0%20t:81%20s:2048",
    

    fid = c(1, 12, 13, 14, 124, 17, 15, 16, 2, 5, 6,8, 29, 9, 115, 114, 23, 20, 21, 221)
    # fid = paste0('f', 1:300, collapse = ',')
        # "f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f26,f29,f98,f102,f103,f124,f62,f128,f136,f115"
    # f1,f11,f19,f22,f24,f25,f128,f152,
    # f29: 1 stock; 2 index; 4 bond; 8 fund 
    dmp1 = read_api_eastmoney(sprintf(
        "http://%s.push2.eastmoney.com/api/qt/clist/get?fields=%s&pn=%s&pz=100&po=1&np=2&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=%s&_=%s", 
        urlcode, paste0('f', fid, collapse = ','), 1, fscode, date_num(Sys.time(), 'ms')))
    totalpages = ceiling(dmp1$data$total/length(dmp1$data$diff))
    
    pagenum = 1
    dmplst = NULL
    while (pagenum <= totalpages) {
        # print(pagenum)
        url = sprintf(
            "http://%s.push2.eastmoney.com/api/qt/clist/get?fields=%s&pn=%s&pz=100&po=1&np=2&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=%s&_=%s", 
            urlcode, paste0('f', fid, collapse = ','), pagenum, fscode, date_num(Sys.time(), 'ms')) 
        dmplst = c(dmplst, list(dt = read_apidata_eastmoney(url, type = 'real_cn') )) 
        pagenum = pagenum + 1
        Sys.sleep(abs(rnorm(1, sleep)))
    }
    
    cols_num = c("open", "high", "low", "close", "volume", "amount", "turnover",  "cap_total", "cap_market", "pe_ttm", "pb", "pe_lyr", "pe_forward")
    dtmp = setnames(
        rbindlist(dmplst), fidnam('fi'), fidnam('ni'), skip_absent=TRUE
    )[, `:=`(
        mktcode = as.character(mktcode), 
        ticktime = as.POSIXct(as.numeric(ticktime), origin='1970-01-01'), 
        date_lastupdate = as_date(date_lastupdate)
    )][, date := as_date(ticktime)
     ][data.table(mktcode = as.character(c(1,2,4,8,512)), market=c('stock', 'index', 'bond', 'fund', '512')),
       on = 'mktcode'
     ][mktcode == '1', symbol := syb_fmt_output(sybcode, mkt='stock')
     ][mktcode == '2', symbol := syb_fmt_output(sybcode, mkt='index')
     ][mktcode == '8', symbol := syb_fmt_output(sybcode, mkt='fund')
     ][grepl('SH$', symbol), exchange := 'sse'
     ][grepl('SZ$', symbol), exchange := 'szse'
     ][grepl('NQ$', symbol), exchange := 'bse'
     ][, (cols_num) := lapply(.SD, as.numeric), .SDcols=cols_num]
    
    datlst = split(dtmp, by = 'market')
    # dtmp[,.N, keyby=.(mktcode, f19, market, exchange)]
    
    return(datlst)
}