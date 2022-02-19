# symbol1 = c('AAPL')
# symbol1 = c('01810.hk')
md_stock1_history_eastmoney = function(symbol1, from=NULL, to=Sys.Date(), date_range='max', adjust = TRUE, ...) {
    . = V1 = amount = urlcode = symbol = NULL
    # from
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    # symbol1
    symbol1 = toupper(symbol1)
    
    # url
    if (grepl('.HK$', symbol1)) {
        urlcode = 33
        syb = paste0('116.', sub('.HK$', '', symbol1))
    } else if (!grepl('.(SS|SZ|SH)$', symbol1)) {
        urlcode = 63
        syb = paste0(105:107, '.', symbol1)
    }
    
    
    # 
    url = sprintf(
        'http://%s.push2his.eastmoney.com/api/qt/stock/kline/get?fields1=f1,f2,f3,f4,f5,f6&fields2=f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,f61&secid=%s&klt=101&fqt=1&end=%s&lmt=%s&ut=fa5fd1943c7b386f172d6893dbfba10b&_=1623766962675', 
        urlcode, syb, format(to+1, '%Y%m%d'), as.integer(to - from)+1)
    datlst = lapply(url, function(x) {
        # print(x)
        ret = try(read_apidata_eastmoney(x),silent = TRUE)
        if (inherits(ret, 'try-error')) return(invisible())
        return(ret)
    }) 
    
    url2 = sprintf(
        'http://%s.push2his.eastmoney.com/api/qt/stock/kline/get?fields1=f1,f2,f3&fields2=f53&secid=%s&klt=101&fqt=2&end=%s&lmt=%s&ut=fa5fd1943c7b386f172d6893dbfba10b&_=1623766962675', 
        urlcode, syb, format(to+1, '%Y%m%d'), as.integer(to - from)+1)
    datlst2 = lapply(url2, function(x) {
        # print(x)
        ret = try(read_apidata_eastmoney(x),silent = TRUE)
        if (inherits(ret, 'try-error')) return(invisible())
        return(ret)
    }) 
    
    # missing: close_prev
    cols_num = c("open", "high", "low", "close", "change", "change_pct", "amplitude", "volume", "amount", "turnover", "close_adj")
    dat = cbind(
        setnames(rbindlist(datlst), c(
            "date", "open", "close", "high", "low",
            "volume", "amount", "amplitude", "change_pct", "change", "turnover", 
            'symbol', 'name', 'market'
        )), 
        rbindlist(datlst2)[,.(close_adj=V1)]
    )[, c('symbol', 'name', "date", cols_num), with = FALSE
     ][, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num
     ][, `:=`(
         date = as_date(date), 
         amount = amount*10^4
     )][date >= from & date <= to]
    
    if (grepl('.HK$', symbol1)) dat = dat[, symbol := symbol1]
    
    dat = md_stock_adj1ohlc(dat, adjust = adjust)
    return(dat)
}


# https://web.ifzq.gtimg.cn/appstock/app/fqkline/get?param=usAAPL.OQ,day,2020-3-1,2021-3-1,500,qfq
# https://web.ifzq.gtimg.cn/appstock/app/fqkline/get?_var=kline_dayhfq&param=sh600519,day,,,320,hfq&r=0.9860043111257255
md_stock1_history_tx = function(symbol1, from=NULL, to=Sys.Date(), date_range='max', ...) {
    # from
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    # data 
    dat0 = md_stock1_history_tx0(symbol1, from, to, adjust='')
    datadj = md_stock1_history_tx0(symbol1, from, to, adjust='hfq')
    while (datadj[, min(date) > from]) {
        to2 = datadj[, min(date)] - 1
        print(to2)
        datmp = try(md_stock1_history_tx0(symbol1, from, to2, adjust='hfq'), silent = TRUE) 
        if (inherits(datmp, 'try-error')) break else datadj = rbind(datmp, datadj) 
    }
    # add close_prev column
    dat0 = merge(
        dat0, 
        setnames(datadj, 'close', 'close_prev')[,c('date', 'close_prev'), with=FALSE], 
        by = 'date', all.x = TRUE
    )[, c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'close_prev', 'volume'), with=FALSE]
    
    return(dat0)
}


md_stock1_history_tx0 = function(symbol1, from, to, adjust) {
    # adjust: '', qfq, hfq
    city = syb1 = symbol = NULL
    
    # symbol1
    sybs_xchg = check_symbol_cn(toupper(symbol1))[!is.na(city), syb1 := paste0(city, symbol)][is.na(city), syb1 := sprintf('us%s.OQ', symbol)]
    
        
    url = sprintf('https://web.ifzq.gtimg.cn/appstock/app/fqkline/get?param=%s,day,%s,%s,%s,%s', sybs_xchg$syb1, from, to, to-from, adjust)
    # url = sprintf('https://web.ifzq.gtimg.cn/appstock/app/fqkline/get?param=%s,day,%s,%s,%s,%s', sybs_xchg$syb1, from, to, to-from, adjust)
    
    txjson2dt = function(x) {
        numcols = c('open', 'close', 'high', 'low', 'volume')
        
        if (inherits(x, 'list')) {
            dt = rbindlist(lapply(x, function(dt) as.data.frame(t(dt))), fill=TRUE) 
        } else {
            dt = setDT(as.data.frame(x))
        }
        
        dt = setnames(
            dt[,paste0('V',1:6),with=FALSE], 
            c('date', 'open', 'close', 'high', 'low', 'volume')
        )[, date := as_date(date)
        ][, (numcols) := lapply(.SD, as.numeric), .SDcols =numcols]
        return(dt)
    }
    dat = fromJSON(url)
    # datlst = txjson2dt(dat$data[[1]][[1]])
    datlst = cbind(
        data.table(symbol=sybs_xchg$syb_exp, name = dat$data[[1]][['qt']][[1]][2])[], 
        txjson2dt(dat$data[[1]][[1]])
    )
    
    return(datlst)
}

