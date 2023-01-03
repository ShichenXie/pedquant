#' @import data.table 
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_unescape_unicode
md_stockall_real_163 = function(symbol = c('a','index'), only_symbol = FALSE, show_tags = FALSE, to_sysdata=FALSE, ...) {
    prov = tags = market = exchange = time = . = submarket = region = board = name = mkt = indu = sec = unit = NULL
    
    syb_nam_dat = md_stock_real('^000001', trytimes = 3)
    
    fun_stock_163 = function(urli, mkt, data_date) {
        code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = hs = volume = turnover = mcap = tcap = pe = mfsum = net_income = revenue = plate_ids = time = NULL
        # stock
        # c('code', 'five_minute' 'high', 'hs', 'lb', 'low', 'mcap', 'mfratio', 'mfsum', 'name', 'open', 'pe', 'percent', 'plate_ids', 'price', 'sname', 'symbol', 'tcap', 'turnover', 'updown', 'volume', 'wb', 'yestclose', 'zf', 'no', 'announmt', 'uvsnews')
        # index
        # c('code', 'high', 'low', 'name', 'open' 'percent', 'price', 'symbol', 'time', 'turnover' 'updown', 'volume', 'yestclose', 'no', 'zhenfu') 
        # data_date = md_stock_real_tx('^000001')$date
        jsonDat = fromJSON(urli)
        
        jsonDF = jsonDat$list
        if (mkt == 'stock') {
            jsonDF$net_income = jsonDF$MFRATIO$MFRATIO2
            jsonDF$revenue = jsonDF$MFRATIO$MFRATIO10
            jsonDF[,c('MFRATIO', 'UVSNEWS','ANNOUNMT','NO')] = NULL 
            names(jsonDF) = tolower(names(jsonDF))
            
            jsonDF = setDT(jsonDF)[,`:=`(
                date = data_date, #as.Date(substr(jsonDat$time,1,10)), 
                time = jsonDat$time#,
                #strptime(jsonDat$time, '%Y-%m-%d %H:%M:%S', tz = 'Asia/Shanghai')
            )][, .(symbol, name, date, open, high, low, close=price, close_prev=yestclose, change=updown, change_pct=percent*100, volume, amount=turnover, turnover=hs*100, cap_market=mcap, cap_total=tcap, pe_last=pe, eps=mfsum, net_income, revenue, plate_ids, time=as.POSIXct(time))
            ][,`:=`(
                province = sub('.*(dy\\d+).*', '\\1', plate_ids),
                plate_ids = sub('dy\\d+','',plate_ids)
            )][,`:=`(
                sector = sub('.*((hy\\d{3}0{3})|hy012001).*', '\\1', plate_ids),
                industry = sub('.*(hy\\d+).*', '\\1', sub('hy\\d{3}0{3}','',plate_ids))
            )]
            
        } else if (mkt == 'index') {
            names(jsonDF) = tolower(names(jsonDF))
            
            jsonDF = setDT(jsonDF)[,`:=`(
                date = data_date#as.Date(substr(jsonDat$time,1,10))
            )][, .(symbol, name, date, open, high, low, close=price, close_prev=yestclose, change=updown, change_pct=percent*100, volume=volume/100, amount=turnover, time=as.POSIXct(time))]
        }
        
        return(jsonDF[, `:=`(market = mkt, region = 'cn')])
    }
    
    urls_163 = list(
        a = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=CODE&order=desc&count=100000&type=query', 
        b = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=100000&type=query',
        index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query',
        index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query'
    )
    idx = which(names(urls_163) %in% symbol) #unlist(strsplit(symbol,','))
    
    df_stock_cn = try(
        rbindlist(
            mapply(fun_stock_163, urls_163[idx], c('stock','stock','index','index')[idx], syb_nam_dat$date, SIMPLIFY = FALSE), 
            fill = TRUE
        ), 
        silent = TRUE
    )#, idcol = 'mkt')#[mkt %in% c('a','b'), mkt := 'stock']
    if (!inherits(df_stock_cn, 'try-error') & 'fund' %in% symbol) df_stock_cn = rbind(df_stock_cn, md_fundall_real_163(), fill=TRUE)
    
    # date time of download
    # datetime = gsub('[^(0-9)]','',df_stock_cn[1,time])
    # if (df_stock_cn[1,time] < as.POSIXct(paste(df_stock_cn[1,date], '15:00:00'))) 
    #   cat('The close price is real price at', as.character(datetime), '\n')
    
    # create/export sysdata.rda 
    if (to_sysdata) return(df_stock_cn)
    # create/export symbol only or tags
    if (only_symbol || show_tags) {
        if (inherits(df_stock_cn, 'try-error')) df_stock_cn = setDT(copy(symbol_stock_163))
        
        df_stock_cn = symbol_163_format(df_stock_cn)
        if ('prov' %in% names(df_stock_cn)) df_stock_cn = df_stock_cn[is.na(prov), prov := stri_unescape_unicode('\\u91cd\\u5e86')]
        
        if (only_symbol & show_tags) {
            df_stock_cn = df_stock_cn[
                , .(market, submarket, region, exchange, board, symbol, name, prov, sec, indu)
            ][order(-market, exchange, symbol)]
        } else if (only_symbol) {
            df_stock_cn = df_stock_cn[
                , .(market, submarket, region, exchange, board, symbol, name)
            ][order(-market, exchange, symbol)]
        }
    } else {
        if (!identical(symbol, 'index')) {
            cols_rm = intersect(names(df_stock_cn), c('eps', 'net_income', 'revenue'))
            if (length(cols_rm)>0) df_stock_cn = df_stock_cn[, (cols_rm) := NULL]
        }
    }
    
    df = df_stock_cn[,unit := 'CNY'][, symbol := syb_fmt_output(symbol, market)]#[, mkt := NULL][]
    
    cols_rm = intersect(names(df), c('sector', 'industry', 'province', 'plate_ids', 'region')) # , 'close_prev', 
    if (length(cols_rm)>0) df = df[, (cols_rm) := NULL]
    return(df)
}


# fund real ------
md_fundall_real_163 = function() {
    . = fund = high = low = name = price = symbol = time = turnover = volume = yestclose = unit = percent = NULL
    Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
    
    dat_lst = lapply(list(
        fund_close = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=STYPE:FDC;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_11861237&req=0%s',
        fund_etf = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=find:/ETF/;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_1696032826&req=0%s',
        fund_lof = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=find:/LOF/;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_814011473&req=0%s',
        fund_leveraged = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=FUND_TYPE4:_int_190701;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_2112195916&req=0%s'
    ), function(u) {
        # fromJSON(sub('.+\((\\1)\)', readline(u)))
        url = sprintf(
            u, 20000,
            paste0(hour(Sys.time()), minute(Sys.time()))
        )
        
        # print(url)
        dt = read_lines(url)
        dt = fromJSON(sub('.+?\\((.+)\\)', '\\1', dt))
        dat = setDT(dt$list)[, time := dt$time]
        return(dat)
    })
    
    dat_df = rbindlist(dat_lst, idcol = 'fund')
    setnames(dat_df, tolower(names(dat_df)))
    
    syb_nam_dat = md_stock_real('^000001', trytimes = 3)
    
    dat_df = dat_df[
        , .SD[1], by = 'symbol'
    ][, .(symbol = syb_fmt_output(symbol, 'fund'), date = syb_nam_dat$date, name, open, high, low, close = price, close_prev = yestclose, change_pct = percent*100, volume, amount = turnover, market = fund, unit = 'CNY')] # change=updown, sname, 
    
    return(dat_df)
}
