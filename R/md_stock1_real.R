md_stock_real = function(symbol, trytimes = 3, source = c('tx', 'sina'), ...) {
    dat = NULL
    ti = 1
    
    if (length(intersect(symbol, c('stock','index', 'fund'))) > 0) {
        while (!inherits(dat, 'list') & ti <= trytimes) {
            dat = try(do.call('md_stocka_eastmoney', args = list(symbol=symbol, ...)), silent = TRUE)
            ti = ti + 1
        }
        
        dat[intersect(symbol, c('stock','index', 'fund'))]
        
    } else {
        si = 1
        
        while (!inherits(dat, 'data.frame') & ti <= trytimes & si <= length(source)) {
            dat = try(do.call(paste0('md_stock_real_', source[si]), args = list(symbol=symbol, ...)), silent = TRUE)
            if (!inherits(dat, 'data.frame') & ti == trytimes) {
                ti = 1
                si = si + 1
            } else {
                ti = ti + 1    
            }
        }
    }
    
    return(dat)
}

# real ------
# sina: hq.sinajs.cn/list=sz150206
# http://hq.sinajs.cn/list=usr_aapl
# symbols = c('000001', '01810.hk', 'fb', '^000001', 'aapl')
md_stock_real_sina = function(symbols, only_syb_nam = FALSE, ...) {
    city = syb = exchg_code = name = time = sybsina = NULL
        
    sybs_xchg = syb_add_cntags(symbols)[
        , sybsina := syb
    ][is.na(city), `:=`(
        syb = toupper(syb), 
        sybsina = tolower(syb), 
        city = 'usr_'
    )]
    
    datlst = lapply(
        split(sybs_xchg, by = 'city'), 
        function(x) {
            xchgsybs = x[, paste0(city, sybsina)]
            # print(x)
            url = sprintf('http://hq.sinajs.cn/list=%s', paste0(xchgsybs, collapse = ','))
            # print(url)
            
            cols_name = c('name', 'open', 'close_prev', 'close', 'high', 'low', 'sell', 'buy',  'volume', 'amount', 
                          'bid1_volume', 'bid1', 'bid2_volume', 'bid2',  'bid3_volume', 'bid3','bid4_volume', 'bid4', 'bid5_volume','bid5', 
                          'ask1_volume', 'ask1', 'ask2_volume', 'ask2',  'ask3_volume', 'ask3','ask4_volume', 'ask4', 'ask5_volume','ask5', 
                          'date', 'time', '')
            if (unique(x$city) == 'usr_') cols_name = c('name', 'close', 'change_pct', 'date', 'change', 'open', 'high', 'low', 'high_52w', 'low_52w', 'volume', 'volume_avg_10d', 'cap_total', 'eps', 'pe', paste0('v',1:4), 'cap_stock', paste0('v',5:8), 'v', 'datetime', 'close_prev', paste0('v',9:16))
            if (unique(x$city) == 'hk') cols_name = c('name_eng', 'name', 'open', 'close_prev', 'high', 'low', 'close', 'change', 'change_pct', 'sell', 'buy', 'amount', 'volume', 'v1', 'v2', 'high_52w', 'low_52w', 'date', 'time') 
            
            sybs = x[,ifelse(is.na(exchg_code), syb, paste(syb, exchg_code, sep='.'))]
            dat = read_apidata_sina(
                url, sybs = toupper(sybs), cols_name = cols_name)
            if (!('name' %in% names(dat))) dat = dat[, name := x$name]
            
            return(dat)
        }
    )
    dt = rbindlist(datlst, fill = TRUE)
    
    if (only_syb_nam) {
        dt2 = dt[, c('symbol', 'name'), with = FALSE]
    } else {
        selcols_name = intersect(c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'close_prev', 'change', 'change_pct', 'volume', 'amount', 'cap_total', 'time'), names(dt))
        num_cols = c('open', 'high', 'low', 'close', 'close_prev', 'volume', 'amount')
        
        dt2 = dt[
            , selcols_name, with = FALSE
        ][, (num_cols) := lapply(.SD, as.numeric), .SDcols= num_cols
        ][, `:=`(
            time = as.POSIXct(paste0(date, time), format='%Y-%m-%d %H:%M:%S', tz='Asia/Shanghai'),
            date = as_date(date)
        )]
    }
    
    return(dt2[])
}

# 126: http://api.money.126.net/data/feed/1000001,money.api

# qq finance: https://stockapp.finance.qq.com/mstats/
# http://qt.gtimg.cn/q=sz000001
# http://qt.gtimg.cn/?q=s_usBABA
# http://qt.gtimg.cn/q=r_hk09988
# http://ifzq.gtimg.cn/appstock/app/kline/mkline?param=sz000001,m5
# https://stockapp.finance.qq.com/mstats/#mod=list&id=hk_ah&module=HK&type=AH
# symbols = c('000001', '01810.hk', 'fb', '^000001', 'aapl')
md_stock_real_tx = function(symbols, only_syb_nam = FALSE, ...) {
    city = exchg_code = syb = symbol = amount = mkt = volume = cap_market = cap_total = unit = NULL
    
    sybs_xchg = syb_add_cntags(symbols)[is.na(city), `:=`(
        syb = toupper(syb), 
        city = 'us'
    )]
    
    datlst = lapply(
        split(sybs_xchg, by = 'city'), 
        function(x) {
            sybs = x[, paste0(city, syb)]
            if (only_syb_nam) sybs = paste0('s_',sybs)
            # print(x)
            url = sprintf('http://qt.gtimg.cn/q=%s', paste0(sybs, collapse = ','))
            dat = read_apidata_tencent(url)
            
            # column names
            # qrr: quantity relative ratio
            if (only_syb_nam) {
                cols_name = c('v1', 'name', 'symbol', 'close', 'change', 'change_pct', 'volume', 'amount', 'cap_market', 'cap_total', 'v11')
            } else {
                cols_name = 
                    c('v1', 'name', 'symbol', 'close', 'close_prev', 'open',
                      'volume', 'vol_sell', 'vol_buy', 
                      'bid1', 'bid1_volume', 'bid2', 'bid2_volume', 'bid3', 'bid3_volume', 'bid4', 'bid4_volume', 'bid5', 'bid5_volume',
                      'ask1', 'ask1_volume', 'ask2', 'ask2_volume', 'ask3', 'ask3_volume', 'ask4', 'ask4_volume', 'ask5', 'ask5_volume',
                      'last_trade', 'date', 'change', 'change_pct', 'high', 'low',  
                      '', 'volume', 'amount', 'turnover', 'pe_trailing', '', '', '', 'amplitude_pct', 'cap_market', 'cap_total', 'pb', 'limit-up', 'limit-down', 'qrr', '', 'average', 'pe_forward', 'pe_last', paste0('v', 1:23))
                
                if (unique(x$city) == 'hk') cols_name = 
                        c('v1', 'name', 'symbol', 'close', 'close_prev', 'open',
                          'volume', 'vol_sell', 'vol_buy', 
                          'bid1', 'bid1_volume', 'bid2', 'bid2_volume', 'bid3', 'bid3_volume', 'bid4', 'bid4_volume', 'bid5', 'bid5_volume',
                          'ask1', 'ask1_volume', 'ask2', 'ask2_volume', 'ask3', 'ask3_volume', 'ask4', 'ask4_volume', 'ask5', 'ask5_volume',
                          'last_trade', 'date', 'change', 'change_pct', 'high', 'low',  
                          '', 'volume', 'amount', '', '', '', '', '', 'amplitude_pct', 'cap_market', 'cap_total', 'name_eng', paste0('v0_', 1:12), 'turnover', paste0('v', 6:20))
                
                if (unique(x$city) == 'us') cols_name = 
                        c('v1', 'name', 'symbol', 'close', 'close_prev', 'open',
                          'volume', 'vol_sell', 'vol_buy', 
                          'bid1', 'bid1_volume', 'bid2', 'bid2_volume', 'bid3', 'bid3_volume', 'bid4', 'bid4_volume', 'bid5', 'bid5_volume',
                          'ask1', 'ask1_volume', 'ask2', 'ask2_volume', 'ask3', 'ask3_volume', 'ask4', 'ask4_volume', 'ask5', 'ask5_volume',
                          'last_trade', 'date', 'change', 'change_pct', 'high', 'low',  
                          'unit', 'volume', 'amount', 'turnover', '', '', '', '', 'amplitude_pct', 'cap_market', 'cap_total', 'name_eng', paste0('v0_', 1:11), 'pe', 'pb', '', paste0('v', 7:13))
            }
            
            
            setnames(dat, cols_name[1:ncol(dat)])
            # sybs = x[,ifelse(is.na(exchg_code), syb, paste(syb, exchg_code, sep='.'))]
            dat = dat[, symbol := x$syb_exp
                    ][, intersect(c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'close_prev', 'change', 'change_pct', 'amplitude_pct', 'turnover', 'volume', 'amount', 'cap_market', 'cap_total', 'name_eng', 'unit'), names(dat)), with=FALSE]
            return(dat)
        }
    )
    dt = rbindlist(datlst, fill = TRUE)
    if (!('unit' %in% names(dt))) dt[, unit := 'CNY'] 
        
    if (only_syb_nam) {
        dt2 = dt[, c('symbol', 'name'), with = FALSE]
    } else {
        num_cols = c(
            'open', 'high', 'low', 'close', 'close_prev', 'change', 'change_pct', 'amplitude_pct', 'turnover', 'volume', 'amount', 'cap_market', 'cap_total'
        )
        
        dt2 = copy(dt)[, `:=`(
            time = as.POSIXct(gsub('[^0-9]', '', date), format='%Y%m%d%H%M%S', tz='Asia/Shanghai'), 
            date = as_date(substr(gsub('[^0-9]', '', date),1,8))
        )][, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols
        ][grepl('HK$', symbol), unit := 'HKD'
        ][is.na(unit), unit := 'CNY'
        ][, amount := amount*10000
        ][, mkt := syb_add_cntags(symbol)$mkt
        ][mkt == 'stock', volume := volume*100
        ][, mkt := NULL
        ][, `:=`(
            cap_market = cap_market*10^8, 
            cap_total  = cap_total*10^8
        )]
    }
    
    return(dt2[])
}


