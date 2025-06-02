md_stock1_history = function(symbol, trytimes = 3, source = c('eastmoney', 'tx'), ...) {
    dat = NULL
    ti = 1
    si = 1
    
    while (!inherits(dat, 'data.frame') & ti <= trytimes & si <= length(source)) {
        dat = try(do.call(paste0('md_stock1_history_', source[si]), args = list(symbol=symbol, ...)), silent = TRUE)
        if (!inherits(dat, 'data.frame') & ti == trytimes) {
            ti = 1
            si = si + 1
        } else {
            ti = ti + 1    
        }
    }
    
    return(dat)
}


# stock hist/et tx ------
# symbol1 = c('AAPL')
# symbol1 = c('01810.hk')
md_stock1_history_eastmoney = function(symbol1, from=NULL, to=Sys.Date(), freq='daily', date_range='3y', forward = NULL, only_adj = FALSE, ...) {
    . = V1 = amount = symbol = city = syb_exp = name = V2 = NULL
    # from
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    freq = check_arg(freq, c('daily', 'weekly', 'monthly'))
    freqcode = list('daily'='101', 'weekly'='102', 'monthly'='103')[[freq]]
    if (is.null(forward)) {
        adjcode = 0
    } else if (isTRUE(forward)) { # hfq
        adjcode = 2
    } else if (isFALSE(forward)) { # qfq
        adjcode = 1
    } 
    # symbol1
    sybtag = suppressWarnings(syb_add_cntags(symbol1))
    symbol1 = sybtag[,syb_exp]
    
    # url
    urlcode = ''
    if (grepl('.HK$', symbol1)) {
        urlcode = '33.'
        syb = paste0('116.', sub('.HK$', '', symbol1))
    } else if (!grepl('.(SS|SZ|SH|ss|sz|sh)$', symbol1)) {
        urlcode = '63.'
        syb = paste0(105:107, '.', symbol1)
    } else {
        urlcode = ''
        syb = sybtag[, ifelse(city == 'sh', sprintf('1.%s', syb), sprintf('0.%s', syb))]
    }

    # &lmt=%s
    # ut=7eea3edcaed734bea9cbfc24409ed989 
    # ,f58,amplitude_pct
    # ,f59,change_pct
    # ,f60,change
    if (isFALSE(only_adj)) {
        url = sprintf(
            'http://%spush2his.eastmoney.com/api/qt/stock/kline/get?fields1=f1,f2,f3,f4,f5,f6&fields2=%s&secid=%s&klt=%s&fqt=%s&beg=%s&end=%s&ut=7eea3edcaed734bea9cbfc24409ed989&_=%s', 
            urlcode, 'f51,f52,f53,f54,f55,f56,f57,f61,f116', syb, freqcode, adjcode, format(from, '%Y%m%d'), format(to, '%Y%m%d'), date_num(Sys.time(),'ms'))
        datlst = rbindlist(lapply(url, function(x) {
            # print(x)
            ret = try(read_apidata_eastmoney(x),silent = TRUE)
            if (inherits(ret, 'try-error')) return(invisible())
            return(ret)
        })) 
        if (ncol(datlst) %in% c(0,3) || is.null(datlst)) {
            cat(symbol1, 'unlisted or delisted. \n')
            return(invisible())
        }
    }
    
    # hfq
    url2 = sprintf(
        'http://%spush2his.eastmoney.com/api/qt/stock/kline/get?fields1=f1,f2,f3&fields2=f51,f53&secid=%s&klt=%s&fqt=%s&beg=%s&end=%s&ut=fa5fd1943c7b386f172d6893dbfba10b&_=%s', 
        urlcode, syb, freqcode, 2, format(from, '%Y%m%d'), format(to, '%Y%m%d'), date_num(Sys.time(),'ms'))
    datlst2 = rbindlist(lapply(url2, function(x) {
        # print(x)
        ret = try(read_apidata_eastmoney(x),silent = TRUE)
        if (inherits(ret, 'try-error')) return(invisible())
        return(ret)
    })) 
    
    if (ncol(datlst2) %in% c(0,3) || is.null(datlst2)) {
        cat(symbol1, 'unlisted or delisted. \n')
        return(invisible())
    }
    # missing: close_prev
    # "amplitude_pct", "change", "change_pct",  
    cols_num = c("open", "high", "low", "close", "volume", "amount", "turnover", "close_adj")
    if (only_adj) cols_num = 'close_adj'
    
    if (only_adj) {
        dat = datlst2[,.(symbol, name, date=V1, close_adj=V2)]
    } else {
        # "amplitude_pct", "change_pct", "change", 
        dat = merge(
            setnames(datlst, c(
                "date", "open", "close", "high", "low",
                "volume", "amount", "turnover", 
                'symbol', 'name', 'market'
            )), 
            datlst2[,.(symbol, name, date=V1, close_adj=V2)], by = c('symbol', 'name', 'date')
        )
    }
    
    dat = dat[, c('symbol', 'name', "date", cols_num), with = FALSE
    ][, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num
    ][, date := as_date(date)
    ][date >= from & date <= to
    ][, symbol := symbol1]
    
    # dat = md_stock_adj1ohlc(dat, adjust = adjust)
    return(dat[])
}


# md_stock1_adjfactor ------
md_stock1_adjfactor = function(symbol1) {
    syb_exp = NULL
    
    sybtag = try(syb_add_cntags(symbol1), silent = TRUE)
    symbol1 = sybtag[,syb_exp]
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
        # print(to2)
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
    sybs_xchg = syb_add_cntags(toupper(symbol1))[
        !is.na(city), syb1 := paste0(city, sub('(\\.ss|\\.sz|\\.sh)$', '', symbol))
    ][is.na(city), syb1 := sprintf('us%s.OQ', symbol)][]
    
    
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


#' adjust stock prices
#' 
#' \code{md_stock_adjust} adjusts the open, high, low and close stock prices. 
#' 
#' @param dt a list/dataframe of time series datasets that didnt adjust for split or dividend.
#' @param forward forward adjust or backward adjust, defaults to FALSE. 
#' @param ... Additional parameters.
#' 
#' @examples 
#' \donttest{
#' data("dt_banks")
#' 
#' dtadj1 = md_stock_adjust(dt_banks, adjust = FALSE)
#' dtadj2 = md_stock_adjust(dt_banks, adjust = TRUE)
#' }
#' @export
md_stock_adjust = function(dt, forward = FALSE, ...) {
    symbol = NULL
    # dt
    dt = check_dt(dt)
    
    # adjusted data list
    dat_list = lapply(
        as.list2(dt[, unique(symbol)]), 
        function(s) {
            dtadj = md_stock_adj1ohlc(dt = dt[symbol == s], forward=forward) 
            return(dtadj)
        }
    )
    
    return(dat_list)
}

md_stock_adj1ohlc = function(dt, forward=NULL) {
    close_adj = NULL
    # param: OHLC columns
    cols_ohlc = c('open', 'high', 'low', 'close')
    
    # return original data
    if (!all(cols_ohlc %in% names(dt)) || is.null(forward) || !('close_adj' %in% names(dt)) || dt[,all(close == close_adj)]) return(dt)
    
    # adjust ohlc
    if (is.logical(forward)) {
        dt = copy(dt)[
            , (cols_ohlc) := lapply(.SD, function(x) {
                if (isTRUE(forward)) {
                    x[.N]*close_adj/close_adj[.N]
                } else {
                    x[1]*close_adj/close[1]
                }
            }), .SDcols = cols_ohlc
        ]
    }
    
    return(dt[])
}
