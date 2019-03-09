# https://github.com/joshuaulrich/quantmod/blob/a8e9cb87825c0997a8468f5105db6c507b26ac5d/R/adjustOHLC.R
adjust_ohlc = function(dt, source, adjust_on = 'dividend', ...) {
    close_adj=ratio=symbol=V1=.=dividends=splits=issue_rate=issue_price=prev_close=factor_adj_spl=factor_adj_div=factor_adj=volume=NULL
    
    cols_ohlc = c('open', 'high', 'low', 'close')
    if (!all(cols_ohlc %in% names(dt))) return(dt)
    
    # adjusting price
    if (source == 'yahoo') {
        dt = dt[, ratio := close_adj/close
              ][, (cols_ohlc) := lapply(.SD, function(x) x*ratio), .SDcols = cols_ohlc
              ][, ratio := NULL]
        # symbol.name = '000001.'
        # div <- getDividends(symbol.name, from="1900-01-01")
        # splits <- getSplits(symbol.name, from="1900-01-01")
        
    } else if (source == '163') {
        symbol1 = dt[1, tstrsplit(symbol, '\\.')][,V1]
        ds = try(md_stock_divsplit1_163(symbol1, ret = c('div', 'spl', 'rig')), silent = TRUE)
        if (inherits(ds, 'try-error')) return(dt)
        
        # data to calculate adjust factor
        ddtt = Reduce(function(x,y) merge(x,y,all=TRUE,by='date'), 
               c(list(dt = dt), lapply(ds, function(x) x[, (c('symbol','name')) := NULL])))
        
        ddtt2 = ddtt[,.(date, close, prev_close, dividends, splits, issue_rate, issue_price)
           ][!(is.na(dividends) & is.na(splits) & is.na(issue_rate))
           ][!is.na(close)]
        ddtt2[is.na(ddtt2)] <- 0
        
        # adjust factor for split and dividend
        fac_adj_dt = ddtt2[, `:=`(
            close_adj_div = (prev_close + issue_price*issue_rate - dividends)/(1+splits+issue_rate),
            close_adj_spl = prev_close/(1+splits)
        )][, (c('factor_adj_div', 'factor_adj_spl')) := lapply(.SD, function(x) prev_close/x), .SDcols = c('close_adj_div', 'close_adj_spl')
         ][order(-date)
         ][, (c('factor_adj_div', 'factor_adj_spl')) := lapply(.SD, cumprod), .SDcols = c('factor_adj_div', 'factor_adj_spl')
         ][order(date)
         ][,.(date, factor_adj_spl, factor_adj_div)]
        fac_adj_dt[['factor_adj']] = fac_adj_dt[[paste0('factor_adj_', substr(adjust_on,1,3) )]]
        
            
        # adjusting ohlc price
        adj_cols = c('factor_adj_spl', 'factor_adj')
        dt = merge(dt, fac_adj_dt, by = 'date', all.x = TRUE
            )[order(date)
            ][, (adj_cols) := lapply(.SD, function(x) shift(x, type = 'lead')), .SDcols=adj_cols
            ][, (adj_cols) := lapply(.SD, function(x) fillna(x, from_last = TRUE)), .SDcols=adj_cols
            ][is.na(factor_adj_spl), factor_adj_spl := 1
            ][is.na(factor_adj), factor_adj := 1
            ][, (cols_ohlc) := lapply(.SD, function(x) x/factor_adj), .SDcols = cols_ohlc
            ][, `:=`(
                volume = volume*factor_adj_spl,
                prev_close = shift(close, type = 'lag'),
                change = close - shift(close, type = 'lag'),
                change_pct = close/shift(close, type = 'lag') - 1,
                factor_adj = NULL, 
                factor_adj_spl = NULL, 
                factor_adj_div = NULL
            )]
        
    }
    return(dt)
}

# dt2 = adjust_ohlc(dt, source = '163')
# merge(dat_div$`000001.SZ`, dat_split$`000001.SZ`, all = TRUE)


# dddttt = copy(dat$`000001`)
# dddttt[, close_adj := cap_market/(cap_market[.N]/close[.N])]