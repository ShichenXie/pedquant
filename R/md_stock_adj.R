# https://github.com/joshuaulrich/quantmod/blob/a8e9cb87825c0997a8468f5105db6c507b26ac5d/R/adjustOHLC.R
adjust_ohlc = function(dt, source, ...) {
    
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
        
        ds = md_stock_divsplit1_163(symbol1, ret = c('div', 'spl', 'rig'))
        
        # adjust factor
        ddtt = Reduce(function(x,y) merge(x,y,all=TRUE,by='date'), 
               c(list(dt = dt), lapply(ds, function(x) x[, (c('symbol','name')) := NULL])))
        
        ddtt2 = ddtt[,.(date, close, prev_close, dividends, splits, issue_rate, issue_price)
           ][!(is.na(dividends) & is.na(splits) & is.na(issue_rate))
           ][!is.na(close)]
        ddtt2[is.na(ddtt2)] <- 0
        
        fac_adj_dt = ddtt2[, close_adj := ((prev_close + issue_price * issue_rate - dividends)/(1+splits+issue_rate))
            ][, factor_adj := prev_close/close_adj
            ][order(-date)
            ][, factor_adj := cumprod(factor_adj)
            # ][, date := date-1
            ][order(date)][,.(date, factor_adj)]
        
        # adjusting ohlc price
        dt = merge(dt, fac_adj_dt, by = 'date', all.x = TRUE
            )[order(date)
            ][, factor_adj := shift(factor_adj, type = 'lead')
            ][, factor_adj := fillna(factor_adj, from_last = TRUE)
            ][is.na(factor_adj), factor_adj := 1
            ][, (cols_ohlc) := lapply(.SD, function(x) x/factor_adj), .SDcols = cols_ohlc
            ][, `:=`(
                prev_close = shift(close, type = 'lag'),
                change = close - shift(close, type = 'lag'),
                change_pct = close/shift(close, type = 'lag') - 1,
                factor_adj = NULL
            )]
        
        # pd_plot(dt2)
    }
    return(dt)
}

# dt2 = adjust_ohlc(dt, source = '163')
# merge(dat_div$`000001.SZ`, dat_split$`000001.SZ`, all = TRUE)


# dddttt = copy(dat$`000001`)
# dddttt[, close_adj := cap_market/(cap_market[.N]/close[.N])]