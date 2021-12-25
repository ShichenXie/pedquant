md_stock_adjust1_split = function() {
    # adjust volume due to splits, still needs to modify
    if (is.null(adjfactor) || nrow(adjfactor) == 0) {
        return(dt)
    } else {
        # div <- getDividends(symbol.name, from="1900-01-01")
        # splits <- getSplits(symbol.name, from="1900-01-01")
        
        if (!inherits(adjfactor,'data.frame')) {
            symbol1 = sub('([0-9]+).+', '\\1', dt[1, symbol])
            divspl = try(md_stock_divsplit1_163(symbol1), silent = TRUE)
            if (inherits(divspl, 'try-error')) {
                warning(sprintf('Returning original data for %s',symbol1))
                return(dt)
            }
        }
        
        
        # data to calculate adjusting factor
        ddtt = merge(dt, divspl, all.x = TRUE, by = 'date')[,.(
            date, close, dividends, splits, issue_rate, issue_price
        )][!(is.na(dividends) & is.na(splits) & is.na(issue_rate))
        ][!is.na(close)]
        ddtt[is.na(ddtt)] = 0
        
        # adjusting factor for split and dividend
        fac_adj_dt = copy(ddtt)[
            order(-date)
        ][, splits_cum := cumprod(1+splits+issue_rate)
        ][, `:=`(
            factor_adj_spl = cumprod(1 + issue_price/close*issue_rate)/splits_cum,
            factor_adj_div = cumsum(dividends/splits_cum)
        )][order(date)
        ][,.(date, factor_adj_spl, factor_adj_div)]
        
        # adjusting ohlc price
        adj_cols = c('factor_adj_spl', 'factor_adj_div')
        dt_adj = merge(dt, fac_adj_dt, by = 'date', all.x = TRUE
        )[order(date)
        ][, (adj_cols) := lapply(.SD, function(x) fillna(x, from_last = TRUE)), .SDcols=adj_cols
        ][is.na(factor_adj_spl), factor_adj_spl := 1
        ][is.na(factor_adj_div), factor_adj_div := 0
        ][, (cols_ohlc) := lapply(.SD, function(x) {
            if (adjust == 'split') {
                xadj = x*factor_adj_spl
            } else if (adjust == 'dividend') {
                xadj = x*factor_adj_spl - factor_adj_div
            }
            xadj = round(xadj, 2)
            return(xadj)
        }), .SDcols = cols_ohlc
        ][, `:=`(
            symbol = NULL, name = NULL, 
            volume = volume*factor_adj_spl,
            factor_adj_spl = NULL, 
            factor_adj_div = NULL
        )]
        dt = cbind(dt[,.(symbol, name)], dt_adj)
    }
}