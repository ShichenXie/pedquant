# ref 
# https://github.com/joshuaulrich/quantmod/blob/a8e9cb87825c0997a8468f5105db6c507b26ac5d/R/adjustOHLC.R
# https://zhuanlan.zhihu.com/p/266430753
# zhuanlan.zhihu.com/p/283168542
# https://wiki.mbalib.com/wiki/%E9%99%A4%E6%9D%83%E9%99%A4%E6%81%AF%E6%97%A5

md_stock_adjust1 = function(dt, adjust=TRUE, source=NULL, adjfactor=NULL, ...) {
    . = adjratio = adjratio_cumchg = close_adj=close_prev=dividends = factor_adj_div=factor_adj_spl=issue_price=issue_rate=name=splits = splits_cum=symbol=volume = change_pct = NULL
    
    if (is.null(adjust)) return(dt)
    
    ## source 
    if (is.null(source)) {
        if ('close_adj' %in% names(dt) && !('close_prev' %in% names(dt)) && dt[!is.na(close_adj), .N>0]) {
            source = 'yahoo'
        } else if ('close_prev' %in% names(dt) && dt[!is.na(close_prev), .N>0]) {
            source = '163'
        }
    }
    
    ## OHLC columns
    cols_ohlc = c('open', 'high', 'low', 'close')
    if (!all(cols_ohlc %in% names(dt))) return(dt)
    
    ## create close_adj for 163 prices
    if (!('close_adj' %in% names(dt)) & ('close_prev' %in% names(dt))) {
        if (as.character(source) == '163') {
            dt = copy(dt)[
                order(date)
            ][!is.na(close_prev), adjratio_cumchg := cumprod(close/close_prev)
            ][is.na(close_prev), adjratio_cumchg := 1
            ][][, close_adj := close[.N]/adjratio_cumchg[.N]*adjratio_cumchg
            ][][, (c('adjratio_cumchg')) := NULL][]
        }
    }
    ## create change_pct for yahoo data
    if (!('change_pct' %in% names(dt)) & ('close_adj' %in% names(dt))) {
        if (as.character(source) == 'yahoo') {
            dt = copy(dt)[
                order(date)
            ][, change_pct := (close_adj/shift(close_adj, 1, type = 'lag')-1)*100]
        }
    }
    
    ## adjust ohlc columns
    if (isFALSE(adjust)) {
        return(dt)
    } else {
        # adjust all ohlc prices if adjust is TRUE
        dt = copy(dt)[, adjratio := close_adj/close
        ][, (cols_ohlc) := lapply(.SD, function(x) x*adjratio), .SDcols = cols_ohlc
        ][, adjratio := NULL]
    }
    
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

#' adjust stock price for split and dividend
#' 
#' \code{md_stock_adjust} adjusts the open, high, low and close stock prices for split and dividend. 
#' 
#' @param dt a list/dataframe of time series datasets that didnt adjust for split or dividend.
#' @param adjust whether to adjust the OHLC prices, defaults to TRUE. If it is NULL, return the original data; if it is FALSE, create close_adj or change_pct column if not exist; if it is TRUE, adjust all open, high, low, close columns.
#' For the yahoo data, the adjustment is based on the close_adj; for the 163 data, the adjustment is based on the cumulative products of close/close_prev.
#' @param source the available data sources are 'yahoo' and '163'. The source will set to yahoo, if the dt has close_adj column; and will set to 163, if the dt has close_prev column. 
#' @param ... Additional parameters.
#' 
#' @examples 
#' \donttest{
#' dt = md_stock('600547', source = '163', date_range = 'max')
#' 
#' dtadj = md_stock_adjust(dt, source = '163')
#' }
#' @export
md_stock_adjust = function(dt, adjust = TRUE, source = NULL, ...) {
    symbol = NULL
    
    # dt
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    dt = setDT(dt)
    # adjfactor
    adjfactor = list(...)[['adjfactor']]
    if (inherits(adjfactor, 'list')) adjfactor = rbindlist(adjfactor, fill = TRUE)
    if (inherits(adjfactor, 'data.frame')) adjfactor = setDT(adjfactor)

    ## single series
    dt_list = list()
    sybs = dt[, unique(symbol)]
    adjfactor_s = NULL
    for (s in sybs) {
        dt_s = dt[symbol == s]
        if (inherits(adjfactor, 'data.frame')) adjfactor_s = adjfactor[symbol == s]
        setkeyv(dt_s, "date")
        
        dt_list[[s]] = do.call(
            'md_stock_adjust1', 
            args = list(dt = dt_s, source=source, adjust=adjust, adjfactor = adjfactor_s) )
    }
    return(dt_list)
    
}

# dt2 = adjust_ohlc(dt, source = '163')
# merge(dat_div$`000001.SZ`, dat_split$`000001.SZ`, all = TRUE)


# dddttt = copy(dat$`000001`)
# dddttt[, close_adj := cap_market/(cap_market[.N]/close[.N])]



# https://zhuanlan.zhihu.com/p/266430753
if (F) {
    library(data.table)
    library(pedquant)
    
    datorig = md_stock('600036', source = '163', date_range = 'max', adjust = NULL)
    datadj = md_stock('600036', source = '163', date_range = 'max', type = 'adjfactor')
    
}