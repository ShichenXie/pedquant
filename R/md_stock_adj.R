# https://github.com/joshuaulrich/quantmod/blob/a8e9cb87825c0997a8468f5105db6c507b26ac5d/R/adjustOHLC.R

md_stock_adjust1 = function(dt, source=NULL, adjust = TRUE, adjfactor = NULL, ...) {
    close_adj=ratio=symbol=V1=.=dividends=splits=issue_rate=issue_price=close_prev=factor_adj_spl=factor_adj_div=factor_adj=volume=name=splits_cum=adjratio=adjratio_cumchg=NULL
    
    # return original dt 
    ## if adjust is null
    if (is.null(adjust) || isFALSE(adjust)) return(dt)
    ## if dt has OHLC columns
    cols_ohlc = c('open', 'high', 'low', 'close')
    if (!all(cols_ohlc %in% names(dt))) return(dt)
    
    if (is.null(source)) {
        if (dt[!is.na(close_adj), .N>0]) {
            source = 'yahoo'
        } else if (dt[!is.na(close_prev), .N>0]) {
            source = '163'
        }
    }
    
    # adjusting price
    ## for data from yahoo
    if (source == 'yahoo') {
        dt = copy(dt)[, adjratio := close_adj/close
        ][, (cols_ohlc) := lapply(.SD, function(x) x*adjratio), .SDcols = cols_ohlc
        ][, adjratio := NULL]
        # symbol.name = '000001.'
        # div <- getDividends(symbol.name, from="1900-01-01")
        # splits <- getSplits(symbol.name, from="1900-01-01")
    ## for data from 163
    } else if (source == '163') {
        # https://wiki.mbalib.com/wiki/%E9%99%A4%E6%9D%83%E9%99%A4%E6%81%AF%E6%97%A5
        # zhuanlan.zhihu.com/p/283168542
        if (is.null(adjfactor)) {
            dt = copy(dt)[
                order(date)
            ][, adjratio_cumchg := cumprod(close/close_prev)
            ][, close_adj := close[.N]/adjratio_cumchg[.N]*adjratio_cumchg
            ][, adjratio := close_adj/close
            ][, (cols_ohlc) := lapply(.SD, function(x) x*adjratio), .SDcols = cols_ohlc
            ][, (c('adjratio_cumchg', 'adjratio')) := NULL]
            
        } else {
        if (!inherits(adjfactor,'data.frame')) {
            symbol1 = sub('([0-9]+).+', '\\1', dt[1, symbol])
            divspl = try(md_stock_divsplit1_163(symbol1), silent = TRUE)
            if (inherits(divspl, 'try-error')) {
                warning(sprintf('Returning original data for %s',symbol1))
                return(dt)
            }
        } else {
            if (nrow(adjfactor) == 0) return(dt)
            divspl = adjfactor
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
    return(dt)
}

#' adjust stock price for split and dividend
#' 
#' \code{md_stock_adjust} adjusts the open, high, low and close stock prices for split and dividend. 
#' 
#' @param dt a list/dataframe of time series datasets that didnt adjust for split or dividend.
#' @param source the available data sources are 'yahoo' and '163'. The source will set to yahoo, if the dt has close_adj column; and will set to 163, if the dt has close_prev column. 
#' @param adjust whether to adjust the OHLC prices. If it is NULL or FALSE, return the original data. Default is TRUE. 
#' For the yahoo data, the adjustment is based on the close_adj; for the 163 data, the adjustment is based on the cumulative products of close/close_prev.
#' @param ... Additional parameters.
#' 
#' @examples 
#' \donttest{
#' dt = md_stock('600547', source = '163', date_range = 'max')
#' 
#' dtadj = md_stock_adjust(dt, source = '163')
#' }
#' @export
md_stock_adjust = function(dt, source = NULL, adjust = TRUE, ...) {
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