# ref 
# https://github.com/joshuaulrich/quantmod/blob/a8e9cb87825c0997a8468f5105db6c507b26ac5d/R/adjustOHLC.R
# https://zhuanlan.zhihu.com/p/266430753
# zhuanlan.zhihu.com/p/283168542
# https://wiki.mbalib.com/wiki/%E9%99%A4%E6%9D%83%E9%99%A4%E6%81%AF%E6%97%A5

md_stock_adj1ohlc = function(dt, adjust=FALSE, forward=TRUE, source=NULL, adjfactor=NULL, ...) {
    close_adj =  close_prev =  adjratio_cumchg =  change_pct =  adjratio = adjratio = NULL
    
    if (is.null(adjust)) return(dt)
    
    ## source ------
    if (is.null(source)) {
        if ('close_adj' %in% names(dt) && !('close_prev' %in% names(dt)) && dt[!is.na(close_adj), .N>0]) {
            source = 'yahoo'
        } else if ('close_prev' %in% names(dt) && dt[!is.na(close_prev), .N>0]) {
            source = '163'
        }
    }
    
    ## adjust ohlc ------
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
            ][]
            
            if (isTRUE(forward)) {
                dt = dt[!is.na(close), close_adj := close[.N]/adjratio_cumchg[.N]*adjratio_cumchg
                ][]
            } else {
                dt = dt[!is.na(close), close_adj := close[1]/adjratio_cumchg[1]*adjratio_cumchg
                ][]
            }
            dt[, (c('adjratio_cumchg')) := NULL][]
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
    
    return(dt)
}

#' adjust stock price for split and dividend
#' 
#' \code{md_stock_adjust} adjusts the open, high, low and close stock prices for split and dividend. 
#' 
#' @param dt a list/dataframe of time series datasets that didnt adjust for split or dividend.
#' @param adjust whether to adjust the OHLC prices, defaults to FALSE. If it is NULL, return the original data; if it is FALSE, create close_adj or change_pct column if not exist; if it is TRUE, adjust all open, high, low, close columns. The adjustment is based on the cumulative products of close/close_prev.
#' @param forward forward adjust or backward adjust, defaults to TRUE.
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
md_stock_adjust = function(dt, adjust = FALSE, forward = TRUE, ...) {
    # adj_vol
    source = list(...)[['source']]
    adj_vol = list(...)[['adj_vol']]
    # dt
    dt = check_dt(dt)
    
    # adjusted data list
    dat_list = lapply(
        split(dt, by = 'symbol'), 
        function(dts) {do.call(
            'md_stock_adj1ohlc', 
            args = list(dt = dts, source=source, adjust=adjust, forward=forward, ...) 
        )}
    )
    
    if (isTRUE(adj_vol)) dat_list = md_stock_adjvol(dat_list, ...)
    
    return(dat_list)
}

md_stock_adj1vol = function(dt, adjfactor, col_vol = 'volume', adj_ohlc = FALSE) {
    . = symbol = from =  to =  dividends =  splits =  issue_rate =  issue_price =  splits_cum =  adjfactor_spl =  adjfactor_div =  adjfactor_spl =  adjfactor_div = volume_spl = close_spl = NULL
    
    # dt = md_stock('600547', date_range = 'max', source = '163')
    # adjfactor = md_stock('600547', date_range = 'max', source = '163', type = 'adjfactor')
        
    dt = check_dt(dt)
    divspl = check_dt(adjfactor)
    symbol1 = sub('([0-9]+).+', '\\1', dt[1, symbol])
    
    if (is.null(divspl)) {
        # div <- getDividends(symbol.name, from="1900-01-01")
        # splits <- getSplits(symbol.name, from="1900-01-01")
        divspl = try(md_stock1_divsplit_163(symbol1), silent = TRUE)
    }
    
    if (inherits(divspl, 'data.frame')) {
        dtfromto = dt[,.(from = min(date), to = max(date)), by = symbol]
        divspl = divspl[dtfromto, on = 'symbol'][date >= from & date <= to]
    }
    
    if (nrow(divspl) == 0 || inherits(divspl, 'try-error')) {
        warning(sprintf('Returning original data for %s',symbol1))
        return(dt)
    } else {
        # data to calculate adjusting factor
        # dt_divspl = merge(
        #     dt, divspl, all = TRUE, 
        #     by = c("symbol", "date")
        # )[,.(
        #     symbol, date, close, dividends, splits, issue_rate, issue_price
        # )][!(is.na(dividends) & is.na(splits) & is.na(issue_rate))
        # ][!is.na(close)]
        dt_divspl = copy(divspl)
        dt_divspl[is.na(dt_divspl)] = 0
        
        # adjusting factor for split and dividend
        adj_divspl = copy(dt_divspl)[
            order(symbol, -date)
        ][, splits_cum := cumprod(1+splits),  # cumprod(1+splits+issue_rate), 
          by = 'symbol'
        ][][, `:=`(
            # adjfactor_spl = cumprod(1 + issue_price/close*issue_rate)/splits_cum,
            adjfactor_spl = 1/splits_cum,
            adjfactor_div = cumsum(dividends/splits_cum)
        ), by = 'symbol'
        ][order(symbol, date)
        ][,.(symbol, date, adjfactor_spl, adjfactor_div)]
        
        # adjusting ohlc price
        cols_adj = c('adjfactor_spl', 'adjfactor_div')
        cols_ohlc = c('open', 'high', 'low', 'close')
        
        dt_adj = merge(
            dt, adj_divspl, by = c('symbol', 'date'), all.x = TRUE
        )[order(symbol, date)
        ][, (cols_adj) := lapply(.SD, function(x) fillna(x, from_last = TRUE)), 
          by = 'symbol', .SDcols = cols_adj
        ][is.na(adjfactor_spl), adjfactor_spl := 1
        ][is.na(adjfactor_div), adjfactor_div := 0
        ]
        
        if (adj_ohlc) {
            dt_adj = dt_adj[, (cols_ohlc) := lapply(.SD, function(x) {
                # xadj = x*adjfactor_spl # adjust split only 
                xadj = x*adjfactor_spl - adjfactor_div
                xadj = round(xadj, 2)
                return(xadj)
            }), .SDcols = cols_ohlc
            ]
        }
        
        dt_adj = dt_adj[
            order(symbol, date)
        ][, volume_spl := get(col_vol)/adjfactor_spl
        ][, close_spl := close * adjfactor_spl
        ][, close_spl := fillna(close_spl), by = 'symbol'
        ][, c(names(dt), 'volume_spl', 'close_spl'), with = FALSE]#[!is.na(get(col_vol))]
        
    }
    
    return(dt_adj)
}

md_stock_adjvol = function(dt, adjfactor, col_vol = 'volume', adj_ohlc = FALSE, ...) {
    symbol = NULL
    # dt
    dt = check_dt(dt)
    
    # adjusted data list
    syblst = dt[, unique(symbol)]
    dat_list = lapply(
        syblst, 
        function(s) {
            # print(s)
            do.call(
            'md_stock_adj1vol', 
            args = list(dt = dt[symbol == s], adjfactor=adjfactor[symbol == s], col_vol=col_vol, adj_ohlc=adj_ohlc) 
        )}
    )
    names(dat_list) = syblst
    
    return(dat_list)
}
