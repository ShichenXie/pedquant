#' calculating returns/equity of portfolio 
#' 
#' \code{pq_portfolio} calculates the weighted returns or the equity of a portfolio assets.
#'
#' @param dt a list/dataframe of price by asset.
#' @param dtv a dataframe of transaction volume by asset.
#' @param x the column name of adjusted asset price.
#' @param v the column name of asset volume, defaults to volume.
#' @param init_value initial equity value.
#' @param method the method to calculate asset returns, the available values include arithmetic and log, defaults to arithmetic.
#'
#' @examples
#' library(pedquant)
#' library(data.table)
#' 
#' data(dt_banks)
#' datadj = md_stock_adjust(dt_banks, adjust = FALSE)
#' 
#' # example I 
#' dtv = data.table(
#'     symbol = c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 
#'     volume = c(100, 200, 300, 300, 100)
#' )
#' dtRa = pq_portfolio(datadj, x='close_adj', dtv=dtv) 
#' pq_plot(dtRa, x = 'cumreturns')
#' 
#' dtRb = pq_return(dt_ssec, x = 'close', freq = 'daily', cumreturns = TRUE)
#' pq_plot(list(Ra = dtRa, Rb = dtRb$`000001.SS`), x = 'cumreturns', 
#'         multi_series = list(nrow=1, ncol=1))
#' 
#' # example II 
#' dtv = data.table(
#'     symbol = rep(c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 3), 
#'     date = rep(c('2009-03-02', '2010-01-04', '2014-09-01'), each = 5), 
#'     volume = rep(c(100, 200, 300, 300, 100), 3) * rep(c(1, -1, 2), each = 5)
#' )
#' dtRa2 = pq_portfolio(datadj, x='close_adj', dtv=dtv, init_value = 10000) 
#' pq_plot(dtRa2, x = 'balance', 
#'         addti = list(equity = list(), fund = list()))
#' 
#' @importFrom stats weighted.mean
#' @export
pq_portfolio = function(dt, dtv, x, v = 'volume', init_value = NULL, method = 'arithmetic') {
    . = equity = equityindex = fund = chg = returns = symbol = value = balance = NULL

    # dt
    dt = check_dt(dt)
    # dtv
    dtv = check_dt(dtv, symb_name = FALSE)
    w = 'weights'
    if (v == w) {
        setnames(dtv, v, 'volume')
        v = 'volume'
    } 
    ## adding date column
    if (!('date' %in% names(dtv))) dtv = dtv[, date := min(dt$date)] 
    ## adding weights column
    dtv = dtv[is.na(get(v)), (v) := 0
    ][order(symbol, date)
    ][, (w) := cumsum(get(v)), keyby = 'symbol']
    
    ## merge dt and dtv
    dt_dtv = Reduce(function(x,y) merge(
        x, y, all = TRUE, 
        by = intersect(intersect(c('symbol','date'), names(x)), names(x))
    ),
    list(setDT(expand.grid(symbol = as.character(unique(dt$symbol)), date = as_date(unique(dt$date)), stringsAsFactors = FALSE))[], 
         dtv, dt[, c(c('symbol','date'), setdiff(names(dt), names(dtv))), with = FALSE] 
    ))[, (c(w,x)) := lapply(.SD, fillna), .SDcols=c(w,x), by='symbol'
    ][]
    ## adding value column
    if (!('value' %in% names(dtv))) dt_dtv = dt_dtv[, value := get(v) * get(x)]
    
    # equity
    portfolio_equity = dt_dtv[, .(
        equity = sum(get(x) * get(w), na.rm=TRUE),
        equityindex = weighted.mean(get(x), get(w), na.rm=TRUE), 
        value = sum(value, na.rm = TRUE)
    ), keyby = 'date' 
    ][, fund := round(cumsum(-value), 2)
    ][, returns := do.call(
        sprintf('return_%s', method), 
        args = list(x=equityindex, shift(equityindex, type ='lag'))
    )][#!is.na(cumreturns)
     ][, chg := sum(1, returns, na.rm = TRUE), by = 'date'
     ][,.(date, returns, cumreturns = cumprod(chg), equity, fund)
     ]
    
    if (is.null(init_value)) {
        init_value = round(abs(portfolio_equity[, min(fund)]), 2)
        warning(sprintf('The initial value is setting to %s', init_value))
    }
    portfolio_equity = portfolio_equity[, fund := fund + init_value][, balance := equity + fund]
    
    return(portfolio_equity[])
}

