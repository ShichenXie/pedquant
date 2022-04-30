#' calculating returns/equity of portfolio 
#' 
#' \code{pq_portfolio} calculates the weighted returns or the equity of a portfolio assets.
#'
#' @param dt a list/dataframe of price by asset.
#' @param dtv a dataframe of transaction volume by asset.
#' @param x the column name of adjusted asset price.
#' @param v the column name of asset volume, defaults to volume.
#' @param init_fund initial fund value.
#' @param method the method to calculate asset returns, the available values include arithmetic and log, defaults to arithmetic.
#' @param ... ignored
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
#' e1 = pq_plot(dtRa, y = 'cumreturns')
#' e1[[1]]
#' 
#' dtRb = pq_return(dt_ssec, x = 'close', freq = 'daily', cumreturns = TRUE)
#' e2 = pq_plot(list(Ra = dtRa, Rb = dtRb$`000001.SS`), y = 'cumreturns', 
#'         arrange = list(rows=1, cols=1))
#' e2[[1]]
#' 
#' # example II 
#' dtv = data.table(
#'     symbol = rep(c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 3), 
#'     date = rep(c('2009-03-02', '2010-01-04', '2014-09-01'), each = 5), 
#'     volume = rep(c(100, 200, 300, 300, 100), 3) * rep(c(1, -1, 2), each = 5)
#' )
#' dtRa2 = pq_portfolio(datadj, x='close_adj', dtv=dtv, init_fund = 10000) 
#' e3 = pq_plot(dtRa2, y = 'balance', 
#'         addti = list(equity = list(), fund = list()))
#' e3[[1]]
#' 
#' @importFrom stats weighted.mean
#' @export
pq_portfolio = function(dt, dtv, x, v = 'volume', init_fund = NULL, method = 'arithmetic', ...) {
    . = equity = equityindex = fund = chg = returns = symbol = value = balance = NULL

    w = 'weights'
    x_value = list(...)[['x_value']]
    if (is.null(x_value)) x_value = x
    # dt
    dt = check_dt(dt)
    # dtv
    dtv = check_dt(dtv, symb_name = FALSE)
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
    if (!('value' %in% names(dtv))) dt_dtv = dt_dtv[, value := get(v) * get(x_value)]
    
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
     ][,.(name='portfolio', date, returns, cumreturns = cumprod(chg), equity, fund)
     ]
    
    if (is.null(init_fund)) {
        init_fund = round(abs(portfolio_equity[, min(fund)]), 2)
        warning(sprintf('The initial value is setting to %s', init_fund))
    }
    portfolio_equity = portfolio_equity[, fund := fund + init_fund][, balance := equity + fund]
    
    return(portfolio_equity)
}


