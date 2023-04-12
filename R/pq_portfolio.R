#' calculating returns/equity of portfolio 
#' 
#' \code{pq_portfolio} calculates the weighted returns or the equity of a portfolio assets.
#'
#' @param dt a list/dataframe of price by asset.
#' @param orders a data frame of transaction orders, which includes symbol, date, prices, volumes and type columns. 
#' @param x the column name of adjusted asset price, defaults to close.
#' @param dtb a list/dataframe of price base asset.
#' @param init_fund initial fund value.
#' @param method the method to calculate asset returns, the available values include arithmetic and log, defaults to arithmetic.
#' @param cols_keep the columns keep in the return data. The columns of symbol, name and date will always kept if they are exist in the input data.
#' @param ... ignored
#'
#' @examples
#' library(pedquant)
#' 
#' data(dt_banks)
#' datadj = md_stock_adjust(dt_banks)
#' 
#' 
#' # example I 
#' orders = data.frame(
#'     symbol = c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 
#'     volumes = c(100, 200, 300, 300, 100)
#' )
#' dtRa = pq_portfolio(datadj, orders=orders) 
#' 
#' e1 = pq_plot(dtRa, y = 'cumreturns')
#' e1[[1]]
#' 
#' 
#' # example II 
#' data(dt_ssec)
#' orders = data.frame(
#'     symbol = rep(c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 3), 
#'     date = rep(c('2009-03-02', '2010-01-04', '2014-09-01'), each = 5), 
#'     volumes = rep(c(100, 200, 300, 300, 100), 3) * rep(c(1, -1, 2), each = 5)
#' )
#' dtRab = pq_portfolio(datadj, orders=orders, dtb = dt_ssec, init_fund = 10000) 
#' 
#' e2 = pq_plot(dtRab, y = 'cumreturns', yb = 'cumreturns_000001.SS', addti = list(portfolio=list()))
#' e2[[1]]
#' 
#' 
#' # example III
#' orders = data.frame(symbol = "000001.SS", 
#'      date = c("2009-04-13", "2010-03-24", "2014-08-13", "2015-09-10"), 
#'      volumes = c(400, -400, 300, -300))
#' dtRa2 = pq_portfolio(dt_ssec, orders=orders, cols_keep = 'all')
#' 
#' e3 = pq_plot(dtRa2, y = 'close', addti = list(cumreturns=list(), portfolio=list()))
#' e3[[1]]
#' 
#' @importFrom stats weighted.mean
#' @export
pq_portfolio = function(dt, orders, x = 'close', dtb = NULL, init_fund = NULL, method = 'arithmetic', cols_keep=NULL, ...) {
    . = equity = equityindex = fund = chg = returns = symbol = value = balance = blchg = cumreturns = NULL

    w = 'weights'
    v = 'volumes'
    
    args = list(...)
    x_value = args[['x_value']]
    if (is.null(x_value)) x_value = x
    # dtv = args[['dtv']]
    # if (!is.null(dtv)) orders = dtv
    
    # dt
    dt = check_dt(dt)
    # orders
    orders = check_odr(orders)

    ## adding date column
    if (!('date' %in% names(orders))) orders = orders[, date := min(dt$date)] 
    ## adding weights column
    orders = orders[is.na(get(v)), (v) := 0
    ][order(symbol, date)
    ][, (w) := cumsum(get(v)), keyby = 'symbol']
    
    ## merge dt and orders
    dt_dtv = Reduce(function(x,y) merge(
        x, y, all = TRUE, 
        by = intersect(intersect(c('symbol','date'), names(x)), names(x))
    ),
    list(setDT(expand.grid(symbol = as.character(unique(dt$symbol)), date = as_date(unique(dt$date)), stringsAsFactors = FALSE))[], 
         orders, dt[, c(c('symbol','date'), setdiff(names(dt), names(orders))), with = FALSE] 
    ))[, (c(w,x)) := lapply(.SD, fillna), .SDcols=c(w,x), by='symbol'
    ][]
    ## adding value column
    if (!('value' %in% names(orders))) dt_dtv = dt_dtv[, value := get(v) * get(x_value)]
    
    # equity
    portfolio_equity = dt_dtv[, .(
        equity = sum(get(x) * get(w), na.rm=TRUE),
        # equityindex = weighted.mean(get(x), get(w), na.rm=TRUE), 
        value = sum(value, na.rm = TRUE)
    ), keyby = 'date' 
    ][, fund := round(cumsum(-value), 2)
    ][,.(date, equity, fund)
     ]
    
    # initial fund
    if (is.null(init_fund)) {
        init_fund = round(abs(portfolio_equity[, min(fund)]), 2)
        warning(sprintf('The initial value is setting to %s', init_fund))
    }
    portfolio_equity = portfolio_equity[, fund := fund + init_fund][, balance := equity + fund]
    
    # returns
    portfolio_equity[
        , returns := do.call(
            sprintf('return_%s', method), 
            args = list(x=balance, shift(balance, type ='lag'))
        )
    ][#!is.na(cumreturns)
    ][, chg := sum(1, returns, na.rm = TRUE), by = 'date'
    ][, cumreturns := cumprod(chg) ]
    
    
    bal_cols = c('equity', 'fund', 'balance')
    setnames(portfolio_equity, bal_cols, paste0('portfolio_',bal_cols))
    
    if (dt[,length(unique(symbol))==1] && !is.null(cols_keep)) {
        if (cols_keep == 'all') cols_keep = names(dt)
        cols_keep = intersect(names(dt), cols_keep)
        portfolio_equity = merge(dt[,cols_keep,with=FALSE], portfolio_equity, by='date')
    }
    
    # baseline asset
    if (!is.null(dtb)) {
        dtb = check_dt(dtb, symb_name = FALSE)
        
        bldt = dcast(dtb, 'date ~ symbol', value.var = x)[]
        blsyb = dtb[1,symbol]
        blret = sprintf('returns_%s', blsyb)
        blcumret = sprintf('cumreturns_%s', blsyb)
        
        portfolio_equity = merge(
            portfolio_equity, bldt, 
            by = 'date', all.x=TRUE
        )[, (blret) := do.call(
            sprintf('return_%s', method), 
            args = list(x=get(blsyb), shift(get(blsyb), type ='lag'))
        )][, blchg := sum(1, get(blret), na.rm = TRUE), by = 'date'
        ][, (blcumret) := cumprod(blchg)
        ][, (c(blsyb, 'blchg')) := NULL]
    }
    
    if (!('symbol' %in% names(portfolio_equity))) portfolio_equity = cbind(data.table(symbol='portfolio'), portfolio_equity)  
    return(portfolio_equity[])
}

# portfolio = function(x, v, init_fund=NULL) {
#     return(invisible())
# }
