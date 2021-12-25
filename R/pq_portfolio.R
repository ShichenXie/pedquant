#' calculating returns of portfolio 
#' 
#' \code{pq_portfolio} calculates the weight returns or the wealth index of a portfolio of assets.
#'
#' @param dt a list/dataframe of returns by asset
#' @param w a dataframe of weights by asset. If it is NULL, then the weights by asset are equal.
#' @param rcol the returns column name
#' @param wcol the weights column name
#' @param wealthindex logical, whether to return a wealth index. Defaults to FALSE. 
#' @param method the method to calculate asset returns, the available values include arithmetic and log, defaults to arithmetic.
#'
#' @examples
#' library(pedquant)
#' library(data.table)
#' 
#' data(dt_banks)
#' datadj = md_stock_adjust(dt_banks, adjust = FALSE)
#' 
#' dt = pq_return(datadj, x = 'close_adj', freq = 'monthly')
#' w = data.table(
#'    symbol = c("601288.SS","601328.SS","601398.SS","601939.SS","601988.SS"), 
#'    weights = c(0.1, 0.2, 0.3, 0.3, 0.1)
#'    )
#' 
#' ret = pq_portfolio(dt, w)
#' retidx = pq_portfolio(dt, w, wealthindex = TRUE)
#' pq_plot(retidx, x = 'wealthindex')
#' 
#' @importFrom stats weighted.mean
#' @export
pq_portfolio = function(dt, w=NULL, rcol = 'returns', wcol = 'weights', wealthindex = FALSE, method = 'arithmetic') {
    . = chg_syb = idx_syb = returns = wealthindex_lag = weights = NULL
    
    ## rbind list of dataframes
    dt = check_dt(dt)
    
    if (is.null(w)) w = data.table(symbol = unique(dt$symbol))[, weights := 1]
    w = setDT(w) 
    
    portfolio_idxret = merge(
        setDT(merge.data.frame(w, dt[,.(date = unique(date))]))[], 
        dt, 
        by = c('symbol', 'date'), all = TRUE
    )[, chg_syb := sum(get(rcol),1,na.rm = TRUE), keyby = c('symbol', 'date')
    ][, idx_syb := cumprod(chg_syb), by = 'symbol'
    ][, .(wealthindex = weighted.mean(idx_syb, get(wcol))), keyby = 'date'
    ][, wealthindex_lag := shift(wealthindex, type = 'lag', fill = 1)
    ][, returns := do.call(
        sprintf('return_%s', method), 
        args = list(x = wealthindex, x_lag = wealthindex_lag)
    )] 
    
    if (wealthindex == TRUE) {
        portfolio_idxret = portfolio_idxret[, .(date, returns, wealthindex)]
    } else {
        portfolio_idxret = portfolio_idxret[, .(date, returns)]
    }
    
    return(portfolio_idxret)
}

