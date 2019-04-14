
#' query stock market data 
#' 
#' \code{md_stock} provides an interface to query EOD (end of date) stock prices.
#' 
#' @param symbol symbols of stock shares.
#' @param source the available data sources are 'yahoo' (\url{http://finance.yahoo.com}) and '163' (\url{http://money.163.com}).
#' @param type the data type, including history, dividend and split. Default is history.
#' @param freq default is daily. It supports daily, weekly and monthly for yahoo data; daily for 163 data.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is '3y'.
#' @param from the start date. Default is NULL.
#' @param to the end date. Default is current system date.
#' @param adjust logical, whether to adjust the prices for dividend. The price data already adjust for splits by default.
#' @param print_step A non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' @param ... Additional parameters.
#' 
#' 
#' @examples 
#' \donttest{
#' # Example I
#' # query history prices from yahoo
#' dt_yahoo1 = md_stock(symbol=c("^GSPC", "000001.SS"))
#' 
#' # FAANG
#' FAANG = md_stock(c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'), date_range = 'max')
#' 
#' # for Chinese shares
#' ## the symbol without suffix
#' dt_yahoo2 = md_stock(c("000001", "^000001"))
#' ## the symbol with suffix
#' dt_yahoo3 = md_stock(c("000001.sz", "000001.ss"))
#' 
#' # split
#' dt_split = md_stock(symbol=c("AAPL", "000001.SZ", "000001.SS"), 
#'                     type='split', date_range='max')
#' # dividend
#' dt_dividend = md_stock(symbol=c("AAPL", "000001.SZ", "000001.SS"), 
#'                        type='dividend', date_range='max')
#' 
#'  
#' # Example II
#' # query history prices from 163
#' dt1 = md_stock(symbol=c('600000', '000001', '^000001', '^399001'), 
#'                source="163")
#' 
#' # valuation ratios (pe, pb, ps)
#' # only available for stock shares in sse and szse
#' dt2 = md_stock(symbol=c('600000', '000001', '^000001', '^399001'), 
#'                source="163", valuation = TRUE)
#'              
#'              
#' # Example III
#' # query spot prices
#' dt_spot1 = md_stock(symbol=c('600000.SS', '000001.SZ', '000001.SS', '399001.SZ'), 
#'                     type='spot', source="163")
#' 
#' # query spot prices of all A shares in sse and szse
#' dt_spot2 = md_stock(symbol='a', source="163", type='spot')
#' # query spot prices of all index in sse and szse
#' dt_spot3 = md_stock(symbol='index', source="163", type='spot')
#' 
#' }
#' 
#' @export
md_stock = function(symbol, source = "yahoo", freq = "daily", date_range = "3y", from = NULL, to = Sys.Date(), type='history', adjust = FALSE, print_step = 1L, ...) {
    # cat(source,"\n")
    
    # arguments
    source = check_arg(as.character(source), c('yahoo','163'), default = 'yahoo')
    syb = tolower(symbol)
    ## remove NAs from the yahoo data
    na_rm = list(...)[['na_rm']]
    if (is.null(na_rm)) na_rm = TRUE
    ## remove ZEROs from the 163 data download
    zero_rm = list(...)[['zero_rm']]
    if (is.null(zero_rm)) zero_rm = TRUE
    ## the environment to keep param for yahoo
    env = list(...)[['env']]
    if (is.null(env)) env = parent.frame()
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = "1000-01-01", default_date_range = '3y')
    from = ft$f
    to = ft$t
    
    
    # data
    dat = try(do.call(paste0("md_stock_", source), args=list(symbol = syb, freq = freq, from = from, to = to, print_step = print_step, env = env, adjust=adjust, zero_rm=zero_rm, na_rm=na_rm, type=type, ...)), silent = TRUE)
    
    # remove error symbols
    error_symbols = names(dat)[which(sapply(dat, function(x) inherits(x, 'try-error')))]
    if (length(error_symbols) > 0) {
        warning(sprintf('The following symbols can\'t imported:\n%s', paste0(error_symbols, collapse=', ')))
        dat = dat[setdiff(names(dat), error_symbols)]
    }
    return(dat)
}