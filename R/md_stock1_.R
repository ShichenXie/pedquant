#' dataset of shanghai composite index
#'
#' The daily historical Shanghai Composite Index from the beginning of the Index to Sept 1, 2020
#'
#' @format A data frame with 7506 rows and 15 variables:
#' \describe{
#'   \item{symbol}{stock ticker symbol}
#'   \item{name}{stock ticker name}
#'   \item{date}{trade date}
#'   \item{open}{stock price at the open of trading}
#'   \item{high}{stock price at the highest point during trading}
#'   \item{low}{stock price at the lowest point during trading}
#'   \item{close}{stock price at the close of trading}
#'   \item{close_prev}{stock price at the close of previous trading day}
#'   \item{change_pct}{change percentage of stock close price}
#'   \item{volume}{number of shares traded}
#'   \item{amount}{monetary value of shares traded}
#'   \item{turnover}{rate of shares traded over total}
#'   \item{cap_market}{tradable market capitalisation}
#'   \item{cap_total}{total market capitalisation}
#'   \item{unit}{price unit, such as in CNY/USD}
#' }
#' 
"ssec"

#' query stock market data 
#' 
#' \code{md_stock} provides an interface to query EOD (end of date) stock prices.
#' 
#' @param symbol symbols of stock shares.
#' @param source the available data sources are 'yahoo' (\url{https://finance.yahoo.com}) and '163' (\url{https://money.163.com}).
#' @param type the data type, including history, adjfactor and spot. Default is history.
#' @param freq default is daily. It supports daily, weekly and monthly for yahoo data; daily for 163 data.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is '3y'.
#' @param from the start date. Default is NULL.
#' @param to the end date. Default is current system date.
#' @param adjust whether to adjust the OHLC prices. If it is NULL or FALSE, return the original data. Default is FALSE. 
#' For the yahoo data, the adjustment is based on the close_adj; for the 163 data, the adjustment is based on the cumulative products of close/close_prev.
#' @param print_step A non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' @param ... Additional parameters.
#' 
#' 
#' @examples 
#' \dontrun{
#' # Example I
#' # query history prices from yahoo
#' dt_yahoo1 = md_stock(symbol=c("^GSPC", "000001.SS"))
#' 
#' # FAANG
#' FAANG = md_stock(c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'), date_range = 'max')
#' 
#' # for Chinese shares/fund
#' ## the symbol without suffix
#' dt_yahoo2 = md_stock(c("000001", "^000001", "512510"))
#' ## the symbol with suffix
#' dt_yahoo3 = md_stock(c("000001.sz", "000001.ss"))
#' 
#' # adjust factors, splits and dividend
#' dt_adj = md_stock(symbol=c("AAPL", "000001.SZ", "000001.SS"), 
#'                     type='adjfactor', date_range='max')
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
#' # query spot prices of all A/B shares and index in sse and szse
#' dt_spot3 = md_stock(symbol=c('a', 'b', 'index'), source="163", type='spot')
#' 
#' # show spot prices and sector/industry
#' dt_spot4 = md_stock(symbol = c('a', 'b', 'index', 'fund'), source = '163', 
#'   type = 'spot', show_tags = TRUE)
#' 
#' # Example IV
#' # query company information, including profile, IPO, structure of income, structure of employee
#' dt_info = md_stock('600036', type = 'info')
#' 
#' # query structure of income in history only
#' dt_info2 = md_stock('600036', type = 'info', str_income_hist = TRUE)
#' 
#' }
#' 
#' @export
md_stock = function(symbol, source = "yahoo", type='history', freq = "daily", date_range = "3y", from = NULL, to = Sys.Date(), adjust = FALSE, print_step = 1L, ...) {
    # cat(source,"\n")
    if (source == '163') {
        check_internet('www.163.com')
    } else if (source == 'yahoo') {
        check_internet('www.yahoo.com')
    }
    # arguments
    type = check_arg(type, c('history', 'spot', 'adjfactor', 'info'))
    
    source = check_arg(as.character(source), c('yahoo','163'), default = 'yahoo')
    if (type %in% c('spot', 'info')) source = '163'
    
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
    
    if (is.null(dat)) return(dat)
    # remove error symbols
    error_symbols = names(dat)[which(sapply(dat, function(x) inherits(x, 'try-error')))]
    if (length(error_symbols) > 0) {
        warning(sprintf('The following symbols can\'t imported:\n%s', paste0(error_symbols, collapse=', ')))
        dat = dat[setdiff(names(dat), error_symbols)]
    }
    return(dat)
}