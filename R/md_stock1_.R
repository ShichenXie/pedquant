#' query stock market data 
#' 
#' \code{md_stock} provides an interface to query stock or fund data.
#' 
#' @param symbol symbols of stock shares.
#' @param type the data type, including history, real. Defaults to history.
# @param freq data frequency, default is daily. .
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is '3y'.
#' @param from the start date. Default is NULL.
#' @param to the end date. Default is current system date.
#' @param forward whether to forward adjust the OHLC prices. If it is NULL, return the original data from source, defaults to NULL.
# If close_prev provided, the adjustment is based on the cumulative products of close/close_prev.
#' @param print_step A non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' @param ... Additional parameters.
#' 
#' 
#' @examples 
#' \dontrun{
#' # Example I: query history data
#' # us
#' FAANG = md_stock(c('META', 'AMZN', 'AAPL', 'NFLX', 'GOOG'))
#' 
#' # hkex
#' TMX = md_stock(c('00700.hk', '03690.hk', '01810.hk'))
#' 
#' # sse/szse
#' ## the symbol without suffix
#' dt_cn1 = md_stock(c("000001", "^000001", "512510"))
#' ## the symbol with suffix
#' dt_cn2 = md_stock(c("000001.sz", "000001.ss", '512510.ss'))
#' 
#'              
#' # Example III: query real prices
#' # real price for equities
#' dt_real1 = md_stock(c('META', 'AMZN', 'AAPL', 'NFLX', 'GOOG',
#'                      '00700.hk', '03690.hk', '01810.hk',
#'                      "000001", "^000001", "512510"), type = 'real')
#'                
#'                
#' # query company information 
#' dt_info1 = md_stock('600036', type = 'info')
#' }
#' 
#' @export
md_stock = function(symbol, type = 'history', date_range = "3y", from = NULL, to = Sys.Date(), forward = NULL, print_step = 1L, ...) {
    # arguments
    args = list(...)
    ## type
    type = check_arg(type, c('history', 'real', 'info')) # , 'adjfactor'
    if (type == 'spot') type = 'real'
    ## symbol
    symbol = tolower(symbol)
    ## remove ZEROs from the 163 data download
    zero_rm = args[['zero_rm']]
    if (is.null(zero_rm)) zero_rm = TRUE
    ## the environment to keep param for yahoo
    # env = args[['env']]
    # if (is.null(env)) env = parent.frame()
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    # query data
    if (type == 'real') {
        dat_list <- try(do.call('md_stock_real', args=list(symbol=symbol, ...)), silent = TRUE)
        
    }  else if (type == 'history') {
        dat_list = load_dat_loop(symbol, 'md_stock1_history_eastmoney', args = list(from = from, to = to, zero_rm = zero_rm, forward=forward, ...), print_step=print_step, sleep = args$sleep)
        
    } else if (type == 'adjfactor') (
        dat_list = load_dat_loop(symbol, 'md_stock1_divsplit_163', args = list(from = from, to = to), print_step=print_step)
        
    ) else if (type == 'info') {
        dat_list = load_dat_loop(symbol, 'md_stock1_info', args = list(...), print_step=print_step)
        
    }
    
    # remove columns
    rmcols_func = function(x) {
        name = NULL
        
        cols_rm = intersect(names(x), c('change')) #,'close_prev', 'change_pct'
        if (length(cols_rm)>0) x = x[, (cols_rm) := NULL]
        if ('name' %in% names(x)) x = x[, name := gsub('\\s', '', name)]
        return(x)
    }
    if (type %in% c('history', 'real')) {
        if (inherits(dat_list, 'list')) {
            dat_list = lapply(dat_list, rmcols_func)
        } else if (inherits(dat_list, 'data.frame')) {
            dat_list = rmcols_func(dat_list)
        }
    }
    
    if (is.null(dat_list)) return(dat_list)
    dat_list = rm_error_dat(dat_list)
    return(dat_list[])
}