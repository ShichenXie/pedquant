
#' get stock market data 
#' 
#' \code{md_stock} provides an interface to query historical stock data.
#' 
#' @param symbol symbols stock share.
#' @param source data source. The available sources including 'yahoo' and '163'.
#' @param freq default is daily. It supports daily, weekly and monthly for data from yahoo; daily for data from 163.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is '3y'.
#' @param from the start date. Default is NULL.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' @param ... Additional parameters.
#' 
#' @examples 
#' \dontrun{
#' # Example I
#' # query Chinese stock data from 163
#' dat1 = md(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163")
#' 
#' dat2 = md(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163", freq="spot")
#' 
#' # query spot price of all A shares in sse and szse
#' dat3 = md(symbol='a', source="163", freq="spot")
#' 
#' # query spot price of all index in sse and szse
#' dat4 = md(symbol='index', source="163", freq="spot")
#' 
#' 
#' # Example II
#' # query Chinese commodity future price
#' dat_com1 = md(symbol='RB0', source="sina")
#' 
#' dat_com2 = md(symbol=c("AG0","AU0"), source="sina")
#' 
#' 
#'  # Example III
#'  # query data from yahoo
#'  dat_yahoo = md(symbol=c("^GSPC", "000001.SS", "EURUSD=X"))
#'  
#'  # for Chinese share
#'  dat_yahoo2 = md(c("000001", "^000001"))
#' }
#' 
#' @export
md_stock = function(symbol, source = "yahoo", freq = "daily", date_range = "3y", from = NULL, to = Sys.Date(), type='history', adjust = TRUE, print_step = 1L, ...) {
    # cat(source,"\n")
    # arguments
    source = check_arg(as.character(source), c('yahoo','163'), default = 'yahoo')
    type = check_arg(type, c('history', 'dividend', 'split'), default = 'history')
    syb = tolower(symbol)
    
    na_rm = list(...)[['na_rm']]
    if (is.null(na_rm)) na_rm = TRUE
    
    fillzero = list(...)[['fillzero']]
    if (is.null(fillzero)) fillzero = FALSE

    env = list(...)[['env']]
    if (is.null(env)) env = parent.frame()
    
    # from
    date_range = check_date_range(date_range, default = "max")
    from = get_from_daterange(date_range, from, to, min_date = "1000-01-01")
    
    # data
    dat = try(do.call(paste0("md_stock_", source), args=list(symbol = syb, freq = freq, from = from, to = to, print_step = print_step, env = env, adjust=adjust, fillzero=fillzero, na_rm=na_rm, type=type, ...)), silent = TRUE)
    return(dat)
}