# get market data # getmd
# - froex
# - bond, money
# - stock, index
# - commodity

# stock
# - symbol, name, exchange, sector, industry, first date point, 


#' get market data
#' 
#' \code{getmd} provides an interface to get historical data for different markets, such as forex, bond, money, stock and commodity, which are provided public sources.
#' 
#' @param symbol symbols market data, which is available via getmd_symbol.
#' @param source data sources. The available sources including 'yahoo', '163', 'sina' and 'original'.
#' @param freq default is daily. It supports daily, weekly and monthly for data from yahoo; daily for data from 163 and sina.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is max.
#' @param from the start date. Default is '2010-01-01'.
#' @param to the end date. Default is current system date.
#' @param adjust logical. Argument for data download from yahoo. Default is FALSE. If it is TRUE, the open, high, low and close values will adjusted based on close_adj. 
#' @param fillzero logical. Argument for data download from 163. Default is TRUE If it is TRUE, the zeros in dataset will be filled with last non-zero values.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' \dontrun{
#' # Example I
#' # get Chinese stock data from 163
#' dat1 = getmd(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163")
#' 
#' dat2 = getmd(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163", freq="spot")
#' 
#' # get spot price of all A shares in sse and szse
#' dat3 = getmd(symbol='a', source="163", freq="spot")
#' 
#' # get spot price of all index in sse and szse
#' dat4 = getmd(symbol='index', source="163", freq="spot")
#' 
#' 
#' # Example II
#' # get Chinese commodity future price
#' dat_com1 = getmd(symbol='RB0', source="sina")
#' 
#' dat_com2 = getmd(symbol=c("AG0","AU0"), source="sina")
#' 
#' 
#'  # Example III
#'  # get data from yahoo
#'  dat_yahoo = getmd(symbol=c("^GSPC", "000001.SS", "EURUSD=X"))
#'  
#'  # for Chinese share
#'  dat_yahoo2 = getmd(c("000001", "^000001"))
#' }
#' 
#' @export
#' 
getmd = function(symbol, source = "yahoo", freq = "daily", date_range="3y", from = NULL, to = Sys.Date(), adjust=FALSE, fillzero = TRUE, print_step = 1L) {
    cat(source,"\n")
    
    # from
    date_range = check_date_range(date_range, default = "max")
    from = get_from_daterange(date_range, to, min_date = "1000-01-01")
    
    # arguments
    args = list(symbol=symbol, freq=freq, from=from, to=to, print_step=print_step, env = parent.frame(), fillzero = fillzero, adjust = adjust)
    
    # return data
    rt = try(do.call(paste0("getmd_", source), args), silent = TRUE)
    return(rt)
}

#' get symbols of market data
#' 
#' \code{getmd_symbol} provides an interface to get symbols of different markets, which can query data via \code{getmd}.
#' 
#' @param market there are six markets, 'currency', 'bond', 'money', 'index', 'stock' and 'commodity'.
#' @param source data sources. The available data sources including 'yahoo', '163', 'sina' and 'original'. 
#' 
#' @examples 
#' \dontrun{
#' # specify source
#' syb_163 = getmd_symbol(source="163")
#' 
#' # specify market
#' syb_stock = getmd_symbol(market="stock")
#' 
#' # specify both source and market
#' syb_commodity_sina = getmd_symbol(market="commodity", source="sina")
#' 
#' # without specify both source and market
#' sybs = getmd_symbol()
#' }
#' 
#' @export
#' 
getmd_symbol = function(market=NULL, source=NULL) {
    # mkt src
    ms = check_mkt_src(market=market, source=source)
    
    # get symbols
    rt = try(do.call(paste0("getmd_symbol_", ms$src), list(market = ms$mkt)), silent = TRUE)
    return(rt)
}