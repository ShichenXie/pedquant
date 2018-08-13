# get market data # getmd
# - froex
# - bond, money
# - stock, index
# - commodity

#' get market data
#' 
#' \code{getmd} provides an interface to get historical data for different markets, such as forex, bond, money, stock and commodity, which are provided public sources.
#' 
#' @param symbol symbols market data, which is available via getmd_symbol.
#' @param frequency default is daily. It supports daily, weekly and monthly for data from yahoo; daily for data from 163 and sina.
#' @param from the start date. Default is '2010-01-01'.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' @param source data source. The available sources including 'yahoo', '163' and 'sina'.
#' @param fillzero logical. Default is FALSE If it is TRUE, the zeros in dataset will be filled with last non-zero values.
#' 
#' @examples 
#' \dontrun{
#' # Example I
#' # get Chinese stock data from 163
#' dat1 = getmd(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163")
#' 
#' dat2 = getmd(symbol=c('600000', '000001', '^000001', '^399001'), 
#'              source="163", frequency="spot")
#' 
#' # get spot price of all A shares in sse and szse
#' dat3 = getmd(symbol='a', source="163", frequency="spot")
#' 
#' # get spot price of all index in sse and szse
#' dat4 = getmd(symbol='index', source="163", frequency="spot")
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
getmd = function(symbol, frequency="daily", from = "2010-01-01", to = Sys.time(), print_step = 1L, source="yahoo", fillzero=FALSE) {
    cat(source,"\n")
    
    args = list(symbol=symbol, frequency=frequency, from=from, to=to, print_step=print_step)
    if (source == "163") args['fillzero'] = fillzero
    
    do.call(eval(parse(text = paste0("getmd_", source))), args)
    
}

#' get symbols of market data
#' 
#' \code{getmd_symbol} provides an interface to get symbols of different markets. yahoo (main currencies, commodities and world-indices), 163 (stock shares in sse and szse) and sina (commodity futures in dce, shfe, sge and zce). 
#' 
#' @param source data source. The available data sources including 'yahoo', '163' and 'sina'.
#' 
#' @examples 
#' \dontrun{
#' syb_yahoo = getmd_symbol(source="yahoo")
#' 
#' syb_commodity_cn = getmd_symbol(source="sina")
#' 
#' syb_stock_cn = getmd_symbol(source="163")
#' }
#' 
#' @export
#' 
getmd_symbol = function(source="yahoo") {

    do.call(eval(parse(text = paste0("getmd_symbol_", source))), list())

}