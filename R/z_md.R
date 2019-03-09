# market data # md
# - froex
# - bond, money
# - stock, index
# - commodity

# stock
# - symbol, name, exchange, sector, industry, first date point, 


# query market data
# 
# \code{md} provides an interface to query historical data for different markets, such as forex, bond, money, stock and commodity, which are provided public sources.
# 
# @param symbol symbols market data, which is available via md_symbol.
# @param source data sources. The available sources including 'yahoo', '163', 'sina' and 'original'.
# @param freq default is daily. It supports daily, weekly and monthly for data from yahoo; daily for data from 163 and sina.
# @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is max.
# @param from the start date. Default is '2010-01-01'.
# @param to the end date. Default is current system date.
# @param adjust logical. Argument for data download from yahoo. Default is FALSE. If it is TRUE, the open, high, low and close values will adjusted based on close_adj. 
# @param fillzero logical. Argument for data download from 163. Default is TRUE If it is TRUE, the zeros in dataset will be filled with last non-zero values.
# @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1.
# 
# @examples 
# \dontrun{
# # Example I
# # query Chinese stock data from 163
# dat1 = md(symbol=c('600000', '000001', '^000001', '^399001'), 
#              source="163")
# 
# dat2 = md(symbol=c('600000', '000001', '^000001', '^399001'), 
#              source="163", freq="spot")
# 
# # query spot price of all A shares in sse and szse
# dat3 = md(symbol='a', source="163", freq="spot")
# 
# # query spot price of all index in sse and szse
# dat4 = md(symbol='index', source="163", freq="spot")
# 
# 
# # Example II
# # query Chinese commodity future price
# dat_com1 = md(symbol='RB0', source="sina")
# 
# dat_com2 = md(symbol=c("AG0","AU0"), source="sina")
# 
# 
#  # Example III
#  # query data from yahoo
#  dat_yahoo = md(symbol=c("^GSPC", "000001.SS", "EURUSD=X"))
#  
#  # for Chinese share
#  dat_yahoo2 = md(c("000001", "^000001"))
# }
# 
# @export
# 
# md = function(symbol, source = "yahoo", freq = "daily", date_range = "3y", from = NULL, to = Sys.Date(), adjust = FALSE, fillzero = TRUE, print_step = 1L) {
#     cat(source,"\n")
#     
#     # from
#     date_range = check_date_range(date_range, default = "max")
#     from = get_from_daterange(date_range, to, min_date = "1000-01-01")
#     
#     # arguments
#     args = list(symbol = symbol, freq = freq, from = from, to = to, print_step = print_step, env = parent.frame(), fillzero = fillzero, adjust = adjust)
#     
#     # return data
#     rt = try(do.call(paste0("md_", source), args), silent = TRUE)
#     return(rt)
# }

# query symbols of market data
# 
# \code{md_symbol} provides an interface to query symbols of different markets, which can query data via \code{md}.
# 
# @param market there are six markets, 'currency', 'bond', 'money', 'index', 'stock' and 'commodity'.
# @param source data sources. The available data sources including 'yahoo', '163', 'sina' and 'original'. 
# 
# @examples 
# \dontrun{
# # specify source
# syb_163 = md_symbol(source="163")
# 
# # specify market
# syb_stock = md_symbol(market="stock")
# 
# # specify both source and market
# syb_commodity_sina = md_symbol(market="commodity", source="sina")
# 
# # without specify both source and market
# sybs = md_symbol()
# }
# 
# @export
# 
# md_symbol = function(market=NULL, source=NULL) {
#     # mkt src
#     ms = check_mkt_src(market=market, source=source)
#     
#     # query symbols
#     rt = try(do.call(paste0("md_symbol_", ms$src), list(market = ms$mkt)), silent = TRUE)
#     return(rt)
# }