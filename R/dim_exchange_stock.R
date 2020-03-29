# stock exchanges list
#
# List of major stock exchanges by January 2015 (top 20 by market capitalization)
#
# @docType data
# @keywords data
# @name exchange_stock
# @usage data(exchange_stock)
# @format A data frame with 65 rows and 13 columns.
#
# @source \url{https://en.wikipedia.org/wiki/List_of_stock_exchange_trading_hours}
# 
# @examples
# # load stock exchange
# data(exchange_stock)
# 
# NULL


#' @import data.table 
dim_exchange_stock = function() {
  # list of stock exchange
  # https://www.stockmarketclock.com/exchanges
  # https://en.wikipedia.org/wiki/List_of_stock_exchange_trading_hours
  wb = read_html("https://en.wikipedia.org/wiki/List_of_stock_exchange_trading_hours") 
  
  # scrap datatable
  # se = xml_table(wb, 2, sup_rm="\\[.+?\\]")[[1]][-c(1,2)]
  se = html_table(wb, fill = TRUE)[[1]][-c(1,2)]
  setnames(se, c("name", "id", "country", "city", "time_zone", "delta", "DST", "open_local", "close_local", "lunch_local", "open_UTC", "close_UTC", "lunch_UTC"))
  
  # # market cap of stock exchanges
  # se2 = read_html("https://www.stockmarketclock.com/exchanges") %>% 
  #   html_table(header=TRUE, fill=TRUE) %>% .[[1]] %>% .[,1:3]
  # names(se2) = c("NAME", "id", "market cap")
  
  return(setDF(se))
}

#' @import data.table 
dim_exchange_stock20 = function() {
  V1 = . = city = economy = read_html = market_cap_bnUSD = monthly_trade_volume_bnUSD = NULL
  
  # https://en.wikipedia.org/wiki/List_of_stock_exchanges
  # http://money.visualcapitalist.com/all-of-the-worlds-stock-exchanges-by-size/
  wb = read_html('https://en.wikipedia.org/wiki/List_of_stock_exchanges')
  
  # se = xml_table(wb, 4, sup_rm="\\[.+?\\]")[[1]][V1 != "Rank"]
  se = html_table(wb, fill = TRUE)[[1]][V1 != "Rank"]
  setnames(se, c("rank", "exchange", "economy", "city", "market_cap_bnUSD", "monthly_trade_volume_bnUSD", "time_zone", "delta_to_UTC", "DST", "opentime_local", "closetime_local", "lunchtime_local", "opentime_UTC", "closetime_UTC"))
  
  se = setDT(se)[,`:=`(
    rank = sub("\\[.+\\]","",rank),
    market_cap_bnUSD = as.numeric(sub(",","",market_cap_bnUSD)),
    monthly_trade_volume_bnUSD = as.numeric(sub(",","",monthly_trade_volume_bnUSD)), 
    city = ifelse(city=="AmsterdamBrusselsLisbonLondonParis","Amsterdam, Brussels, Lisbon, London, Paris", city),
    economy = ifelse(economy=="United Kingdom, Italy","United Kingdom, Italy", economy)
  )]
  
  return(setDF(se))
}
# exchange_stock = dim_exchange_stock()
# save(exchange_stock, file="./data/exchange_stock.RData")


