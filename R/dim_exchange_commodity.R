#' commodity exchanges list
#'
#' List of commodities exchanges
#'
#' @docType data
#' @keywords data
#' @name exchange_commodity
#' @usage data(exchange_commodity)
#' @format A data frame with 97 rows and 7 columns.
#' @source \url{https://en.wikipedia.org/wiki/List_of_commodities_exchanges}
#' @examples
#' # load commodity exchange
#' data(exchange_commodity)
#' 
NULL


#' @import data.table rvest 
dim_exchange_commodity = function() {
    read_html = exchange = location = abbreviation = product_types = life_time = economy = city = region = . = NULL
    
    # https://en.wikipedia.org/wiki/List_of_commodities_exchanges
    # https://en.wikipedia.org/wiki/List_of_futures_exchanges
    
    ce = read_html("https://en.wikipedia.org/wiki/List_of_commodities_exchanges") %>% html_table(header=TRUE, fill=TRUE)
    ce_africa = ce[[2]]
    ce_americas = ce[[3]]
    ce_asia = ce[[4]]
    ce_europe = ce[[5]]
    ce_oceania = ce[[6]]
    
    ce_dt = rbindlist(list(africa=ce_africa, americas=ce_americas, asia=ce_asia, europe=ce_europe, oceania=ce_oceania), fill = TRUE, idcol = "region")
    
    setnames(ce_dt, c("region", "exchange", "abbreviation", "location", "product_types", "life_time"))
    
    ce_dt = ce_dt[,`:=`(
        exchange = gsub("\\[.+\\]","",exchange),
        location = ifelse(location %in% c("Manila Philippines", "Almaty Kazakhstan", "Bangkok Thailand"), sub(" ",", ",location), location)
    )][,c("city", "economy") := tstrsplit(location, ",", fixed=TRUE)
       ][,economy := ifelse(is.na(economy), city, economy)
         ][order(region, economy, city, exchange)
           ][,.(region, exchange, abbreviation, economy, city, product_types, life_time)]
    
    ce_dt = setDF(ce_dt)
    return(ce_dt)
}
# exchange_commodity = dim_exchange_commodity()
# save(exchange_commodity, file="./data/exchange_commodity.RData")
