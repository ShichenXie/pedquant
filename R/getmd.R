
# md_src_list = data.table(
#     market = c("stocks", "commodities", "stocks", "indices", "currencies", "commodities"),
#     source = c("163", "sina", rep("yahoo",4))
# )

#' get market data
#' 
#' query market data from public sources. 
#' 
#' 
getmd = function(symbol, market=NULL, exchange=NULL, source="yahoo", frequency="daily", from = "1900-01-01", to = Sys.time(), print_step = 1L) {
    
    do.call(
        eval(parse(text = paste0("getmd_", source))), 
        list(symbol=symbol, frequency=frequency, from=from, to=to, print_step=print_step)
    )
    
}


# getmd_symbol = function(source, market=NULL, exchange=NULL, source=NULL) {
#     eval(parse(text = paste0(c("getmd_symbol", source), collapse = "_")))
#     
# }