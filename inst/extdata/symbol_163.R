# symbol 163
symbol_163_format = function(df_symbol) {
    type = . = id = name = symbol = tags = market = submarket = region = exchange = board = prov = indu = sec = mkt = syb  = syb3 = NULL
    
    # merge jsonDF with prov_indu_163
    prov_indu_163 = setDT(copy(prov_indu_163))
    
    if (all(c('province', 'industry', 'sector') %in% names(df_symbol))) {
        df_symbol = merge(df_symbol, prov_indu_163[type=="prov",.(province=id, prov=name)], by = "province", all.x = TRUE)
        df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(industry=id, indu=name)], by = "industry", all.x = TRUE)
        df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(sector=id, sec=name)], by = "sector", all.x = TRUE)
    }
    
    
    if (!('market' %in% names(df_symbol))) {
        df_symbol = copy(df_symbol)[, `:=`(market = ifelse(grepl("\\^", symbol), "index", "stock") )]
    }
    df_symbol = merge(
        # symbol list dataframe
        df_symbol[
            , syb := sub(".*?(\\d+).*","\\1", symbol) 
        ][nchar(syb)==6, syb3 := substr(syb,1,3)],
        # tangs by syb3 and market
        syb_cntags()[, c("exchange","submarket","board") := tstrsplit(tags,",")
        ][,.(market=mkt, syb3, exchange, submarket, board)],
        # merge by 
        all.x = TRUE, by = c('syb3', 'market')
    )[order(-market, exchange, symbol)
    ][, (c('syb','syb3')) := NULL]
    
    return(df_symbol)
}


#' @import data.table
#' @importFrom jsonlite fromJSON 
md_stock_symbol_163 = function() {
    # . = board = exchange = indu = market = name = prov = sec = submarket = symbol = NULL 
    df_syb = md_stockall_real_163(symbol = c('a', 'b', 'index'), only_symbol=FALSE, show_tags=TRUE)
    setnames(df_syb, c('sec', 'indu', 'prov'), c('sector', 'industry', 'province'))
    # [,.(market, submarket, exchange, board, symbol, name, sector = sec, industry = indu, province = prov)]
    return(df_syb)
}