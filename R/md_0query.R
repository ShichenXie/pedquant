
func_md_symbol = function(...) {
    .=symbol=name=main=NULL
    
    rbindlist(list(
        forex = func_forex_symbol(), # fred, oanda
        money = func_ibor_symbol(), # fred, shibor
        bond = func_bond_symbol(), # fred, chinabond
        # index = func_indices_symbol()[,.(symbol,name,main)], # yahoo,
        commodity = func_commodity_symbol() # fred
    ), idcol = 'category', fill = TRUE)
}
    
func_mkt_cate = function() {
    fread(
        'category, name
        forex, foregin exchange rate
        money, interbank offered rate
        bond, government bond yield
        stock, stock market prices
        future, future marekt prices'
    )
}

func_select_market = function(market, onerow) {
    mkt_cate = func_mkt_cate()
    if (!is.null(market)) {
        market = unlist(lapply(market, function(x) {
            check_arg(x, mkt_cate[['category']])
        }))
    }
            
    while (is.null(market) || length(market)==0) {
        market = select_rows_df(mkt_cate, column='category', onerow=onerow)[['category']]
    }
    
    return(market)
}
# query market data by category
# 
# \code{md_query} provides an interface to access main market data in five categories, including forex, money, bond, stock, future.
# 
# @param market the market category, forex, money, bond, stock, future. Default is NULL.
# @param ... ignored parameters
# 
# @examples 
# \dontrun{
# dat = md_query()
# }
# 
# @export
md_query = function(market=NULL, ...) {
    args = list(...)
    src = args[['source']]

    if (src == 'stooq' && !is.null(src)) {
        sybnam = setDT(copy(symbol_stooq))[toupper(args[['symbol']]), on='symbol']
        queryfunc = 'md_stooq'
    } else {
        mkt = func_select_market(market, onerow = TRUE)
        queryfunc = paste0('md_',market)
    }
    
    # data
    datlst = do.call(queryfunc, args = list(...))
    return(datlst)
}

#' symbol of market data
#' 
#' \code{md_symbol} returns all symbols by market category, including forex, money, bond, stock, future.
#' 
#' @param market the market category, including forex, money, bond, stock, future. Default is NULL.
#' @param ... ignored parameters
#' 
#' @examples 
#' \dontrun{
#' syblst = md_symbol()
#' }
#' 
#' @export
md_symbol = function(market=NULL, ...) {
    args = list(...)
    src = args[['source']]
    if (!is.null(args[['cate']])) market = args[['cate']]
    
    if (any(src == 'stooq')) {
        datsyb = setDT(copy(symbol_stooq))
        datlst = lapply(
            split(datsyb, by='geo'), 
            function(x) split(x, by='market')
        )
    } else {
        market = func_select_market(market, onerow = FALSE)
        
        mktlst = as.list(market) 
        names(mktlst) = mktlst
        
        datlst = lapply(
            mktlst, 
            function(m) do.call(sprintf('md_%s_symbol', m), args = list(...))
        )
    }
    
    return(datlst)
} 