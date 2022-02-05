
func_md_symbol = function() {
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
# query market data by category
# 
# \code{md_query} provides an interface to access main market data in five categories, including forex, money, bond, stock, future.
# 
# @param cate the market category, forex, money, bond, stock, future. Default is NULL.
# @param ... ignored parameters
# 
# @examples 
# \dontrun{
# dat = md_query()
# }
# 
# @export
md_query = function(cate=NULL, ...) {
    category = NULL
    
    # md_symbol = func_md_symbol()
    # market category
    mkt_cate = func_mkt_cate()
    if (!is.null(cate)) cate = check_arg(cate, mkt_cate$category)
    
    while (is.null(cate) || length(cate)==0) {
        cate = select_rows_df(mkt_cate, column='category', onerow=TRUE)[,category]
    }
    cat(sprintf('%s\n', cate))
    
    # data
    dat_lst = do.call(paste0('md_',cate), args = list(...))
    return(dat_lst)
}

#' symbol of market data by category
#' 
#' \code{md_stock_symbol} returns all symbols by market category, including forex, money, bond, stock, future.
#' 
#' @param cate the market category, including forex, money, bond, stock, future. Default is NULL.
#' @param ... ignored parameters
#' 
#' @examples 
#' \dontrun{
#' syblst = md_symbol()
#' }
#' 
#' @export
md_symbol = function(cate=NULL, ...) {
    # market category
    mkt_cate = func_mkt_cate()
    if (is.null(cate)) cate = select_rows_df(mkt_cate, column='category')
    
    catelist = as.list(cate$category) 
    names(catelist) = cate$category
    
    lapply(
        catelist, 
        function(c) {
            do.call(sprintf('md_%s_symbol', c), args = list(...))
        }
    )
} 