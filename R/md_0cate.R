mkt_cate = setDT(list(
    category = c('forex', 'money', 'bond', 'index', 'commodity'),
    name = c('foregin exchange rate', 'Interbank offered rate', 'government bond yield', 'stock market index', 'commodity price')
))
func_md_symbol = function() {
    .=symbol=name=main=NULL
    
    rbindlist(list(
        forex = func_forex_symbol(), # fred, oanda
        money = func_ibor_symbol(), # fred, shibor
        bond = func_bond_symbol(), # fred, chinabond
        index = func_indices_symbol()[,.(symbol,name,main)], # yahoo,
        commodity = func_commodity_symbol() # fred
    ), idcol = 'category', fill = TRUE)
}
    

#' query main market data by category
#' 
#' \code{md_cate} provides an interface to access main market data in five categories, including forex, money, bond, index, commodity.
#' 
#' @param cate the market category, forex, money, bond, index, commodity. Default is NULL.
#' @param symbol symbols of main market indicators.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is '3y'.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L. 
#' @param ... ignored parameters
#' 
#' @examples 
#' \dontrun{
#' dat = md_cate()
#' }
#' 
#' @export
md_cate = function(cate=NULL, symbol=NULL, date_range = "3y", from = NULL, to = Sys.Date(), print_step = 1L, ...) {
    category = NULL
    # md_symbol = func_md_symbol()
    # market category
    if (!is.null(cate)) cate = check_arg(cate, mkt_cate$category)
    while (is.null(cate) || length(cate)==0) {
        cate = select_rows_df(mkt_cate, column='category', onerow=TRUE)[,category]
    }
    cat(sprintf('%s\n', cate))
    
    env = list(...)[['env']]
    if (is.null(env)) env = parent.frame()
    
    # data
    dat_lst = do.call(paste0('md_',cate), args = list(symbol=symbol, date_range=date_range, from=from, to=to, print_step=print_step, env=env, ...))
    return(dat_lst)
}

