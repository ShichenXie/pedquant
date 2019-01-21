mkt_cate = setDT(list(
    category = c('forex', 'money', 'bond', 'index', 'commodity'),
    name = c('foregin exchange rate', 'Interbank offered rate', 'government bond yield', 'stock market index', 'commodity price')
))
func_md_symbol = function() {
    rbindlist(list(
        forex = func_forex_symbol(), # fred, oanda
        money = func_ibor_symbol(), # fred, shibor
        bond = func_bond_symbol(), # fred, chinabond
        index = func_indices_symbol()[,.(symbol,name,main)], # yahoo,
        commodity = func_commodity_symbol() # fred
    ), idcol = 'category', fill = TRUE)
}
    
#' main market data by category
#' 
#' 
#' 
#' @export
md_cate = function(cate=NULL, symbol=NULL, date_range = "3y", from = NULL, to = Sys.Date(), print_step = 1L, ...) {
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


# md_cate_symbol = function(cate=NULL) {
#     if (is.null(cate)) cate = select_rows_df(mkt_cate, column='market_category', onerow=TRUE)[,market_category]
#     syb = select_rows_df(md_symbol[market_category==cate,.(symbol,name)], column='symbol')[,symbol]
#     return(syb)
# }
