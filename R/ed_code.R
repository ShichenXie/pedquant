
#' code list by category
#' 
#' \code{ed_code} get the code list of country, currency, stock exchange, commodity exchange and administrative district of mainland of China.
#' 
#' @param cate The available category values including 'country', 'currency', 'stock_exchange', 'commodity_exchange', 'china_district'.
#' 
#' @examples 
#' \dontrun{
#' # specify the categories
#' code_list1 = ed_code(cate = c('country', 'currency'))
#' 
#' # interactivly return code list
#' code_list2 = ed_code()
#' 
#' }
#' 
#' @export
ed_code = function(cate=NULL) {
    category = c('country', 'currency', 'exchange_stock', 'exchange_commodity', 'china_district')
    
    # consistent with the version below 0.1.3.999
    if (!is.null(cate)) cate = sapply(cate, function(c) {
        if (c %in% c('stock_exchange', 'commodity_exchange')) {
            c2 = unlist(strsplit(c, '_'))
            c = paste(c2[2], c2[1], sep = '_')
        }
        return(c)
    } )
    
    # market category
    if (!is.null(cate)) cate = intersect(cate, category)
    # if (!is.null(cate)) cate = check_arg(cate, code_category)
    cate = select_rows_df(
        dt=setDT(list(category=category)), 
        column='category', input_string=cate
    )[,category]
    
    
    cod_lst = list()
    for (i in cate) {
        cod_lst[[i]] = setDT(copy(
            eval(parse(text =  sprintf('code_%s', cate)))
        )) # gsub(' ','_',paste("code", cate))
    }
    return(cod_lst)
}


