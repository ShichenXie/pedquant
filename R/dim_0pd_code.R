
#' code list by category
#' 
#' \code{pd_code} get the code list of country, currency, stock exchange and commodity exchange.
#' 
#' @param cate The available category values including 'country', 'currency', 'stock_exchange', 'commodity_exchange'.
#' 
#' @examples 
#' \donttest{
#' # specify the categories
#' code_list1 = pd_code(cate = c('country', 'currency'))
#' 
#' # interactivly return code list
#' code_list2 = pd_code()
#' 
#' }
#' 
#' @export
pd_code = function(cate=NULL) {
    code_category = c('country', 'currency', 'stock_exchange', 'commodity_exchange')
    
    # market category
    if (!is.null(cate)) cate = intersect(cate, code_category)
    # if (!is.null(cate)) cate = check_arg(cate, code_category)
    cate = select_rows_df(
        dt=setDT(list(code_category=code_category)), 
        column='code_category', input_string=cate
    )[,code_category]
    
    cod_lst = list()
    for (i in cate) {
        cod_lst[[i]] = setDT(copy(eval(parse(text = gsub(' ','_',paste("code", cate)) ))))
    }
    return(cod_lst)
}

