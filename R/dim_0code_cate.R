
#' code list by category
#' 
#' 
#' 
#' @export
code_cate = function(cate=NULL, ...) {
    code_category = c('country', 'currency', 'stock exchange', 'commodity exchange')
    # market category
    if (!is.null(cate)) cate = check_arg(cate, code_category)
    while (is.null(cate)) {
        cate = select_rows_df(dt=setDT(list(code_category=code_category)), column='code_category', onerow=TRUE
                            )[,code_category]
    }
    
    cod = setDT(copy(eval(parse(text = gsub(' ','_',paste("code", cate)) ))))
    return(cod)
}

