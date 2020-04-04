
#' code list by category
#' 
#' \code{ed_code} get the code list of country, currency, stock exchange, commodity exchange and administrative district of mainland of China.
#' 
#' @param cate The available category values including 'country', 'currency', 'stock_exchange', 'commodity_exchange', 'china_district'.
#' 
#' @examples 
#' \donttest{
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


# Internal data # http://r-pkgs.had.co.nz/data.html
update_sysdata = function() {
    . = industry = market = name = province = sector = symbol = NULL
    
    # # china district
    # library(RSelenium); library(rvest); library(httr); library(data.table)
    # code_china_district = admin_div_cn(admin_level = 3)
    # code_china_district = admin_mca()
    
    # # country currency
    # cc = dim_country_currency()
    # code_country = cc$country
    # code_currency = cc$currency
    
    # # financial_statements_163
    # # prov_indu_163
    
    # # symbol_stock_163
    # # update on 20200403
    # syb_stock1 = pedquant:::md_stock_spotall_163(symbol = c('a', 'b', 'index', 'fund'), to_sysdata=TRUE)
    # syb_stock2 = syb_stock1[
    #     ,.(market, symbol = gsub('[^0-9]', '', symbol), name, province, sector, industry)
    # ][market == 'index', symbol := paste0('^',symbol)][, market := NULL]
    # # syb_stock3 = pedquant:::md_fund_spotall_163()[market == 'index', symbol := paste0('^',symbol)][,.(symbol, name)]
    # symbol_stock_163 = setDF(
    #     setDT(
    #         rbind(syb_stock2, symbol_stock_163, fill=TRUE)
    #     )[,.SD[1], keyby=symbol
    #     ][grepl('^c.+', province), province := NA] )
        
    # # symbol_future_sina
    
    # # exchange
    # code_exchange_commodity = pedquant:::dim_exchange_commodity()
    # code_exchange_stock = pedquant:::dim_exchange_stock()
    
    # usethis::use_data(financial_statements_163, prov_indu_163, symbol_future_sina, symbol_stock_163, code_exchange_commodity, code_exchange_stock, code_country, code_currency, code_china_district, internal = TRUE, overwrite = TRUE)
}





