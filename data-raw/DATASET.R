## code to prepare `DATASET` dataset goes here
library(pedquant)
library(data.table)
ssec = md_stock('^000001', date_range = 'max', source = '163')
ssec = setDF(ssec$`^000001`)
usethis::use_data(ssec, overwrite = TRUE)

## Internal data # http://r-pkgs.had.co.nz/data.html
if (FALSE) {
    
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
    
    # usethis::use_data(financial_statements_163, prov_indu_163, symbol_future_sina, symbol_stock_163, code_exchange_commodity, code_exchange_stock, code_country, code_currency, code_china_district, urls_pbc, internal = TRUE, overwrite = TRUE)
    
}


# database urls 
if (FALSE) {
    library(rvest)
    'http://www.cciee.org.cn/zkdh.aspx?clmId=69' %>% 
        read_html() %>% 
        html_attr('')
        html_table() %>% 
        .[[1]]
        
        
}


