# "https://emweb.securities.eastmoney.com/CompanySurvey/Index?type=web&code=SH603777"

md_stock1_info = function(symbol1) {
    city = ipodate = NULL 
    
    # symbol1 = '000001'
    sybtag = try(syb_add_cntags(symbol1), silent = TRUE)
    syb = sybtag[, ifelse(city == 'sh', sprintf('1.%s', syb), sprintf('0.%s', syb))]
    
    url = 'https://emweb.securities.eastmoney.com/CompanySurvey/Index?type=web&code=SH603777'
    web = read_html(url)
    tbls = html_table(web)
    
    url = sprintf(
        "http://push2.eastmoney.com/api/qt/stock/get?ut=%s&fltt=%s&invt=%s&fields=%s&secid=%s&_=%s", 
        'fa5fd1943c7b386f172d6893dbfba10b', 
        2, 2, 
        'f57,f58,f84,f85,f116,f117,f127,f128,f189',
        # paste0('f', 1:300, collapse = ','), 
        syb, 
        date_num(Sys.time(), 'ms')
    )
    
    dtmp = read_api_eastmoney(url)
    
    dtmp2 = setDT(dtmp$data)[]
    setnames(dtmp2, c('symbol', 'name', 'shares_total', 'shares_market', 'cap_total', 'cap_market', 'sector', 'prov', 'ipodate' ))
    dtmp2[, `:=`(
        ipodate = as_date(as.character(ipodate)), 
        symbol = sybtag$syb_exp
    )][]
    
    return(dtmp2)
}