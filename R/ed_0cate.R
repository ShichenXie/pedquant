ecodat_cate = NULL
ecodat_cate[['gdp']] = setDT(list(
    symbol = c(),
    name = c()
))

# https://tradingeconomics.com/
# gdp
# labour
# prices
# money
# trade
# government
# business
# consumer
# housing
# taxes


# main economic data by category
# 
# export
ed_cate_current = function(area='top') {
    area = check_arg(area, c('world', 'europe', 'america', 'asia', 'africa', 'australia', 'top'))
    
    wb = read_html(sprintf('https://tradingeconomics.com/matrix?g=%s', area)) 
    tbl = setDT(html_table(wb, fill = TRUE)[[1]])
    setnames(tbl, gsub('[ \\./]+', '_', tolower(names(tbl))))
    setnames(tbl, '', 'area')
    
    return(tbl)
}
# BIZ
# GDP	GDP YoY	GDP QoQ	
# Interest rate	
# Inflation rate	
# Jobless rate	
# Gov. Budget	
# Debt/GDP	
# Current Account	
# Currency	
# Population




