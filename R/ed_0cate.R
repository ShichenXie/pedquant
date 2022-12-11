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
ed_cate_current = function(area='top') {
    area = check_arg(area, c('world', 'europe', 'america', 'asia', 'africa', 'australia', 'top'))
    
    wb = read_html(sprintf('https://tradingeconomics.com/matrix?g=%s', area)) 
    tbl = setDT(html_table(wb, fill = TRUE)[[1]])
    setnames(tbl, gsub('[ \\./]+', '_', tolower(names(tbl))))
    
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



ed_chn = function() {
    symbol = value = NULL
    
    ed_m12 = function() {
        m12 = ed_nbs('A0D01', freq = 'm', geo_type = 'n', date_range = 'max')
        
        pq_plot(rbindlist(m12[c('A0D0102', 'A0D0104')])[!is.na(value)], y='value', arrange = list(rows=1, cols=1), title = 'M1,M2')
    }
    
    ed_gdp = function() {
        gdp = ed_nbs(symbol = c('A010101', 'A010102', 'A010301', 'A010302'), freq = 'q', geo_type = 'n', date_range = 'max')
        pq_plot(rbindlist(gdp[c('A010301', 'A010302')])[!is.na(value)][,value := value - 100], y='value', arrange = list(rows=1, cols=1), title = 'GDP')
    }
    
    ed_cpi = function() {
        cpi = ed_nbs(c('A01010101', 'A01010201', 'A01070101'), freq = 'm', geo_type = 'n', date_range = 'max')
        
        pq_plot(rbindlist(cpi)[!is.na(value) & date >= '1996-01-01'][symbol %in% c('A01010101', 'A01010201'), symbol := 'A0101'][, value := value - 100], y = 'value', arrange = list(rows=1, cols=1), title = 'CPI,PPI')
    }
    
    ed_pmi = function() {
        pmi = ed_nbs(c('A0B0101', 'A0B0201'), freq = 'm', geo_type = 'n', date_range = 'max')
        
        pq_plot(rbindlist(pmi)[!is.na(value)], y = 'value', arrange = list(rows=1, cols=1), title = 'PMI')
    }
    
    ed_ele = function() {
        ele = ed_nbs(c('A03010G03', 'A03010G04'), freq = 'm', geo_type = 'n', date_range = 'max')
        
        pq_plot(rbindlist(ele)[!is.na(value) & date >= '1996-01-01'], y = 'value', arrange = list(rows=1, cols=1), title = 'ELE')
    }
    
    ed_gov = function() {
        gov = ed_nbs(c('A0C0103', 'A0C0203'), freq = 'm', geo_type = 'n', date_range = 'max')
        
        pq_plot(rbindlist(gov)[!is.na(value) ], y = 'value', arrange = list(rows=1, cols=1), title = 'GOV')
        
    }
}




ed_biz = function() {
    
}

# https://www.conference-board.org/topics/business-cycle-indicators
# https://www.conference-board.org/topics/us-leading-indicators

# https://www.ifo.de/en/ifo-time-series
# https://www.ifo.de/sites/default/files/secure/timeseries/gsk-e-202211.xlsx

# refs
# http://www.pbc.gov.cn

# inflation rate ------

