# interest rate ------
# loan prime rate, LPR
#' @import data.table 
md_lpr = function(from="2013-01-01", to=Sys.Date()) {
    lpr1y = lpr5y = value = NULL

    from = as.Date(from)
    to = as.Date(to)
    seqdates = unique(c(seq.Date(from, to, by = 'years'), to))
    
    lpr_lst = lapply(seq_along(seqdates[-1]), function(x) {
        url = sprintf("http://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/LprHisExcel?lang=CN&strStartDate=%s&strEndDate=%s", seqdates[x], seqdates[x+1]) 
        dt = setDT(load_read_xl(url))
        return(dt)
    })
    
    lpr_dat = rbindlist(lpr_lst, fill = TRUE)
    setnames(lpr_dat, c('date', 'lpr1y', 'lpr5y'))
    lpr_dat[, `:=`(
        date  = as.Date(date), 
        lpr1y = as.numeric(lpr1y),
        lpr5y = as.numeric(lpr5y)
    )][!is.na(date)]
    lpr_dat = lpr_dat[date >= from & date <= to]
    
    # melt datatable
    dat_melt = melt(lpr_dat, id.vars = "date", variable.name = "symbol", variable.factor = FALSE)[!is.na(value)]
    setkey(dat_melt, "symbol", "date")
    return(dat_melt)
}


# DBIR1YCN	LBIR1YCN
# benchmark/policy rate
#' @import data.table
#' @importFrom stats complete.cases
md_br = function(from="1998-01-01", to=Sys.Date()) {
    benchmark_deposit_rate_1y = benchmark_lending_rate_1y = NULL
    
    fromto = lapply(list(from=from, to=to), function(x) {
        y = format(check_fromto(x), "%d-%b-%Y")
        return(gsub("-", "%20", y))
    })
    
    # load data
    path = paste0("http://www.chinamoney.com.cn/dqs/rest/cm-u-bk-currency/SddsIntrRatePlRatHisExcel?startDate=",fromto$from,"&endDate=",fromto$to,"&lang=EN")
    dt_br = load_read_xl(path)
    
    # modify
    setDT(dt_br)
    setnames(dt_br, c("date", "benchmark_deposit_rate_1y", "benchmark_lending_rate_1y"))
    dt_br = dt_br[complete.cases(dt_br)][,`:=`(
        date = as.Date(date, format="%d %b %Y"),
        benchmark_deposit_rate_1y = as.numeric(benchmark_deposit_rate_1y),
        benchmark_lending_rate_1y = as.numeric(benchmark_lending_rate_1y)
    )]
    
    # melt datatable
    dt_melt = melt(dt_br, id.vars = "date", variable.name = "symbol", variable.factor = FALSE)
    setkey(dt_melt, "symbol", "date")
    
    return(dt_melt)
}


# refs
# http://www.pbc.gov.cn
# http://www.chinamoney.com.cn

# inflation rate ------