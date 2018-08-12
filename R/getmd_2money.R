# shanghai interbank offered rate, shibor
#' @import data.table xml2
getmd_shibor = function(from="2006-01-01", to=Sys.Date()) {
    V1 = NULL
    fromto = lapply(list(from=from, to=to), check_fromto)
    
    # shibor in recent 10 days
    if ( as.integer(Sys.Date() - check_fromto(from)) <= 10 ) {
        wb = read_html("http://www.shibor.org/shibor/ShiborTendaysShow_e.do")
        dt = setDT(xml_table(wb, 3)[[1]])[, V1 := as.Date(V1)]
        setnames(setDT(dt), c("date", "shiboron", "shibor1w", "shibor2w", "shibor1m", "shibor3m", "shibor6m", "shibor9m", "shibor1y"))
        
    } else {
        # shibor in history
        fromto_y = lapply(fromto, function(x) {
            # year of from/to
            y = as.integer(substr(x,1,4))
            # current year
            cur_year = as.integer(substr(Sys.Date(),1,4))
            # check year
            y = ifelse(y < 2006, 2006, ifelse(y > cur_year, cur_year, y))
            return(y)
        })
        years = eval(parse(text = paste0(fromto_y$from, ":", fromto_y$to)))
        
        dt_shibor = lapply(years, function(y) {
            path = paste0("http://www.shibor.org/shibor/web/html/downLoad.html?nameNew=Historical_Shibor_Data_",y,".xls&nameOld=Shibor%CA%FD%BE%DD",y,".xls&shiborSrc=http%3A%2F%2Fwww.shibor.org%2Fshibor%2F&downLoadPath=data")
            dt = load_read_xl(path)
            
            setnames(setDT(dt), c("date", "shiboron", "shibor1w", "shibor2w", "shibor1m", "shibor3m", "shibor6m", "shibor9m", "shibor1y"))
            dt[, date := as.Date(date)]
        })
        dt = rbindlist(dt_shibor, fill = TRUE)
        
    }
    dt = dt[date>=fromto$from & date<=fromto$to]
    setkey(dt, "date")
    return(dt)
}


# loan prime rate, LPR
#' @import data.table xml2
getmd_lpr = function(from="2013-01-01", to=Sys.Date()) {
    V1 = NULL
    fromto = lapply(list(from=from, to=to), check_fromto)
    
    # lpr in recent 10 days
    if ( as.integer(Sys.Date() - check_fromto(from)) <= 10 ) {
        wb = read_html("http://www.shibor.org/shibor/LPRTendaysShow_e.do")
        dt = setDT(
            xml_table(wb)[[1]][!(V1 %in% c("","Date")), 1:2]
        )[, V1 := as.Date(V1)]
        setnames(setDT(dt), c("date", "lpr1y"))
        
    } else {
        # lpr in history
        fromto_y = lapply(fromto, function(x) {
            # year of from/to
            y = as.integer(substr(x,1,4))
            # current year
            cur_year = as.integer(substr(Sys.Date(),1,4))
            # check year
            y = ifelse(y < 2013, 2013, ifelse(y > cur_year, cur_year, y))
            return(y)
        })
        years = eval(parse(text = paste0(fromto_y$from, ":", fromto_y$to)))
        
        dt_lpr = lapply(years, function(y) {
            path = paste0("http://www.shibor.org/shibor/web/html/downLoad.html?nameNew=Historical_LPR_Data_", y, ".xls&nameOld=LPR%CA%FD%BE%DD", y, ".xls&shiborSrc=http%3A%2F%2Fwww.shibor.org%2Fshibor%2F&downLoadPath=data")
            dt = load_read_xl(path)
            
            setnames(setDT(dt), c("date", "lpr1y"))
            dt[, date := as.Date(date)]
        })
        dt = rbindlist(dt_lpr, fill = TRUE)

    }
    dt = dt[date>=fromto$from & date<=fromto$to]
    setkey(dt, "date")
    return(dt)
}

# DBIR1YCN	LBIR1YCN
# benchmark/policy rate
#' @import data.table
#' @importFrom stats complete.cases
getmd_br = function(from="1998-01-01", to=Sys.Date()) {
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
    
    return(dt_br)
}


# refs
# http://www.pbc.gov.cn
# http://www.shibor.org/
# http://www.chinamoney.com.cn