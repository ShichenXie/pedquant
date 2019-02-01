# d, w, m, q, ytd, y, 

pd1_perf = function(dt, date_range="max", from=NULL, to=Sys.Date(), y="open|close|value") {
    y = names(dt)[grepl(y, names(dt))]
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = dt[, tolower(class(date))], shift=1) 
    if (is.null(from)) from = get_from_daterange(date_range, to, min_date = dt[,date[1]])
    
    # set range for data
    dat = dt[date>=from & date<=to
           ][, (y) := lapply(.SD, function(x) fill0(x)/x[1]-1), .SDcols = y]
    
    cols = y
    for (i in c('name', 'symbol', 'date')) {
        if (i %in% names(dat)) cols = c(i, cols)
    }
    
    return(dat[, cols, with = FALSE])
}

# create performance of data sets
# 
# 
pd_perf = function(dt, date_range="max", from=NULL, to=Sys.Date(), y="open|close|value") {
    symbol = NULL
    
    # bind list of dataframe
    if (is.list(dt) & !is.data.frame(dt)) {
        dt = rbindlist(dt, fill = TRUE)
    }
    setDT(dt)
    # check date_range
    date_range = check_date_range(date_range, default = "max")
    
    # plot symbol
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (s in sybs) {
        dt_s = dt[symbol == s]
        setkeyv(dt_s, "date")
        
        dt_list[[s]] = do.call(pd1_perf, args = list(dt=dt_s, y=y, date_range=date_range, from=from, to=to))
    }
    
    return(dt_list)
}