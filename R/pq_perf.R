# d, w, m, q, ytd, y, 

pq1_perf = function(dt, date_range="max", from=NULL, to=Sys.Date(), x="close|value") {
    x = intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    if (x %in% c('open','high','low','close')) {
        cols = intersect(names(dt), c('open','high','low','close'))
    } else cols = x
    # from to 
    date_range = check_date_range(date_range, default = "max")
    from = get_from_daterange(date_range, from, to, min_date = dt[1,date])
    
    # set range for data
    dt = dt[date>=from & date<=to]
    fst_xvalue = dt[1,][[x]]
    dat = dt[, (cols) := lapply(.SD, function(c) c/fst_xvalue-1), .SDcols = cols][]
    
    
    cols2 = intersect(names(dt), c('symbol', 'name', 'date', cols))
    return(dat[, cols2, with = FALSE])
}

#' create performance of data sets
#' 
#' @export
pq_perf = function(dt, date_range="max", from=NULL, to=Sys.Date(), x="close|value") {
    symbol = NULL
    
    # bind list of dataframe
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    
    # plot symbol
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (s in sybs) {
        dt_s = dt[symbol == s]
        setkeyv(dt_s, "date")
        
        dt_list[[s]] = do.call(pq1_perf, args = list(dt=dt_s, x=x, date_range=date_range, from=from, to=to))
    }
    
    return(dt_list)
}
