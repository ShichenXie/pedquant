# d, w, m, q, ytd, y, 

pq1_perf = function(dt, date_range='max', from=NULL, to=Sys.Date(), x='close|value', base_value=0) {
    x = intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    if (x %in% c('open','high','low','close')) {
        cols = intersect(names(dt), c('open','high','low','close'))
    } else cols = x
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = dt[1,date], default_date_range = 'max')
    from = ft$f
    to = ft$t
    
    # set range for data
    dt = dt[date>=from & date<=to]
    fst_xvalue = dt[1,][[x]]
    if (base_value == 0) {
        dat = dt[, (cols) := lapply(.SD, function(c) c/fst_xvalue-1), .SDcols = cols][]
    } else dat = dt[, (cols) := lapply(.SD, function(c) c/fst_xvalue * base_value), .SDcols = cols][]
    
    
    cols2 = intersect(names(dt), c('symbol', 'name', 'date', cols))
    return(dat[, cols2, with = FALSE])
}

#' creating performance trends
#' 
#' \code{pq_perf} provides an easy way to create the performance trends for a set of time series data.
#' 
#' @param dt a list/dataframe of time series dataset
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param x the name of column to calculate. Default is 'close|value'.
#' @param base_value the base value of performance index. Default is 0.
#' 
#' @examples  
#' \donttest{
#' # load data
#' dat = md_stock(c('000001', '^000001'), date_range = 'max', source = '163')
#' 
#' # create performance trends
#' perf = pq_perf(dat)
#' # pq_plot(perf)
#' 
#' }
#' 
#' @export
pq_perf = function(dt, date_range='max', from=NULL, to=Sys.Date(), x='close|value', base_value=0) {
    symbol = NULL
    
    # bind list of dataframe
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    
    # plot symbol
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (s in sybs) {
        dt_s = dt[symbol == s]
        setkeyv(dt_s, 'date')
        
        dt_list[[s]] = do.call(pq1_perf, args = list(dt=dt_s, x=x, date_range=date_range, from=from, to=to))
    }
    
    return(dt_list)
}
