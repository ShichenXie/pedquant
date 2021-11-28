return_arithmetic = function(x, x_lag) x/x_lag-1
return_log = function(x, x_lag) log(x/x_lag)

# rename the column name of asset returns
ra_rename = function(dt, freq, rcol_rename=NULL) {
    if (is.null(rcol_rename)) rcol_rename = paste0('return_', freq)
    setnames(dt, 'Ra', rcol_rename)
    
    return(dt)
}

pq1_return = function(dt, x, method='arithmetic', freq='daily', rcol_rename = NULL, cols_keep) {
    . = byfreq = func = byyear = wkdiff = wkN = x_lag = Ra = NULL
    
    setkeyv(dt, "date")
    
    freq_list = c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
    if (freq == 'all') freq = freq_list
    freq = intersect(freq_list, freq)
    
    freqfunc = data.table(
        freq = freq_list, 
        func = c('yday', 'isoweek', 'month', 'quarter', 'year'), 
        key = 'freq'
    )
        
    dt_rt = lapply(freq, function(freqi) {
        dt2 = copy(dt)[, byfreq := do.call(freqfunc[freqi,func], args = list(x=date))
        ][, byyear := year(date)]
        
        if (freqi == 'weekly') {
            dt2 = dt2[byfreq %in% c(1,52,53), 
                      wkdiff := byfreq - shift(byfreq, 1, type="lag") 
            ][byfreq %in% c(1,52,53) & (wkdiff!=0|is.na(wkdiff)), wkdiff := 1
            ][byfreq %in% c(1,52,53), wkN := cumsum(wkdiff)
            ][byfreq %in% c(1,52,53) & wkN>0, byyear := max(byyear), by=wkN
            ][, (c('wkdiff', 'wkN')) := NULL]
        } 
        
        # BOP, bebinning of period
        bop = dt2[, .SD[1], by = .(byyear, byfreq)]
        # EOP, end of period
        eop = dt2[, .SD[.N], by = .(byyear, byfreq)
        ][, x_lag := shift(get(x),n=1,type = 'lag') 
        ][1, x_lag := bop[1,get(x)]
        ][, Ra := do.call(
            sprintf('return_%s', method), 
            args = list(x=get(x), x_lag)
        )][, unique(c(cols_keep, 'Ra')), with = FALSE]
        
        eop = ra_rename(eop, freqi, rcol_rename)
        return(eop)
    })

    dt_rt2 = Reduce(function(x,y) merge(x,y, all.x=TRUE, by=cols_keep), dt_rt)
    return(dt_rt2)
}


#' calculating returns by frequency
#' 
#' \code{pq_return} calculates returns for daily series based on specified column, frequency and method type.
#' 
#' @param dt a list/dataframe of daily series dataset
#' @param x the column name of adjusted asset price. 
#' @param freq the frequency of returns. It supports c('all', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly').
#' @param method the method to calculate returns.
#' @param rcol_rename setting the column name of returns, defaults to return_freq. 
#' @param cols_keep the columns keep in the return data. The columns of symbol, name and date will always kept if they are exist in the input data.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' 
#' @examples 
#' \donttest{
#' data(ssec)
#' 
#' # create a close_adj column
#' datadj = md_stock_adjust(ssec, adjust = FALSE)
#' 
#' # set freq
#' dts_returns1 = pq_return(datadj, x = 'close_adj', freq = 'all')
#' 
#' # set method
#' dts_returns2 = pq_return(datadj, x = 'close_adj', method = 'log')
#' 
#' # set cols_keep
#' dts_returns3 = pq_return(datadj, x = 'close_adj', cols_keep = 'cap_total')
#' 
#' }
#' 
#' @export
pq_return = function(dt, x, freq='monthly', method='arithmetic', rcol_rename=NULL, cols_keep=NULL, date_range='max', from=NULL, to=Sys.Date(), print_step=1L) {
    symbol = NULL 
    
    # arg
    ## method 
    method = check_arg(method, c('arithmetic', 'log'), default='arithmetic', arg_name = 'method')
    # arithmetic: p_{t-1} * (r+1) = p_{t}
    # log: p_{t-1} * e^r = p_{t}
    ## freq
    freq = check_arg(freq, c('all', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly'), default='all', arg_name = 'freq')
    # if (!check_freq_isdaily(dt)) stop('Please provide daily data')
    ## from/to
    ft = get_fromto(date_range, from, to)
    from = ft$f
    to = ft$t
    ## dt
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    ## price column 
    x = check_xcol(dt, x)
    ## kept columns
    cols_keep = intersect(names(dt), unique(c('symbol', 'name', 'date', cols_keep)))
    ## dt preprocess
    dt = setDT(dt)[date >= from & date <= to][, unique(c(cols_keep, x)), with = FALSE]
    
    
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (i in seq_len(length(sybs))) {
        s = sybs[i]
        dt_s = dt[symbol == s]
        setkeyv(dt_s, "date")
        
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s/%s %s\n', i, length(sybs), s))
        dt_list[[s]] = do.call(
            'pq1_return', 
            args = list(dt=dt_s, x=x, method=method, freq=freq, rcol_rename = rcol_rename, cols_keep = cols_keep)
        )
    }
    return(dt_list)
}