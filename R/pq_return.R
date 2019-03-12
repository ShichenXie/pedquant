r_arithmetic = function(x, x_lag) x/x_lag-1
r_log = function(x, x_lag) log(x/x_lag)

pq1_dreturn = function(dt, x='close|value', type='arithmetic') {
    x_lag = daily_return = NULL
    xx = intersect(unlist(strsplit(x,'\\|')), names(dt))[1]
    dt2 = copy(dt)[, x_lag := shift(get(xx),n=1,type = 'lag')
                 ][, daily_return := do.call(paste0('r_',type), args = list(x=get(xx), x_lag))]
    
    cols_ret = intersect(c('symbol', 'name', 'date', 'daily_return'), names(dt2))
    return(dt2[,cols_ret, with=FALSE])
}

pq1_wreturn = function(dt, x='close|value', type='arithmetic') {
    w = wi = x_lag = weekly_return = NULL
    
    xx = intersect(unlist(strsplit(x,'\\|')), names(dt))[1]
    dt2 = copy(dt)[, w := isoweek(date)
                   ][, wi := w - shift(w, 1, type="lag")
                     ][, wi := ifelse(wi!=0 | is.na(wi), 1, wi)
                       ][, wi := cumsum(wi)]
    
    dt3 = dt2[, .SD[.N], by = wi
            ][, x_lag := shift(get(xx),n=1,type = 'lag')
              ][, weekly_return := do.call(paste0('r_',type), args = list(x=get(xx), x_lag))]
    
    if (xx == 'close' & 'open' %in% names(dt)) {
        dt3[1, weekly_return := dt2[wi==1, do.call(paste0('r_',type), args = list(x=close[.N], x_lag=open[1]))]]
    } else {
        dt3[1, weekly_return := dt2[wi==1, do.call(paste0('r_',type), args = list(x=get(xx)[.N], x_lag=get(xx)[1]))]]
    }

    cols_ret = intersect(c('symbol', 'name', 'date', 'weekly_return'), names(dt3))
    return(dt3[,cols_ret, with=FALSE])
}

pq1_mreturn = function(dt, x='close|value', type='arithmetic') {
    .=y=m=x_lag=monthly_return=NULL
    
    xx = intersect(unlist(strsplit(x,'\\|')), names(dt))[1]
    dt2 = copy(dt)[, `:=`(y=year(date), m=month(date))]
    
    dt3 = copy(dt2)[, .SD[.N], by = .(y, m)
                 ][, x_lag := shift(get(xx),n=1,type = 'lag') 
                 ][, monthly_return := do.call(paste0('r_',type), args = list(x=get(xx), x_lag))]
    
    if (xx == 'close' & 'open' %in% names(dt)) {
        dt3[1, monthly_return := dt2[y==y[1] & m==m[1], do.call(paste0('r_',type), args = list(x=close[.N], x_lag=open[1]))]]
    } else {
        dt3[1, monthly_return := dt2[y==y[1] & m==m[1], do.call(paste0('r_',type), args = list(x=get(xx)[.N], x_lag=get(xx)[1]))]]
    }
    
    cols_ret = intersect(c('symbol', 'name', 'date', 'monthly_return'), names(dt3))
    return(dt3[,cols_ret, with=FALSE])
}

pq1_qreturn = function(dt, x='close|value', type='arithmetic') {
    . = y = x_lag = quarterly_return = NULL
    
    xx = intersect(unlist(strsplit(x,'\\|')), names(dt))[1]
    dt2 = copy(dt)[, `:=`(y=year(date), q=quarter(date))]
    
    dt3 = copy(dt2)[, .SD[.N], by = .(y, q)
                    ][, x_lag := shift(get(xx),n=1,type = 'lag') 
                      ][, quarterly_return := do.call(paste0('r_',type), args = list(x=get(xx), x_lag))]
    
    if (xx == 'close' & 'open' %in% names(dt)) {
        dt3[1, quarterly_return := dt2[y==y[1] & q==q[1], do.call(paste0('r_',type), args = list(x=close[.N], x_lag=open[1]))]]
    } else {
        dt3[1, quarterly_return := dt2[y==y[1] & q==q[1], do.call(paste0('r_',type), args = list(x=get(xx)[.N], x_lag=get(xx)[1]))]]
    }
    
    cols_ret = intersect(c('symbol', 'name', 'date', 'quarterly_return'), names(dt3))
    return(dt3[,cols_ret, with=FALSE])
}

pq1_yreturn = function(dt, x='close|value', type='arithmetic') {
    .=y=x_lag=yearly_return=NULL
    
    xx = intersect(unlist(strsplit(x,'\\|')), names(dt))[1]
    dt2 = copy(dt)[, `:=`(y=year(date))]
    
    dt3 = copy(dt2)[, .SD[.N], by = .(y)
                    ][, x_lag := shift(get(xx),n=1,type = 'lag') 
                      ][, yearly_return := do.call(paste0('r_',type), args = list(x=get(xx), x_lag))]
    
    if (xx == 'close' & 'open' %in% names(dt)) {
        dt3[1, yearly_return := dt2[y==y[1], do.call(paste0('r_',type), args = list(x=close[.N], x_lag=open[1]))]]
    } else {
        dt3[1, yearly_return := dt2[y==y[1], do.call(paste0('r_',type), args = list(x=get(xx)[.N], x_lag=get(xx)[1]))]]
    }
    
    cols_ret = intersect(c('symbol', 'name', 'date', 'yearly_return'), names(dt3))
    return(dt3[,cols_ret, with=FALSE])
}


pq1_return = function(dt, x='close|value', type='arithmetic', freq='daily', date_range='3y', from=NULL, to=Sys.Date()) {
    setkeyv(dt, "date")
    if (freq == 'all') freq = c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
    
    
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = dt[1,date], default_date_range = 'max')
    from = ft$f
    to = ft$t
    
    
    dt_rt = lapply(freq, function(f) {
        do.call(sprintf("pq1_%sreturn",substr(f,1,1)), list(dt=dt[date>=from & date<=to], x=x, type=type))
    })

    by_cols = intersect(c('symbol', 'name', 'date'), names(dt))
    dt_rt2 = Reduce(function(x,y) merge(x,y, all.x=TRUE, by=by_cols), dt_rt)
    return(dt_rt2)
}


#' calculating returns by frequency
#' 
#' \code{pq_return} calculates returns for daily series based on specified column, frequency and method type.
#' 
#' @param dt a list/dataframe of daily series dataset
#' @param x the variable used to calculate returns.
#' @param method the method to calcualte returns.
#' @param freq the frequency of returns. It supports c('all', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly').
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' 
#' @examples 
#' \donttest{
#' #' dts = md_stock(c('000001', '^000001'), source = '163')
#' 
#' # set freq
#' dts_returns1 = pq_return(dts, freq = 'all')
#' dts_returns2 = pq_return(dts, freq = 'weekly')
#' 
#' # set method
#' dts_returns3 = pq_return(dts, freq = 'monthly', method = 'arithmetic') # default method
#' dts_returns4 = pq_return(dts, freq = 'monthly', method = 'log')
#' }
#' 
#' @export
pq_return = function(dt, x='close|value', method='arithmetic', freq='all', date_range='max', from=NULL, to=Sys.Date(), print_step=1L) {
    symbol = type = NULL
    
    method = check_arg(method, c('arithmetic', 'log'), default='arithmetic', arg_name = 'method')
    # arithmetic: p_{t-1} * (r+1) = p_{t}
    # log: p_{t-1} * e^r = p_{t}
    freq = check_arg(freq, c('all', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly'), default='all', arg_name = 'freq')
    # if (!check_freq_isdaily(dt)) stop('Please provide daily data')
    
    
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (i in seq_len(length(sybs))) {
        s = sybs[i]
        dt_s = dt[symbol == s]
        setkeyv(dt_s, "date")
        
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s/%s %s\n', i, length(sybs), s))
        dt_list[[s]] = do.call(pq1_return, args = list(dt=dt_s, x=x, type=method, freq=freq, date_range=date_range, from=from, to=to))
    }
    return(dt_list)
}