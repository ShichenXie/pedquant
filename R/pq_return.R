return_arithmetic = function(x, x_lag) x/x_lag-1
return_log = function(x, x_lag) log(x/x_lag)
rateofchange = function(x, n=1, method = 'arithmetic') {
    p = p_lag = NULL
    data.table(p=x)[
        ,p_lag := shift(p,n=n,type='lag')
    ][, do.call(
        sprintf('return_%s', method), 
        args = list(p, p_lag)
    )]
}

dat_add_byfreq = function(dat, freq) {
    byfreq = funcs = byyear = wkdiff = wkN = NULL

    freqfunc = data.table(
        freqs = c('daily', 'weekly', 'monthly', 'quarterly', 'yearly'), 
        funcs = c('yday', 'isoweek', 'month', 'quarter', 'year'), 
        key = 'freqs'
    )
    
    dat2 = copy(dat)[
        , byfreq := do.call(freqfunc[freq,funcs], args = list(x=date))
    ][, byyear := year(date)]
    
    if (freq == 'weekly') {
        dat2 = dat2[byfreq %in% c(1,52,53), 
                  wkdiff := byfreq - shift(byfreq, 1, type="lag") 
        ][byfreq %in% c(1,52,53) & (wkdiff!=0|is.na(wkdiff)), wkdiff := 1
        ][byfreq %in% c(1,52,53), wkN := cumsum(wkdiff)
        ][byfreq %in% c(1,52,53) & wkN>0, byyear := max(byyear), by=wkN
        ][, (c('wkdiff', 'wkN')) := NULL]
    } 
    
    return(dat2)
}

#' @importFrom xefun date_bop date_eop
pq1_return = function(dt, x, freq='daily', n=1, date_type = 'eop', method = 'arithmetic', leading=FALSE, cumreturns = FALSE, rcol_name = NULL, cols_keep = c('symbol', 'date')) {
    . = Ra = chg = cumRa = byfreq = byyear = x_lag = NULL
    
    setkeyv(dt, "date")
    
    freq_list = c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
    if (freq == 'all') freq = freq_list
    freq = xefun:::c_list(intersect(freq_list, freq))
    
    
        
    dt_rt = lapply(freq, function(freqi) {
        dt2 = dat_add_byfreq(dt, freqi)
        
        # BOP, bebinning of period
        datbop = dt2[, .SD[1], by = .(byyear, byfreq)]
        # EOP, end of period
        dateop = dt2[, .SD[.N], by = .(byyear, byfreq)
        ][, x_lag := shift(get(x), n=n, type = 'lag') 
        ][]
        if (leading) dateop = dateop[1, x_lag := datbop[1,get(x)]][]
        
        col_Ra = paste0('returns.',freqi)
        dateop = dateop[, (col_Ra) := do.call(
            sprintf('return_%s', method), 
            args = list(x=get(x), x_lag)
        )][, unique(c(cols_keep, col_Ra)), with = FALSE
         ]
        
        col_cumRa = NULL 
        if (cumreturns == TRUE) {
            col_cumRa = paste0('cumreturns.',freqi)
            dateop = dateop[
                , chg := sum(get(col_Ra),1,na.rm = TRUE), keyby = c('symbol', 'date')
            ][, (col_cumRa) := cumprod(chg), by = 'symbol'
            ][, chg := NULL]
        }
        
        if (date_type == 'eop') {
            dateop = dateop[, date := as_date(date_eop(freqi, date, workday = TRUE)) ][]
        } else if (date_type == 'bop') {
            dateop = dateop[, date := as_date(date_bop(freqi, date, workday = TRUE)) ]
        }
        
        if (!is.null(rcol_name)) {
            cols_from = c(col_Ra, col_cumRa)
            cols_to = sub(col_Ra, rcol_name, cols_from)
            setnames(dateop, cols_from, cols_to)
        }
        return(dateop)
    })
    
    dt_rt2 = Reduce(function(x,y)merge(x,y,all=TRUE), dt_rt)[]
    return(dt_rt2)
}


#' calculating returns by frequency
#' 
#' \code{pq_return} calculates returns for daily series based on specified column, frequency and method type.
#' 
#' @param dt a list/dataframe of daily series.
#' @param x the column name of adjusted asset price. 
#' @param freq the frequency of returns. It supports 'daily', 'weekly', 'monthly', 'quarterly', 'yearly' and 'all'. Defaults to daily.
#' @param n the number of preceding periods used as the base value, defaults to 1, which means based on the previous period value.
#' @param date_type the available date type are eop (end of period) and bop (beginning of period), defaults to the eop.
#' @param method the method to calculate asset returns, the available methods including arithmetic and log, defaults to arithmetic. 
# @param leading whether to return the incomplete leading period returns.
#' @param cumreturns logical, whether to return cumulative returns. Defaults to FALSE. 
#' @param rcol_name setting the column name of returns, defaults to NULL.
#' @param cols_keep the columns keep in the return data. The columns of symbol, name and date will always kept if they are exist in the input data.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param ... ignored
#' 
#' @examples 
#' \donttest{
#' # load data and adjust
#' data(dt_banks)
#' datadj = md_stock_adjust(dt_banks, adjust = FALSE)
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
#' # cumulative returns
#' dts_cumreturns = pq_return(datadj, x = 'close_adj', from = '2012-01-01', cumreturns = TRUE)
#' e1 = pq_plot(dts_cumreturns, y = 'cumreturns.daily', title='cumreturns', 
#'         arrange = list(rows=1, cols=1))
#' e1[[1]]
#' }
#' 
#' @export
pq_return = function(dt, x, freq='daily', n=1, date_type='eop', method='arithmetic', cumreturns = FALSE, rcol_name = NULL, cols_keep=NULL, date_range='max', from=NULL, to=Sys.Date(), ...) {
    # arg
    args = list(...)
    if (!is.null(args$num)) n = args$num
    leading = args$leading
    if (is.null(leading)) leading = FALSE
    ## method 
    method = check_arg(method, c('arithmetic', 'log'), default='arithmetic', arg_name = 'method')
    # arithmetic: p_{t-1} * (r+1) = p_{t}
    # log: p_{t-1} * e^r = p_{t}
    ## freq
    freq = check_arg(freq, c('daily', 'weekly', 'monthly', 'quarterly', 'yearly', 'all'), default='daily', arg_name = 'freq')
    # if (!check_freq_isdaily(dt)) stop('Please provide daily data')
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = 'max')
    
    ## dt
    dt = check_dt(dt, symb_name = FALSE)
    ## price column 
    x = check_xcol(dt, x)
    ## kept columns
    if (cols_keep == 'all' && !is.null(cols_keep)) cols_keep = names(dt)
    cols_keep = intersect(names(dt), unique(c('symbol', 'name', 'date', cols_keep)))
    ## dt preprocess
    dt = setDT(dt)[date >= from & date <= to][, unique(c(cols_keep, x)), with = FALSE]
    
    
    dt_list = lapply(
        split(dt, by = 'symbol'), 
        function(dts) {do.call(
            'pq1_return', 
            args = list(dt=dts, x=x, method=method, leading=leading, freq=freq, n=n, cumreturns=cumreturns, rcol_name = rcol_name, cols_keep = cols_keep, date_type=date_type)
        )}
    )
    return(dt_list)
}