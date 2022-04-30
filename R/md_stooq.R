# https://stooq.com

md_stooq1 = function(syb, from, to, freq='d', sybnam=NULL, ...) {
    symbol = name = NULL
    
    # freq: d, w, m, q, y
    url = sprintf("https://stooq.com/q/d/l/?s=%s&d1=%s&d2=%s&i=%s", syb, format(as_date(from), '%Y%m%d'), format(as_date(to), '%Y%m%d'), freq)
    
    dt = try(load_read_csv2(url), silent=TRUE) 
    if (inherits(dt, 'try-error')) dt = load_read_csv(url)
    if (nrow(dt)==0) {
        dt = data.table(symbol='0', name='0', date=as_date('10000101'), open=0, high=0, low=0, close=0)[.0]
        return(dt)
    }
    
    nam = syb
    if (!is.null(sybnam)) nam = sybnam[symbol == toupper(syb), name]
    if (length(nam) == 0) nam = syb
    
    dt = setnames(setDT(dt), tolower(names(dt)))
    dt = cbind(
        data.table(symbol=toupper(syb), name=nam), 
        dt[, date := as_date(date)]
    )
    
    return(dt)
}

md_stooq = function(symbol, type = 'history', date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    sybnam = setDT(copy(symbol_stooq))[toupper(symbol), on='symbol']
    
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    
    dat_list = load_dat_loop(
        tolower(symbol), 'md_stooq1', 
        args = list(from = from, to = to, sybnam=sybnam, ...), 
        print_step=print_step, ...)
    dat_list = rm_error_dat(dat_list)
    
    return(dat_list)
}

