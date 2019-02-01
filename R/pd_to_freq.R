# to_period in xts package

pd1_dtow = function(dat) {
    w=wi=.=symbol=name=high=low=volume=NULL
    
    dat[, w := isoweek(date)
      ][, wi := w - shift(w, 1, type="lag")
      ][, wi := ifelse(wi!=0 | is.na(wi), 1, wi)
      ][, wi := cumsum(wi)]
    
    dat2 = dat[, .(
        date=date[.N],
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = wi][, wi := NULL]
    
    return(dat2)
}
pd1_dtom = function(dat) {
    .=symbol=name=high=low=volume=NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date), m=month(date))][, (c('y','m')) := NULL]
    
    return(dat2)
}
pd1_dtoq = function(dat) {
    . = symbol = name = high = low = volume = NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date), q=quarter(date))][, (c('y','q')) := NULL]
    
    return(dat2)
}
pd1_dtoy = function(dat) {
    .=symbol=name=high=low=volume=NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date))][, (c('y')) := NULL]
    
    return(dat2)
}

check_freq_isdaily = function(dat) {
  setkeyv(dat, "date")
  # check freq of input data
  diff_date = dat[, as.numeric(mean(date - shift(date, n=1, type="lag"), na.rm=TRUE)) ]
  
  isdaily = ifelse(diff_date > 2, FALSE, TRUE)
  return(isdaily)
}

pd1_to_freq = function(dat, freq) {
    . = high = low = name = symbol = volume = NULL
    
    if (freq == "daily" || !check_freq_isdaily(dat)) return(dat)
    setkeyv(dat, "date")
    
    # change
    dat2 = do.call(paste0("pd1_dto",substr(freq,1,1)), list(dat=dat))
    
    # symbol and name
    if (all(c("symbol","name") %in% names(dat))) {
      dat2 = dat2[, .(date, symbol=dat[1,symbol], name=dat[1,name], open, high, low, close, volume)]
    } else if ("symbol" %in% names(dat)) {
      dat2 = dat2[, .(date, symbol=dat[1,symbol], open, high, low, close, volume)]
    }
    
    return(dat2)
}

#' convert the frequency of daily data
#' 
#' @param dt time series datasets
#' @param freq the frequency that the input data will converted to. It supports weekly, monthly, quarterly and yearly.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' dts = md_stock(c("^000001", "000001"), date_range = 'max', source = '163')
#' 
#' dts_weekly = pd_to_freq(dts, "weekly")
#' 
#' @export
#' 
pd_to_freq = function(dt, freq, print_step=0L) {
    symbol = len_names = dt_names = NULL
    # check freq argument
    freq = check_arg(freq, c("weekly","monthly","quarterly","yearly"), "weekly")
  
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    
    dt_list = list()
    sybs = dt[, unique(symbol)]
    for (i in seq_len(length(sybs))) {
      s = sybs[i]
      dt_s = dt[symbol == s]
      setkeyv(dt_s, "date")
      
      if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s/%s %s\n', i, len_names, dt_names[i]))
      dt_list[[s]] = do.call(pd1_to_freq, args = list(dat=dt_s, freq=freq))
    }
    
    
    return(dt_list)
}