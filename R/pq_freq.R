# to_period in xts package

to_freq = function(dat, freq, date_type='eop') {
  .=amount=byfreq=byyear=close_prev=close_prev_lag=high=low=volume=NULL
  
  dat_byfreq = dat_add_byfreq(dat, freq)
  setkeyv(dat_byfreq, 'date')
  
  datN = dat_byfreq[
    , close_prev_lag := shift(close_prev, type = 'lead')
  ][][
    , .SD[.N], 
    keyby = .(byyear, byfreq),
    .SDcols = setdiff(names(dat_byfreq), c('open', 'high', 'low', 'close_prev', 'change_pct', 'volume', 'amount', 'turnover', 'byyear', 'byfreq'))
  ][, close_prev := shift(close_prev_lag, type = 'lag')
  ][]
  
  dat1 = dat_byfreq[,.(
    datebop = date[1], # date bebinning of period
    open = open[1], 
    high = max(high, na.rm = TRUE), 
    low  = min(low, na.rm = TRUE), 
    volume = sum(volume, na.rm = TRUE), 
    amount = sum(amount, na.rm = TRUE)
  ), 
  keyby = .(byyear, byfreq)
  ]
  
  if (date_type == 'eop') {
    datN = datN[, date := as_date(date_eop(freq, date, workday = TRUE)) ]
  } else if (date_type == 'bop') {
    datN = datN[, date := as_date(date_bop(freq, date, workday = TRUE)) ]
  }
  
  
  merge(
    datN, dat1, by = c('byyear', 'byfreq')
  )[, (c('byyear', 'byfreq')) := NULL
  ][]
  
}

pq1_freq = function(dat, freq, date_type='eop') {
    
    # if (freq == "daily" || !check_freq_isdaily(dat)) return(dat)
    setkeyv(dat, "date")
    datcols = names(dat)
    
    # add col
    for (col in setdiff(c('close_prev', 'volume', 'amount'), datcols)) dat[[col]] = 0
    # converting freq
    dat2 = to_freq(dat, freq=freq, date_type=date_type)
    # remove col
    for (col in setdiff(c('close_prev', 'volume', 'amount'), datcols)) dat2[[col]] = NULL
    
    kpcols = c('symbol', 'name', 'date', "open", "high", "low", "close", "close_prev", "volume", "amount", "cap_market", "cap_total", "unit")
    return(dat2[, intersect(datcols, kpcols), with = FALSE])
}

#' converting frequency of daily data
#' 
#' \code{pq_freq} convert a daily OHLC dataframe into a specified frequency.
#' 
#' @param dt a list/dataframe of time series dataset.
#' @param freq the frequency that the input daily data will converted to. It supports weekly, monthly, quarterly and yearly.
#' @param date_type the available date type are eop (end of period) and bop (bebinning of period), defaults to the eop. 
#' 
#' @examples 
#' \donttest{
#' data(dt_ssec)
#' dat1_weekly = pq_freq(dt_ssec, "weekly")
#' 
#' data(dt_banks)
#' dat2_weekly = pq_freq(dt_banks, "monthly")
#' }
#' @export
#' 
pq_freq = function(dt, freq = "monthly", date_type = "eop") {
    # check freq argument
    freq = check_arg(freq, c("weekly","monthly","quarterly","yearly"))
  
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    dt = setDT(dt)
    
    dt_list = lapply(
      split(dt, by = 'symbol'), 
      function(dts) {do.call(
        'pq1_freq', args = list(dat=dts, freq=freq)
      )}
    )
    
    return(dt_list)
}