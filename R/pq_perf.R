# d, w, m, q, ytd, y, 

pq1_perf = function(dt, date_range='max', from=NULL, to=Sys.Date(), x='close|value', base_value=1) {
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


check_w_position = function(w) {
  symbol = type = position = . = long_short = price = NULL
  
  dim_type = data.table(
    type = c('buy', 'sell', 'bto', 'stc', 'sto', 'btc'), 
    long_short = c(rep('long',4), rep('short',2)))
  
  cols = intersect(names(w), names(dim_type))
  
  w = merge(
    w, dim_type, by = cols, all.x = TRUE
  )[, symbol := check_symbol_for_yahoo(symbol)
  ][grepl('sell|st.', type), position := -abs(position)
  ][, .(long_short, type, symbol, date, price, position, value = price*position)]
  
  return(w)
}

w_summary = function(w) {
  value = position = price = . = NULL
  
  if (!all(c('long_short', 'type', 'symbol', 'date', 'price', 'position', 'value' ) %in% names(w))) w = check_w_position(w)
  
  w = w[, value := position * price
  ][, .(position = sum(position), value = sum(value)), by = c('long_short', 'symbol')]
  
  return(w)
}

# creating the equity trends for portfolio 
# 
# \code{pq_portfolio} creates the equity trends of a portfolio secturites. 
# 
# @param dt a list/dataframe of time series dataset
# @param w a dataframe of position and price of all transactions. 
# @param init_equity initial equity
# @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
# @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
# @param to the end date. Default is the current date.
# @param x the name of column to calculate. Default is 'close|value'.
# @param summarise logical, whether to display summary of portfolio position
# 
# @examples 
# \donttest{
# dat = md_stock(c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'), date_range = 'max')
# w = data.frame(
#       date = c('2016-01-08'),
#       symbol = c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'), 
#       price = c(99.88, 618.88, 97.88, 115.88, 716.88),
#       position = c(100, 200, 300, 400, 500)
#     )
# pf = pq_portfolio(dat, w)
# }
# @export
pq_portfolio = function(dt, w, init_equity=NULL, date_range='max', from=NULL, to=Sys.Date(), x='close|value', summarise = FALSE) {
    . = symbol = current_equity = position = value = change = price = cum_equity = cash_value = NULL
  
    ## rbind list of dataframes 
    if (inherits(dt, 'list')) dt = rbindlist(dt)
    # x
    x1 = intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    if (x1 != 'close') dt[['close']] = dt[[x1]]
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = dt[1,date], default_date_range = 'max')
    from = ft$f
    to = ft$t
    ## w
    w = w[, date := as.Date(date)][date >= from & date <= to]
    w = check_w_position(w)
    
    
    ret_lst = list()
    if (summarise) {
      summarise_w = merge(
        w_summary(w), 
        dt[date <= to][, .SD[.N], by = 'symbol'][,.(symbol, close)], 
        all.x = TRUE, by = 'symbol'
      )[, current_equity := position * close
        ][, `:=`(
          change = current_equity - value,
          change_pct = current_equity/value-1
        )][order(-change)]
      
      # ret_lst[['summarise']] = 
      print(summarise_w)
    }
    
    
    
    equity = merge(
        dt[, c('symbol','date','name','close'), with=FALSE][date >= w[, min(date)], ], 
        w, 
        by = c('symbol', 'date'), all.x = TRUE
    )[is.na(position), position := 0
      ][is.na(price), price := close
        ][][order(symbol, date)
            ][][, cum_equity := cumsum(position)*close - position*(close-price), by = 'symbol']
    
    
    equity2 = dcast(equity, date ~ symbol, fun.aggregate = mean, value.var = 'cum_equity')[, lapply(.SD, fillna)]
    equity3 = equity2[,.(date)][, cum_equity := rowSums(copy(equity2)[, date := NULL], na.rm = TRUE)]
    
    
    if (is.null(init_equity)) init_equity = equity3[.N, cum_equity]
    cash = w[, .(value = sum(price * position)), keyby = c('date')
             ][][, .(date, cash_value=init_equity-cumsum(value))][]
    
    dt = merge(
        equity3, cash, all.x = TRUE, by = 'date'
    )[][, cash_value := fillna(cash_value)
        ][][, .(date, symbol='equity', name='equity', value =cum_equity + cash_value)]
    
    # ret_lst[['equity']] = dt
    # pq_plot(dt, x = 'value')
    return(dt)
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
pq_perf = function(dt, date_range='max', from=NULL, to=Sys.Date(), x='close|value', base_value=1) {
    symbol = NULL
    
    # bind list of dataframe
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    # x
    x1 = intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    
    # plot symbol
    dt_list = list()
    sybs = dt[, unique(symbol)]
    x = intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    for (s in sybs) {
        dt_s = dt[symbol == s]
        setkeyv(dt_s, 'date')
        
        dt_list[[s]] = do.call(pq1_perf, args = list(dt=dt_s, x=x, date_range=date_range, from=from, to=to, base_value=base_value))
    }
    
    return(dt_list)
}
