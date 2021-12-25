
check_wdf = function(w) {
    setDT(w)

    if (all(c('price', 'position') %in% names(w))) {
        w = w[, value := price * position
        ][, (c('price', 'position')) := NULL
        ][]
    }

    if ('type' %in% names(w)) {
        dim_type = data.table(
            type = c('buy_op', 'sell_cl', 'sell_op', 'buy_cl', 'buy', 'sell'),
            long_short = c('long', 'long', 'short', 'short', 'long', 'long'))

        w = merge(w, dim_type, by = 'type', all.x = TRUE)
    } else {
        w = w[, `:=`(
            long_short = 'long',
            type = 'buy_op'
        )]
    }

    if ('date' %in% names(w)) w = w[, date := as.Date(date)]

    w = w[, intersect(c('long_short', 'type', 'symbol', 'date', 'value'), names(w)), with = FALSE
        ][, symbol := check_symbol_for_yahoo(symbol)
        ][grepl('^sell', type), value := -abs(value)
    ]
    return(w)
}

w_summary = function(w) {
    value = position = price = . = NULL

    if (!all(c('long_short', 'type', 'symbol', 'date', 'price', 'position', 'value' ) %in% names(w))) w = check_wdf(w)

    w = w[, value := position * price
    ][, .(position = sum(position), value = sum(value)), by = c('long_short', 'symbol')]


    return(w)
}

#' creating the equity trends for portfolio
#'
#' \code{pq_portfolio} creates the equity trends of a portfolio secturites.
#'
#' @param dt a list/dataframe of time series dataset
#' @param R the column name of asset returns.
#' @param w a dataframe of position and price of all transactions.
#' @param init_equity initial equity
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#'
#' @examples
#' \donttest{
#' data(ssec)
#' # create a close_adj column
#' datadj = md_stock_adjust(ssec, adjust = FALSE)
#' # returns
#' datR = pq_return(datadj, x = 'close_adj', freq = 'daily',
#'                  rcol_rename = 'Ra', cols_keep = 'cap_total' )
#' datR$`000001.SS` = NULL
#'
#' w = data.frame(
#'       date = c('2016-01-08'),
#'       symbol = c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'),
#'       price = c(99.88, 618.88, 97.88, 115.88, 716.88),
#'       position = c(100, 200, 300, 400, 500),
#'       type = rep('buy_op', 5)
#'     )
#'
#' pf = pq_portfolio(datR, R = 'Ra', w)
#' }
#' @export
pq_portfolio = function() {
    # dt, R, w, init_value = 1, index=FALSE, date_range='max', from=NULL, to=Sys.Date()

    ## from/to
    ft = get_fromto(date_range, from, to)
    from = ft$f
    to = ft$t

    ## rbind list of dataframes
    if (inherits(dt, 'list')) dt = rbindlist(dt)
    dt = setDT(dt)[date >= from & date <= to]

    ## x
    R = check_xcol(dt, R)

    if (inherits(w, 'character') && w %in% names(dt)) {
        dt[, idx := cumprod(get(R)+1)][]


        ## weight geometric mean index
        wgm_idx = dt[, w := weight/sum(weight, na.rm = TRUE), keyby=date
        ][is.na(chain_index), chain_index := 0
        ][, cw := chain_index**w
        ][, .(value = prod(cw)), keyby=date
        ][value != 0]

    } else if (inherits(w, 'data.frame')) {
        ## w
        w = check_wdf(w)
        if ('date' %in% names(w)) dt = dt[date >= w[,min(date)]]
        # equity
        equity = merge(
            dt, w,
            by = intersect(c('symbol', 'date'), names(w)),
            all.x = TRUE
        )[, idx := cumprod(get(R) + 1), by = 'symbol'][]

        equity[is.na(position), position := 0
        ][is.na(price), price := close
        ][][order(symbol, date)
        ][][, cum_equity := cumsum(position)*close - position*(close-price), by = 'symbol']


        equity2 = dcast(equity, date ~ symbol, fun.aggregate = mean, value.var = 'cum_equity')[, lapply(.SD, fillna)]
        equity3 = equity2[,.(date)][, cum_equity := rowSums(copy(equity2)[, date := NULL], na.rm = TRUE)]


        if (is.null(init_equity)) init_equity = equity3[cum_equity!=0][.N, cum_equity]
        cash = w[, .(value = sum(price * position)), keyby = c('date')
        ][][, .(date, cash_value=init_equity-cumsum(value))][]

        dt = merge(
            equity3, cash, all.x = TRUE, by = 'date'
        )[][, cash_value := fillna(cash_value)
        ][][, .(date, symbol='equity', name='equity', value =cum_equity + cash_value)]
    }


    # ret_lst[['equity']] = dt
    # pq_plot(dt, x = 'value')
    return(dt)

    return(ret)
}

# return
dat = setDT(ssec)[symbol != '000001.SS']
datadj = md_stock_adjust(dat, adjust = FALSE)
dt = pq_return(datadj, x = 'close_adj', freq = 'monthly')
r = 'returns'
w = data.table(
    symbol = c("601288.SS", "601328.SS", "601398.SS", "601939.SS", "601988.SS"), 
    weights = c(0.1, 0.2, 0.3, 0.3, 0.1)
)

# equity
pq_portfolio_equity = function(dt, x, w, init_value=NULL) {
    . = symbol = current_equity = position = value = change = price = cum_equity = cash_value = NULL





    ret_lst = list()
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


    if (is.null(init_equity)) init_equity = equity3[cum_equity!=0][.N, cum_equity]
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
