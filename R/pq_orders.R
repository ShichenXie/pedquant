# symbol:
# date:
# time:
# side: buy-long 1, sell-long -1, sell-short -1.1, buy-short 1.1
# order_type: market, limit, stop, trailing stop, and conditional
# quantity:
# prices:
# values:
# stop loss:
# fees
# current prices:
# current values
check_odr = function(orders) {
    . = symbol = side = prices = values = quantity = side_term = n = term = NULL
    
    if (is.null(orders)) return(orders)
    # symbol, date, side, prices, quantity, values
    if (inherits(orders, 'list')) orders = rbindlist(orders, fill = TRUE)
    orders = setDT(orders)
    
    if (inherits(orders, 'data.frame')) {
        if ('date' %in% names(orders)) orders = orders[, date := as_date(date)]

        if (!('side' %in% names(orders))) orders = orders[, side := 1]
        orders = orders[!is.na(side)]
        
        if (all(c('side', 'prices', 'quantity') %in% names(orders))) {
            orders = orders[, quantity := side * abs(quantity)
            ][, .(
                prices = sum(prices*quantity)/sum(quantity), 
                quantity = sum(quantity), 
                values = sum(prices*quantity)
            ), by = .(symbol, date)
            ][, side := ifelse(values >= 0, 1, -1)]
        }
        
        # side level
        if (!('side_term' %in% names(orders))) {
            if ('n' %in% names(orders)) {
                orders = orders[, side_term := n] 
            } else if ('term' %in% names(orders)) {
                orders = orders[, side_term := term] 
            } else {
                orders = orders[, side_term := 10]
            }
        } 
        
        # side type
        if (!('side_bs' %in% names(orders))) {
            orders = orders[
                data.table(side=c(1,-1, 1.1,-1.1), side_bs = c('buy', 'sell', 'buy_short', 'sell_short')), 
                on='side', nomatch=0
            ][]
        }
        
    }
    
    return(orders)
}

odr_lag = function(dtorders, odrcols = c('side', 'prices', 'quantity')) {
    odrcols = intersect(odrcols, names(dtorders))
    dtorders = dtorders[, (odrcols) := lapply(.SD, shift), .SDcols = odrcols]
    return(dtorders)
}

odr_exp = function(dtorders, odrcols = c('side', 'prices', 'quantity'), kpallrow = FALSE, kp1strow = FALSE, ti = NULL) {
    side = ctcnt = prices = NULL
    
    cols_do = intersect(c('symbol', 'date', 'close', odrcols), names(dtorders))
    ti = c(ti, c('sma', 'bias', 'maroc', 'ma2roc', 'swing', 'runmax', 'runmin', 'swing'))
    cols_ti = sort(names(dtorders)[grepl(paste0(ti, collapse = '|'), names(dtorders))])
    
    dtorders = copy(dtorders)[
        , c(cols_do, cols_ti), with = FALSE
    ][, (cols_ti) := lapply(.SD, function(x) round(x,4)), .SDcols = cols_ti
    ][, prices := round(prices,2)][, close := round(close,2)]
    
    if (isFALSE(kpallrow)) {
        dtorders = dtorders[!is.na(side)]
        if (kp1strow) dtorders = dtorders[order(date)][, ctcnt := conticnt(side,cnt=TRUE)][abs(ctcnt) == 1][, ctcnt := NULL] 
    }
    
    return(dtorders[order(date)])
} 

odr_filter_vol = function(orders, odrvol = 'quantity') {
    side = rid = cumvol = NULL 
    
    orders = setkeyv(copy(orders), 'date')
    
    volindt = odrvol %in% names(orders)
    if (isFALSE(volindt)) orders = orders[, (odrvol) := 1]
    
    orders = orders[side == 1, (odrvol) := abs(get(odrvol))
    ][side == -1, (odrvol) := -abs(get(odrvol))
    ]
    
    NegPos1Rid = function(orders) {
        copy(orders)[, cumvol := cumsum(get(odrvol))
             ][, rid := .I
             ][cumvol<0
             ][1, rid]
    }
        
    while (!is.na(NegPos1Rid(orders))) {
        orders = orders[-NegPos1Rid(orders)]
    }
        
    if (isFALSE(volindt)) orders = orders[, (odrvol) := NULL]
    return(orders)
}

odr_addvol = function(orders) {
    quantity = side = NULL 
    
    if (!('quantity' %in% names(orders))) {
        orders[, quantity := 1]
    }
    
    orders = orders[, quantity := side * abs(quantity)]
    
    return(orders)
}
odr_place = function(orders, init_fund = 10^6, rate = 1, odrvol = 'quantity', lot = 100) {
    rid=side=quantity=prices=position=value=equity=fund=balace=.=symbol=NULL 
    
    fundi = init_fund
    dtodr = copy(orders)[, rid := .I]
    
    for (i in dtodr[,rid]) {
        dtodr = dtodr[
            rid == i & side %in% c(1), quantity := floor(fundi*rate/prices/lot)*lot 
        ][, position := cumsum(quantity)
        ]
        
        dtodr = dtodr[
            rid == i & side %in% c(-1), quantity := dtodr[i-1, -position]
        ][, position := cumsum(quantity)
        ]
        
        dtodr = dtodr[, value := quantity * prices 
        ][, equity := (position * prices)
        ][, fund := cumsum(-value) + init_fund
        ][, balace := equity + fund]
        
        fundi = dtodr[!is.na(fund)][.N,fund]
    }
    return(dtodr[,.(symbol, date, side, prices, quantity)])
}

