check_odr = function(orders) {
    . = symbol = type = prices = values = volumes = NULL
    
    # symbol, date, type, prices, volumes, values
    if (inherits(orders, 'list')) orders = rbindlist(orders, fill = TRUE)
    
    if (inherits(orders, 'data.frame')) {
        orders = setDT(orders)[!is.na(type)]
        
        if ('date' %in% names(orders)) orders = orders[, date := as_date(date)]
        
        if (all(c('type', 'prices', 'volumes') %in% names(orders))) {
            orders = orders[
                type %in% c('sell', 'stc', 'sto'), volumes := - volumes
            ][, .(
                prices = sum(prices*volumes)/sum(volumes), 
                volumes = sum(volumes), 
                values = sum(prices*volumes)
            ), by = .(symbol, date)
            ][, type := ifelse(values >= 0, 'buy', 'sell')]
        }
    }
    
    return(orders)
}

odr_lag = function(dtorders, odrcols = c('type', 'prices', 'volumes')) {
    odrcols = intersect(odrcols, names(dtorders))
    dtorders = dtorders[, (odrcols) := lapply(.SD, shift), .SDcols = odrcols]
    return(dtorders)
}

odr_exp = function(dtorders, odrcols = c('type', 'prices', 'volumes'), kpallrow = FALSE, kp1strow = FALSE, ti = NULL) {
    type = ctcnt = prices = NULL
    
    cols_do = intersect(c('symbol', 'date', 'close', odrcols), names(dtorders))
    ti = c(ti, c('sma', 'bias', 'maroc', 'ma2roc', 'swing', 'runmax', 'runmin', 'swing'))
    cols_ti = sort(names(dtorders)[grepl(paste0(ti, collapse = '|'), names(dtorders))])
    
    dtorders = copy(dtorders)[
        , c(cols_do, cols_ti), with = FALSE
    ][, (cols_ti) := lapply(.SD, function(x) round(x,4)), .SDcols = cols_ti
    ][, prices := round(prices,2)][, close := round(close,2)]
    
    if (isFALSE(kpallrow)) {
        dtorders = dtorders[!is.na(type)]
        if (kp1strow) dtorders = dtorders[order(date)][, ctcnt := conticnt(type,cnt=TRUE)][abs(ctcnt) == 1][, ctcnt := NULL] 
    }
    
    return(dtorders[order(date)])
} 

odr_filter_vol = function(orders, odrvol = 'volumes') {
    type = rid = cumvol = NULL 
    
    orders = setkeyv(copy(orders), 'date')
    
    volindt = odrvol %in% names(orders)
    if (isFALSE(volindt)) orders = orders[, (odrvol) := 1]
    
    orders = orders[type == 'buy', (odrvol) := abs(get(odrvol))
    ][type == 'sell', (odrvol) := -abs(get(odrvol))
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
    volumes = type = NULL 
    
    if (!('volumes' %in% names(orders))) {
        orders[, volumes := 1]
    }
    
    orders = orders[
        type %in% c('bto', 'buy'), volumes := abs(volumes)
    ][type %in% c('stc', 'sell'), volumes := -abs(volumes)]
    
    return(orders)
}
odr_place = function(orders, init_fund = 10^6, rate = 1, odrvol = 'volumes', lot = 100) {
    rid=type=volumes=prices=position=value=equity=fund=balace=.=symbol=NULL 
    
    fundi = init_fund
    dtodr = copy(orders)[, rid := .I]
    
    for (i in dtodr[,rid]) {
        dtodr = dtodr[
            rid == i & type %in% c('bto', 'buy'), volumes := floor(fundi*rate/prices/lot)*lot 
        ][, position := cumsum(volumes)
        ]
        
        dtodr = dtodr[
            rid == i & type %in% c('stc', 'sell'), volumes := dtodr[i-1, -position]
        ][, position := cumsum(volumes)
        ]
        
        dtodr = dtodr[, value := volumes * prices 
        ][, equity := (position * prices)
        ][, fund := cumsum(-value) + init_fund
        ][, balace := equity + fund]
        
        fundi = dtodr[!is.na(fund)][.N,fund]
    }
    return(dtodr[,.(symbol, date, type, prices, volumes)])
}

