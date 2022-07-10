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

odr_place = function(orders, init_fund = 10^6, rate = 1, odrvol = 'volumes') {
    rid=type=volumes=prices=position=value=equity=fund=balace=.=symbol=NULL 
    
    fundi = init_fund
    dtodr = copy(orders)[, rid := .I]
    
    for (i in dtodr[,rid]) {
        dtodr = dtodr[
            rid == i & type == 'buy', volumes := floor(fundi*rate/prices/100)*100 
        ][, position := cumsum(volumes)
        ]
        
        dtodr = dtodr[
            rid == i & type == 'sell', volumes := dtodr[i-1, -position]
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

