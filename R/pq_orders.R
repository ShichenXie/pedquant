odr_filter_machg = function(dtorders, ma, n=1, odrcols = c('type', 'prices', 'volumes')) {
    machg = type = NULL
    
    odrcols = intersect(odrcols, names(dtorders))
    
    # order type should consistent with the slop of moving average
    pq_return(
        copy(dtorders), x=ma, freq = 'daily', num=n, 
        rcol_name = 'machg', cols_keep = 'all'
    )[[1]][
        # !is.na(type)
    ][machg < 0 & type == 'buy', (odrcols) := NA  
    ][machg > 0 & type == 'sell', (odrcols) := NA  
    ][, machg := NULL
    ][]
    
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

