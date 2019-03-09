# interbank offered rate, IBOR ------
# http://www.shibor.org/

# london
libor_symbol = setDT(list(
    symbol =paste0('uko', rep(c('usd','eur','gbp','jpy','chf'),each=7), c('on','1w','1m','2m','3m','6m','1y')),
    name = paste('libor', rep(c('usd','eur','gbp','jpy','chf'),each=7), c('overnight','1w','1m','2m','3m','6m','1y')),
    symbol_fred = paste0(rep(c('usd','eur','gbp','jpy','chf'),each=7), c('ont','1wk','1mt','2mt','3mt','6mt','12m'), 'd156n')
))

# shanghai
shibor_symbol = setDT(list(
    symbol = paste0('cnocny', c('on','1w','2w','1m','3m','6m','9m','1y')),
    name = paste('shibor cny', c('overnight','1w','2w','1m','3m','6m','9m','1y'))
))

# ibor symbol list
ibor_symbol = rbindlist(list(libor_symbol, shibor_symbol), fill = TRUE)
func_ibor_symbol = function() ibor_symbol    


# shanghai interbank offered rate, shibor
#' @import data.table xml2
md_shibor = function(symbol, from=NULL, to=Sys.Date(), print_step=1L) {
    . = name = value = V1 = NULL
    
    # arguments
    ## symbols
    syb_len = length(symbol)
    if (syb_len == 0) return(NULL)
    
    # download data
    if (as.integer(Sys.Date() - check_fromto(from)) <= 10 ) {
        # shibor in recent 10 days
        wb = read_html("http://www.shibor.org/shibor/ShiborTendaysShow_e.do")
        dt_shibor = setDT(xml_table(wb, 3)[[1]])[, V1 := as.Date(V1)]
        setnames(setDT(dt_shibor), c("date", shibor_symbol$symbol))
        
    } else {
        # shibor in history
        fromto_y = lapply(list(from=from, to=to), function(x) {
            # year of from/to
            y = as.integer(substr(x,1,4))
            # current year
            cur_year = as.integer(substr(Sys.Date(),1,4))
            # check year
            y = ifelse(y < 2006, 2006, ifelse(y > cur_year, cur_year, y))
            return(y)
        })
        years = seq(fromto_y$from, fromto_y$to)
        
        dt_shibor = lapply(years, function(y) {
            path = paste0("http://www.shibor.org/shibor/web/html/downLoad.html?nameNew=Historical_Shibor_Data_",y,".xls&nameOld=Shibor%CA%FD%BE%DD",y,".xls&shiborSrc=http%3A%2F%2Fwww.shibor.org%2Fshibor%2F&downLoadPath=data")
            dt = load_read_xl(path)
            
            setnames(setDT(dt), c("date", shibor_symbol$symbol))
            return(dt)# dt[, date := as.Date(date)]
        })
        dt_shibor = rbindlist(dt_shibor, fill = TRUE)[, date := as.Date(date)]
        
    }
    dt = melt(dt_shibor[date>=from & date<=to], id.vars = 'date', variable.name = 'symbol'
             )[shibor_symbol, on='symbol'][, .(symbol, name, date, value)]
    
    # return data list
    dt_list = list()
    for (i in seq_len(syb_len)) {
        syb_i = symbol[i]
        # print step info
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, syb_len)), collapse = '/'), syb_i))
        dt_list[[syb_i]] = setDT(dt[symbol == syb_i], key = 'date')
    }
    return(dt_list)
}

# euribor
# https://www.euribor-rates.eu/interesting-websites.asp

# london interbank offered rate, libor
md_libor1_last5 = function(currency) {
    symbol = value = . = name = NULL
    
    # libor in recent 5 days
    # from # https://www.global-rates.com
    
    #c('usd','eur','gbp','jpy','chf')
    url_lst = list(
        usd='/american-dollar/american-dollar.aspx', 
        gbp='/british-pound-sterling/british-pound-sterling.aspx', 
        eur='/european-euro/euro.aspx', 
        jpy='/japanese-yen/japanese-yen.aspx', 
        chf='/swiss-franc/swiss-franc.aspx')
    url = paste0('https://www.global-rates.com/interest-rates/libor', url_lst[currency])
    
    # scrap
    wb = read_html(url)
    dt_libor_5 = xml_table(wb, attr = '[@cellpadding="2"]', header = TRUE)[[1]]
    dt_libor_5 = dt_libor_5[,-1][,symbol := paste0('uko',currency,c('on','1w','2w',paste0(1:11,'m'),'1y'))]
    dt_libor_5[dt_libor_5=='-'] <- NA
    dt_libor_5 = melt(dt_libor_5, id.vars = 'symbol', na.rm = TRUE, variable.name = 'date')[, `:=`(
        value = as.numeric(sub('\\%', '', value)), 
        date = as.Date(date, format='%m-%d-%Y')
    )][libor_symbol[,.(symbol, name)], on='symbol', nomatch=0]
    return(dt_libor_5)
}
md_libor1_hist = function(syb, from, to) {
    symbol = symbol_fred = . = name = value = geo = NULL
    
    # libor in history
    dt_libor_hist = ed_fred(
        libor_symbol[symbol == syb, symbol_fred], from=from, to=to, print_step=0L
    )[[1]][,`:=`(symbol_fred = symbol, symbol = NULL, name = NULL
    )][libor_symbol, on='symbol_fred', nomatch=0
     ][, .(symbol, name, date, value, geo, unit)
     ][!is.na(value)]
    # return
    return(dt_libor_hist)
}

md_libor = function(symbol, from=NULL, to=Sys.Date(), print_step=1L) {
    # arguments
    ## symbols
    syb_len = length(symbol)
    if (syb_len == 0) return(NULL)
    
    # libor in last 5days
    currency = unique(substr(symbol, 4, 6))
    dat_lst_last5 = list()
    for (c in currency) dat_lst_last5[[c]] = md_libor1_last5(c)
    dat_last5 = rbindlist(dat_lst_last5)
    
    # libor
    dt_list = list()
    for (i in seq_len(syb_len)) {
        syb_i = symbol[i]
        # print step info
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, syb_len)), collapse = '/'), syb_i))
        # load data
        temp = rbind(md_libor1_hist(syb_i, from=from, to=to), dat_last5[symbol==syb_i],fill=TRUE)
        setkey(temp, 'date')
        cols_fillna = intersect(c('geo', 'unit'), names(temp))
        if (length(cols_fillna) > 0) {
            temp = unique(temp, by='date')[, (cols_fillna) := lapply(.SD, function(x) fillna(x)), .SDcols = cols_fillna]
        }
        dt_list[[syb_i]] = temp
    }
    return(dt_list)
}



# interbank offerd rate
# 
# @export
md_money = function(symbol=NULL, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = name = NULL
    
    # arguments
    ## symbol
    if (is.null(symbol)) {
        symbol = select_rows_df(ibor_symbol[,.(symbol,name)], column='symbol')[,symbol]
    } else if (length(symbol)==1) {
        symbol = select_rows_df(ibor_symbol[,.(symbol,name)], column='symbol', input_string=symbol)[,symbol]
    }
    syb = intersect(symbol, ibor_symbol$symbol)
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = "1000-01-01", default_date_range = '3y')
    from = ft$f
    to = ft$t
    
    # data
    dt_list = c(
        do.call(md_libor, args = list(symbol=syb[grepl('uk',syb)], from=from, to=to, print_step=print_step)), 
        do.call(md_shibor, args = list(symbol=syb[grepl('cn',syb)], from=from, to=to, print_step=print_step))
    )
    return(dt_list)
}

    
    
    
