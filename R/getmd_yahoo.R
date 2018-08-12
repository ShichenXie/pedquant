#' @importFrom curl handle_setopt new_handle curl_download handle_cookies
handle_new_session = function(url="https://finance.yahoo.com", curl_options=list()) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in 1:5) {
        h = new_handle()
        handle_setopt(h, .list = curl_options)
        # random query to avoid cache
        cu = paste(url, paste(sample(c(LETTERS, letters, 0:9), 6), collapse = ""), sep="?")
        curl_download(cu, tmp, handle = h) #req = curl_fetch_memory(cu, handle = h)
        if (nrow(handle_cookies(h)) > 0) break
        Sys.sleep(0.1)
    }
    if (nrow(handle_cookies(h)) == 0) stop("Could not establish session after 5 attempts.")
    return(h)
}

yahoo_crumb = function(handle) {
    # get crumb
    url_getcrumb = sprintf("https://query%s.finance.yahoo.com/v1/test/getcrumb", ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L))
    cres = curl_fetch_memory(url_getcrumb, handle = handle)
    crumb = rawToChar(cres$content)
    
    return(crumb)
}


# stock, world index
# type = c("history", "div", "split")
#' @importFrom curl curl_fetch_memory 
getmd_stock1_yahoo = function(symbol, handle, crumb, frequency="daily", from="1900-01-01", to=Sys.time(), type="history") {
    Date = NULL
    
    # frequency
    intervals = c(daily = "1d", weekly = "1wk", monthly = "1mo")
    frequency = intervals[match.arg(frequency, names(intervals))]
    # from, to
    from_to = lapply(list(from=from, to=to), function(x) date_to_sec(x))
    # url of eod 
    urli <- sprintf("https://query%s.finance.yahoo.com/v7/finance/download/%s?period1=%.0f&period2=%.0f&interval=%s&events=history&crumb=%s", ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L), symbol, from_to$from, from_to$to, frequency, crumb)
    
    dat = load_read_csv(urli, handle = handle)
    setnames(setDT(dat), c("date", "open", "high", "low", "close", "close_adj", "volume"))
    dat[dat=="null"] = NA
    dat[, date := as.Date(date)]
    
    return(dat)
}





# currencies, commodities
getmd_curcom1_yahoo = function(symbol, handle, crumb, frequency="daily", from="1900-01-01", to=Sys.time()) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    
    # symbol = "BTCUSD=X"
    # handle = handle_new_session()
    # crumb = yahoo_crumb(handle)
    
    # frequency
    intervals = c(daily = "1d", weekly = "1wk", monthly = "1mo")
    frequency = intervals[match.arg(frequency, names(intervals))]
    # from, to
    from_to = lapply(list(from=from, to=to), function(x) date_to_sec(x))
    # url of eod 
    urli <- paste0("https://query",ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L),".finance.yahoo.com/v8/finance/chart/",symbol,"?symbol=",symbol,"&period1=",from_to$from,"&period2=",from_to$to,"&interval=",frequency,"&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=",crumb,"&corsDomain=finance.yahoo.com")
    
    curl_download(urli, tmp, handle = handle)
    dat2 = fromJSON(readLines(tmp))
    dat3 = data.table(
        date = dat2$chart$result$timestamp[[1]],
        symbol = dat2$chart$result$meta$symbol,
        open = dat2$chart$result$indicators$quote[[1]]$open,
        high = dat2$chart$result$indicators$quote[[1]]$high,
        low = dat2$chart$result$indicators$quote[[1]]$low,
        close = dat2$chart$result$indicators$quote[[1]]$close,
        close_adj = dat2$chart$result$indicators$adjclose[[1]],
        volume = dat2$chart$result$indicators$quote[[1]]$volume
    )[, date := as.POSIXct(date, origin="1970-01-01")]
    
    # dat[dat=="null"] = NA
    return(dat3)
}

# get market data from yahoo
getmd_yahoo = function(symbol, frequency="daily", from="1900-01-01", to=Sys.time(), print_step=1L) {
    
    handle = handle_new_session()
    crumb = yahoo_crumb(handle)
    
    # split symbols into currency/commodity and stock/index two groups
    is_curcom = grepl("=[FX]", symbol)
    symbol_stockindex = symbol[!is_curcom]
    symbol_curcom = symbol[is_curcom]
    
    dat_list_si = list()
    dat_list_cc = list()
    if (length(symbol_stockindex) > 0) {
        dat_list_si = load_dat_loop(symbol_stockindex, "getmd_stock1_yahoo", args = list(handle = handle, crumb = crumb, frequency=frequency, from = from, to = to), print_step=print_step)
    } 
    if (length(symbol_curcom) > 0) {
        dat_list_cc = load_dat_loop(symbol_curcom, "getmd_stock1_yahoo", args = list(handle = handle, crumb = crumb, frequency=frequency, from = from, to = to), print_step=print_step)
    }
    
    return(c(dat_list_si, dat_list_cc))
}


# currencies
# "https://query1.finance.yahoo.com/v8/finance/chart/BTCUSD=X?symbol=BTCUSD%3DX&period1=1472572800&period2=1502208000&interval=1d&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=FNY6lCVMYOY&corsDomain=finance.yahoo.com"

# commodities
# https://query1.finance.yahoo.com/v8/finance/chart/GCQ18.CMX?symbol=GCQ18.CMX&period1=1448899200&period2=1532614529&interval=1d&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=FNY6lCVMYOY&corsDomain=finance.yahoo.com

# world index # same as stock
# https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=1500996001&period2=1532532001&interval=1d&events=history&crumb=FNY6lCVMYOY

# money, bonds, currencies, indices, commodities
# market="world-indices"

# get symbols from yahoo
# get symbols of currency, commodity and world-indices from yahoo
#' @import data.table xml2
getmd_symbol_yahoo = function() {
    cat("For more details on the data provided by Yahoo Finance see", "\nhttps://help.yahoo.com/kb/SLN2310.html\n")

    df_symbol = lapply(
        list(currencies="currencies", indices="world-indices", commodities="commodities"), 
        function(mkt) {
            . = symbol = name = last_price = change = `%_change` = NULL
            
            # scrap web page
            wb = read_html(paste0("https://finance.yahoo.com/", mkt))
            # column names
            col_names = xml_text(xml_find_all(wb, "//th//span"))
            col_names = tolower(gsub(" ","_",col_names))
            
            # dataframe
            symbol_name = xml_text(xml_find_all(wb, "//td"))
            symbol_name = as.data.frame(matrix(symbol_name, ncol = length(col_names), byrow=TRUE))
            
            # renames
            setnames(setDT(symbol_name), col_names)
            
            return(symbol_name[,.(symbol, name, last_price, change, change_pct = `%_change`)])
        })
    
    return(rbindlist(df_symbol, idcol = "market")[,.(market, symbol, name)])
}
