#' @importFrom curl handle_setopt new_handle curl_download handle_cookies
handle_new_session = function(url="https://finance.yahoo.com", curl_options=list(), env = NULL, handle_name = ".handle", reload = FALSE) {
    h = NULL
    if (!is.null(env)) h = get0(handle_name, env)
    
    if (is.null(h) || reload) {
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
      
      if (!is.null(env)) assign(handle_name, h, env)
    }
    
    return(h)
}

yahoo_crumb = function(handle) {
    # crumb
    url_getcrumb = sprintf("https://query%s.finance.yahoo.com/v1/test/getcrumb", ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L))
    cres = try(curl_fetch_memory(url_getcrumb, handle = handle), silent = TRUE)
    crumb = rawToChar(cres$content)
    
    return(crumb)
}


# @importFrom rvest html_nodes
get_symbol_name_yahoo = function(syb, encode='UTF-8', handle=new_handle()) {
  . = NULL
  
  url = sprintf('https://finance.yahoo.com/quote/%s', syb)
  
  temp = tempfile()
  on.exit(unlink(temp))
  
  curl_download(url, destfile = temp, handle = handle)
  dat = read_html(temp)
  # object.size(dat)
  # syb_nam = xml_text(xml_find_all(dat, '//h1'))
  # syb_nam = unlist(strsplit(syb_nam, ' - '))
  syb_nam = html_nodes(dat, 'h1') %>% 
    html_text() %>% 
    sub('\\)', '', .) %>% 
    strsplit(., ' \\(') %>% 
    .[[1]]
  # Location, Currency
  # syb_nam2 = xml_text(xml_find_all(dat, '//div[@id="quote-header-info"]//span'))
  # syb_nam2 = unlist(strsplit(syb_nam2[1], ' - |\\. Currency in '))[c(1,3)]
  syb_nam2 = html_nodes(dat, xpath = '//div[@class="Mt(15px)"]//span') %>% 
    html_text() %>% 
    .[1] %>% 
    sub('.+ Currency in ', '', .)
  
  return(list(symbol=syb_nam[2], name=syb_nam[1], currency=syb_nam2))#list(sn=syb_nam, lc=syb_nam2))
}
# stock, world index
# type = c("history", "div", "split")
# https://query1.finance.yahoo.com/v7/finance/download/000001.SZ?period1=1511841299&period2=1543377299&interval=1d&events=split&crumb=j/T2/8/3fvH
#' @importFrom curl curl_fetch_memory 
md_stock1_yahoo = function(symbol, handle, crumb, freq="daily", from="1900-01-01", to=Sys.time(), type="history", na_rm=TRUE, ...) {
    name = splits = s2 = s1 = . = high = low = close_adj = volume = dividends = NULL
    
    # symbol
    syb = check_symbol_for_yahoo(symbol)
    # type
    # type = check_arg(type, c('history', 'dividend', 'split'), default = 'history')
    # if (type=='dividend') {type = 'div'} #else if (type=='splits') {type = 'split'}
    # symbol, name, currency
    syb_nam_cur = get_symbol_name_yahoo(syb, handle = handle)
    
    # url of eod 
    # https://query1.finance.yahoo.com/v7/finance/download/000001.SZ?period1=1423526400&period2=1581292800&interval=1d&events=history&crumb=2pDrF2QYj1H
    
    if (type=='history') {
      dat = load_read_csv(
        url = sprintf("https://query%s.finance.yahoo.com/v7/finance/download/%s?period1=%.0f&period2=%.0f&interval=%s&events=%s&crumb=%s", ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L), syb, from, to, freq, type, crumb), 
        handle = handle)
      
      cols_num = c("open", "high", "low", "close", "close_adj", "volume")
      setnames(setDT(dat), c("date", cols_num))
      
      dat[dat=="null"] = NA
      dat = dat[, `:=`(
        symbol = syb_nam_cur$symbol, name = syb_nam_cur$name, unit=syb_nam_cur$currency
      )][, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num
       ][,.(symbol, name, date, open, high, low, close, close_adj, volume, unit)]
      
      if (na_rm) dat = dat[!is.na(close)]
      # adjusting ohlc
      # adjust = list(...)[['adjust']]
      # if (adjust) dat = md_stock_adjust(dat, source = 'yahoo', ...)
      dat = do.call('md_stock_adjust1', args = c(list(dt = dat, source = 'yahoo'), list(...)))
    } else {
      div_spl = lapply(c('split', 'div'), function(t) {
        tbl = load_read_csv(
          url = sprintf("https://query%s.finance.yahoo.com/v7/finance/download/%s?period1=%.0f&period2=%.0f&interval=%s&events=%s&crumb=%s", ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L), syb, date_to_sec(check_fromto('1900-01-01')), to, freq, t, crumb), 
          handle = handle)
        
        col_name = ifelse(t=='div', 'dividends', 'splits')
        setnames(setDT(tbl), c('date', col_name))
        return(tbl)
      })
      
      dat = Reduce(
        function(x,y) merge(x,y,all=TRUE,by='date'), div_spl
      )
      if (nrow(dat)>0) dat = dat[, (c('s1','s2')) := tstrsplit(splits, '[^0-9]')]
      
      dat = dat[,`:=`(
        symbol = syb_nam_cur$symbol, 
        name = syb_nam_cur$name
      )][, .(
        symbol, name, date, 
        splits = as.numeric(s1)/as.numeric(s2) - 1,
        dividends
      )]
    }
    
    dat = dat[, date := as.Date(date)]
    setkeyv(dat, 'date')
    return(dat)
}

# query market data from yahoo
md_stock_yahoo = function(symbol, freq="daily", from="1900-01-01", to=Sys.time(), print_step=1L, na_rm=TRUE, ...) {
  
    yahoo_env = list(...)[["env"]]
    reload = list(...)[["reload"]]
    if (is.null(reload)) reload = FALSE
    handle = handle_new_session(env = yahoo_env, handle_name = "yahoo_handle")
    crumb = try(yahoo_crumb(handle), silent = TRUE)
    if (inherits(crumb, 'try-error') || reload) {
      handle = handle_new_session(env = yahoo_env, handle_name = "yahoo_handle", reload = TRUE)
      crumb = yahoo_crumb(handle)
    }
    
    # type
    arg = list(...)

    # frequency
    intervals = c(daily = "1d", weekly = "1wk", monthly = "1mo")
    freq = check_arg(freq, names(intervals))
    freq = intervals[names(intervals) == freq]
    
    # from, to
    from = date_to_sec(check_fromto(from))
    to = date_to_sec(check_fromto(to))
    
    # load data
    dat_list = load_dat_loop(toupper(symbol), "md_stock1_yahoo", args = c(list(handle = handle, crumb = crumb, freq=freq, from = from, to = to, na_rm=na_rm), arg), print_step=print_step)
    return(dat_list)
}


# currencies
# "https://query1.finance.yahoo.com/v8/finance/chart/BTCUSD=X?symbol=BTCUSD%3DX&period1=1472572800&period2=1502208000&interval=1d&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=FNY6lCVMYOY&corsDomain=finance.yahoo.com"

# commodities
# https://query1.finance.yahoo.com/v8/finance/chart/GCQ18.CMX?symbol=GCQ18.CMX&period1=1448899200&period2=1532614529&interval=1d&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=FNY6lCVMYOY&corsDomain=finance.yahoo.com

# world index # same as stock
# https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=1500996001&period2=1532532001&interval=1d&events=history&crumb=FNY6lCVMYOY

# money, bonds, currencies, indices, commodities
# market="world-indices"











#######
# query symbols from yahoo
# query symbols of currency, commodity and world-indices from yahoo
#' @import data.table xml2
md_symbol_yahoo = function(market=NULL) {
    . = name = symbol = NULL
  
    mkts_yahoo = list(currency="currencies", index="world-indices", commodity="commodities")
    if (market == "stock") {
      cat("Details on the stock/index provided by Yahoo Finance see\n",
          "https://finance.yahoo.com\n",
          "https://help.yahoo.com/kb/SLN2310.html\n\n")
      return()
      
    } else if (is.null(market) || !any(names(mkts_yahoo) %in% market)) {
      mkts_sel = mkts_yahoo
      
    } else {
      mkts_sel = mkts_yahoo[which(names(mkts_yahoo) %in% market)]
      
    }
    
    df_symbol = lapply(
      mkts_sel, 
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

# currencies, commodities
md_curcom1_yahoo = function(symbol, handle, crumb, freq="daily", from="1900-01-01", to=Sys.time(), adjust = 'split') {
  . = high = low = close_adj = volume = NULL
  
  tmp <- tempfile()
  on.exit(unlink(tmp))
  
  # symbol = "BTCUSD=X"
  # handle = handle_new_session()
  # crumb = yahoo_crumb(handle)
  
  
  # url of eod 
  urli <- paste0("https://query",ifelse(unclass(Sys.time()) %% 1L >= 0.5, 1L, 2L),".finance.yahoo.com/v8/finance/chart/",symbol,"?symbol=",symbol,"&period1=",from,"&period2=",to,"&interval=",freq,"&includePrePost=true&events=div%7Csplit%7Cearn&lang=en-US&region=US&crumb=",crumb,"&corsDomain=finance.yahoo.com")
  
  curl_download(urli, tmp, handle = handle)
  dat2 = fromJSON(readLines(tmp))
  
  cols_num = c("open", "high", "low", "close", "close_adj", "volume")
  dat3 = data.table(
    date = dat2$chart$result$timestamp[[1]],
    symbol = dat2$chart$result$meta$symbol,
    open = dat2$chart$result$indicators$quote[[1]]$open,
    high = dat2$chart$result$indicators$quote[[1]]$high,
    low = dat2$chart$result$indicators$quote[[1]]$low,
    close = dat2$chart$result$indicators$quote[[1]]$close,
    close_adj = dat2$chart$result$indicators$adjclose[[1]],
    volume = dat2$chart$result$indicators$quote[[1]]$volume
  )[, date := as.POSIXct(date, origin="1970-01-01")
    ][, (cols_num) := lapply(.SD, as.numeric), 
      .SDcols=cols_num
      ][,.(date, symbol, open, high, low, close, close_adj, volume)]
  setkey(dat3, date)
  
  # adjusting ohlc
  # if (adjust) dat = adjust_ohlc(dat)
  # dat[dat=="null"] = NA
  return(dat3)
}
