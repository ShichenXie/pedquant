# hkex ------
#' @import data.table
#' @importFrom stringi stri_unescape_unicode
md_stock_symbol_hk = function(source="sina", return_price=TRUE) {
  symbol = exchange = market_symbols = stock_list_hk2 = ticktime = NULL

  if (source == "163") {
    # function
    fun_listcomp_hk = function(urli, mkt_board) {
      dt = V1 = name = NULL

      doc = readLines(urli, warn = FALSE)

      # date
      datetime = as.Date(sub(".+time\":\"([0-9\\-]+).+", "\\1", doc))

      # list company
      listcomp = data.table(dt = doc)[
        , strsplit(sub(".+\\[\\{(.+)\\}\\].+", "\\1", dt), "\\},\\{")
      ][, c("eps", "exchange_rate", "financedata.net_profit", "financedata.totalturnover_", "high", "low", "market_capital", "name", "open", "pe", "percent", "price", "symbol", "turnover", "updown", "volume", "yestclose", "zf", "no") := tstrsplit(V1, ",", fixed=TRUE)
      ][, lapply(.SD, function(x) gsub("\".+\"\\:|\\}|\"", "", x))
      ][, `:=`(
        V1 = NULL, date = datetime, board = mkt_board,
        name = stri_unescape_unicode(name),
        market = "stock", exchange = "hkex"
      )][, c("market", "exchange", "board", "symbol", "name", "date", "open", "high", "low", "price", "percent", "updown", "volume", "turnover", "exchange_rate", "zf", "pe", "market_capital", "eps", "financedata.net_profit", "financedata.totalturnover_"), with=FALSE]

      # # string to number
      # cols_str_to_num = c("eps", "exchange_rate", "financedata.net_profit", "financedata.totalturnover_", "high", "low", "market_capital", "open", "pe", "percent", "price", "turnover", "updown", "volume", "yestclose", "zf", "no")
      # listcomp[, (cols_str_to_num) := lapply(.SD, as.numeric), .SDcols = cols_str_to_num]
      return(listcomp)
    }


    urls = c(
      # main
      main = "http://quotes.money.163.com/hk/service/hkrank.php?host=/hk/service/hkrank.php&page=0&query=CATEGORY:MAIN;TYPE:1;EXCHANGE_RATE:_exists_true&fields=no,time,SYMBOL,NAME,PRICE,PERCENT,UPDOWN,OPEN,YESTCLOSE,HIGH,LOW,VOLUME,TURNOVER,EXCHANGE_RATE,ZF,PE,MARKET_CAPITAL,EPS,FINANCEDATA.NET_PROFIT,FINANCEDATA.TOTALTURNOVER_&sort=SYMBOL&order=desc&count=300000&type=query&callback=callback_1620346970&req=12219",
      # GEM
      gem = "http://quotes.money.163.com/hk/service/hkrank.php?host=/hk/service/hkrank.php&page=0&query=CATEGORY:GEM;TYPE:1;EXCHANGE_RATE:_exists_true&fields=no,SYMBOL,NAME,PRICE,PERCENT,UPDOWN,OPEN,YESTCLOSE,HIGH,LOW,VOLUME,TURNOVER,EXCHANGE_RATE,ZF,PE,MARKET_CAPITAL,EPS,FINANCEDATA.NET_PROFIT,FINANCEDATA.TOTALTURNOVER_&sort=SYMBOL&order=desc&count=20000&type=query&callback=callback_2057363736&req=12220"
    )


    # hongkong stock list
    stock_list_hk = mapply(fun_listcomp_hk, urls, names(urls), SIMPLIFY = FALSE)
    stock_list_hk = rbindlist(stock_list_hk)

  } else if (source=="sina") {
    url_hk_sina <- c(
      "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHKStockData?page=1&num=100000&sort=symbol&asc=1&node=qbgg_hk&_s_r_a=init"
    )
    stock_list_hk = setDT(fromJSON(url_hk_sina))[, `:=`(
        market = "stock", exchange = "hkex",
        board = ifelse(substr(symbol,1,2)=="08", "gem", "main")
    )] 
    setnames(stock_list_hk, c('engname', 'lasttrade', 'prevclose', 'pricechange', 'changepercent', 'market_value', 'pe_ratio'), c('name_eng', 'close', 'close_prev', 'change', 'change_pct', 'cap_total', 'pe'))
  } else if (source == 'eastmoney') {
        url_hk_em = "http://72.push2.eastmoney.com/api/qt/clist/get?pn=1&pz=5000&po=1&np=1&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=m:128%20t:1&fields=f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f24,f25,f22,f11,f62,f128,f136,f115,f152&_=1624010056945"
        # %20t:3,m:128%20t:4,m:128%20t:1,m:128%20t:2
    }


  stock_list_hk = stock_list_hk[,`:=`(
      symbol = paste0(symbol, '.HK'),
      date = as_date(ticktime)
  )][, c('exchange', 'market', 'board', 'symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'close_prev', 'volume', 'amount', 'change_pct',  'cap_total', 'eps', 'dividend', 'pe', 'name_eng', 'ticktime')]
  if (return_price == FALSE) stock_list_hk = stock_list_hk[, c("market", "exchange", "board", "symbol", "name"), with=FALSE]
  # data("market_symbols", envir = environment())
  # stock_list_hk2 = rbindlist(
  #   list(setDT(market_symbols)[exchange=="hkex"], stock_list_hk))[, .SD[1], by=symbol]

  return(stock_list_hk)
}

# shse, szse ------
#' @import data.table 
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_unescape_unicode
md_stockall_real_163 = function(symbol = c('a','index'), only_symbol = FALSE, show_tags = FALSE, to_sysdata=FALSE, ...) {
    prov = tags = market = exchange = time = . = submarket = region = board = name = mkt = indu = sec = NULL
    
    fun_stock_163 = function(urli, mkt) {
        code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = hs = volume = turnover = mcap = tcap = pe = mfsum = net_income = revenue = plate_ids = time = NULL
        # stock
        # c('code', 'five_minute' 'high', 'hs', 'lb', 'low', 'mcap', 'mfratio', 'mfsum', 'name', 'open', 'pe', 'percent', 'plate_ids', 'price', 'sname', 'symbol', 'tcap', 'turnover', 'updown', 'volume', 'wb', 'yestclose', 'zf', 'no', 'announmt', 'uvsnews')
        # index
        # c('code', 'high', 'low', 'name', 'open' 'percent', 'price', 'symbol', 'time', 'turnover' 'updown', 'volume', 'yestclose', 'no', 'zhenfu') 
        data_date = md_stock_real_tx('^000001')$date
        jsonDat = fromJSON(urli)
        
        jsonDF = jsonDat$list
        if (mkt == 'stock') {
            jsonDF$net_income = jsonDF$MFRATIO$MFRATIO2
            jsonDF$revenue = jsonDF$MFRATIO$MFRATIO10
            jsonDF[,c('MFRATIO', 'UVSNEWS','ANNOUNMT','NO')] = NULL 
            names(jsonDF) = tolower(names(jsonDF))
            
            jsonDF = setDT(jsonDF)[,`:=`(
                date = data_date, #as.Date(substr(jsonDat$time,1,10)), 
                time = jsonDat$time#,
                #strptime(jsonDat$time, '%Y-%m-%d %H:%M:%S', tz = 'Asia/Shanghai')
            )][, .(symbol, name, date, open, high, low, close=price, close_prev=yestclose, change=updown, change_pct=percent*100, volume, amount=turnover, turnover=hs*100, cap_market=mcap, cap_total=tcap, pe_last=pe, eps=mfsum, net_income, revenue, plate_ids, time=as.POSIXct(time))
            ][,`:=`(
                province = sub('.*(dy\\d+).*', '\\1', plate_ids),
                plate_ids = sub('dy\\d+','',plate_ids)
            )][,`:=`(
                sector = sub('.*((hy\\d{3}0{3})|hy012001).*', '\\1', plate_ids),
                industry = sub('.*(hy\\d+).*', '\\1', sub('hy\\d{3}0{3}','',plate_ids))
            )]
            
        } else if (mkt == 'index') {
            names(jsonDF) = tolower(names(jsonDF))
            
            jsonDF = setDT(jsonDF)[,`:=`(
                date = data_date#as.Date(substr(jsonDat$time,1,10))
            )][, .(symbol, name, date, open, high, low, close=price, close_prev=yestclose, change=updown, change_pct=percent*100, volume=volume/100, amount=turnover, time=as.POSIXct(time))]
        }
        
        return(jsonDF[, `:=`(market = mkt, region = 'cn')])
    }
    
    urls_163 = list(
        a = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=CODE&order=desc&count=100000&type=query', 
        b = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=100000&type=query',
        index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query',
        index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query'
    )
    idx = which(names(urls_163) %in% symbol) #unlist(strsplit(symbol,','))
    
    df_stock_cn = try(
        rbindlist(
            mapply(fun_stock_163, urls_163[idx], c('stock','stock','index','index')[idx], SIMPLIFY = FALSE), 
            fill = TRUE
        ), 
        silent = TRUE
    )#, idcol = 'mkt')#[mkt %in% c('a','b'), mkt := 'stock']
    if (!inherits(df_stock_cn, 'try-error') & 'fund' %in% symbol) df_stock_cn = rbind(df_stock_cn, md_fundall_real_163(), fill=TRUE)
    
    # date time of download
    datetime = gsub('[^(0-9)]','',df_stock_cn[1,time])
    # if (df_stock_cn[1,time] < as.POSIXct(paste(df_stock_cn[1,date], '15:00:00'))) 
    #   cat('The close price is real price at', as.character(datetime), '\n')
    
    # create/export sysdata.rda 
    if (to_sysdata) return(df_stock_cn)
    # create/export symbol only or tags
    if (only_symbol || show_tags) {
        if (inherits(df_stock_cn, 'try-error')) df_stock_cn = setDT(copy(symbol_stock_163))
        
        df_stock_cn = symbol_163_format(df_stock_cn)
        if ('prov' %in% names(df_stock_cn)) df_stock_cn = df_stock_cn[is.na(prov), prov := stri_unescape_unicode('\\u91cd\\u5e86')]
        
        if (only_symbol & show_tags) {
            df_stock_cn = df_stock_cn[
                , .(market, submarket, region, exchange, board, symbol, name, prov, sec, indu)
            ][order(-market, exchange, symbol)]
        } else if (only_symbol) {
            df_stock_cn = df_stock_cn[
                , .(market, submarket, region, exchange, board, symbol, name)
            ][order(-market, exchange, symbol)]
        }
    } else {
        if (!identical(symbol, 'index')) {
            cols_rm = intersect(names(df_stock_cn), c('eps', 'net_income', 'revenue'))
            if (length(cols_rm)>0) df_stock_cn = df_stock_cn[, (cols_rm) := NULL]
        }
    }
    
    df = df_stock_cn[,unit := 'CNY'][, symbol := check_symbol_for_yahoo(symbol, market)]#[, mkt := NULL][]
    
    cols_rm = intersect(names(df), c('sector', 'industry', 'province', 'plate_ids', 'region')) # , 'close_prev', 
    if (length(cols_rm)>0) df = df[, (cols_rm) := NULL]
    return(df)
}


symbol_163_format = function(df_symbol) {
  type = . = id = name = symbol = tags = market = submarket = region = exchange = board = prov = indu = sec = mkt = syb  = syb3 = NULL
  
  # merge jsonDF with prov_indu_163
  prov_indu_163 = setDT(copy(prov_indu_163))
  
  if (all(c('province', 'industry', 'sector') %in% names(df_symbol))) {
    df_symbol = merge(df_symbol, prov_indu_163[type=="prov",.(province=id, prov=name)], by = "province", all.x = TRUE)
    df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(industry=id, indu=name)], by = "industry", all.x = TRUE)
    df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(sector=id, sec=name)], by = "sector", all.x = TRUE)
  }
  
  
  if (!('market' %in% names(df_symbol))) {
    df_symbol = copy(df_symbol)[, `:=`(market = ifelse(grepl("\\^", symbol), "index", "stock") )]
  }
  df_symbol = merge(
    # symbol list dataframe
    df_symbol[
      , syb := sub(".*?(\\d+).*","\\1", symbol) 
    ][nchar(syb)==6, syb3 := substr(syb,1,3)],
    # tangs by syb3 and market
    tags_dt()[, c("exchange","submarket","board") := tstrsplit(tags,",")
    ][,.(market=mkt, syb3, exchange, submarket, board)],
    # merge by 
    all.x = TRUE, by = c('syb3', 'market')
  )[order(-market, exchange, symbol)
  ][, (c('syb','syb3')) := NULL]
  
  return(df_symbol)
}


#' @import data.table
#' @importFrom jsonlite fromJSON 
md_stock_symbol_163 = function() {
  # . = board = exchange = indu = market = name = prov = sec = submarket = symbol = NULL 
  df_syb = md_stockall_real_163(symbol = c('a', 'b', 'index'), only_symbol=FALSE, show_tags=TRUE)
  setnames(df_syb, c('sec', 'indu', 'prov'), c('sector', 'industry', 'province'))
  # [,.(market, submarket, exchange, board, symbol, name, sector = sec, industry = indu, province = prov)]
  return(df_syb)
}


# us ------
md_stock_symbol_us = function(exchange) {
  . = symbol = name = sector = industry = country = ipoyear = marketCap = NULL
  # c("AMEX", "NASDAQ", "NYSE")
  exchange = toupper(exchange)
  exchange2code = list(NASDAQ = '105', NYSE='106', NYSE = '107')
  
  datem = read_apidata_eastmoney(url = sprintf("http://72.push2.eastmoney.com/api/qt/clist/get?pn=1&pz=20000&po=1&np=1&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=m:%s&fields=f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f24,f25,f26,f22,f33,f11,f62,f128,f136,f115,f152&_=1624010056945", exchange2code[[exchange]]), type = 'real_us')
  dat2 = datem[, c('symbol', 'name', 'open', 'high', 'low', 'close', 'close_prev', 'volume', 'amount', 'change', 'change_pct', 'amplitude', 'turnover', 'pe'), with=FALSE]
  
  if (FALSE) {
      url = sprintf(
          # "https://old.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download",
          # 'http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download', 
          "https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=%s&download=true",
          exchange)
      
      dat = fromJSON(url)
      dat2 = setDT(dat$data$rows)[,.(market='stock', exchange=exchange, board=NA, symbol, name, sector, industry, cap_market=marketCap, ipoyear, country)] 
  }
  
  
  return(dat2)
}


# stock symbol by exchange ------
md_stock_symbol_exchange = function(XCHG=NULL, print_step=1L) {
  exchange = NULL
  
  exchange_list = c('sse','szse', 'hkex', "amex", "nasdaq", "nyse")
  while ((is.null(XCHG) || length(XCHG)==0)) {
    XCHG = select_rows_df(dt = setDT(list(exchange = exchange_list)), column = 'exchange')[,exchange]
  }
  XCHG = intersect(tolower(XCHG), exchange_list)
  
  
  i = 1
  exc_len = length(XCHG)
  dat_lst = NULL
  if (any(c("sse", "szse") %in% XCHG)) {
    temp = md_stock_symbol_163()[exchange %in% XCHG]
    exc = intersect(c("sse", "szse"),XCHG)
    for (e in exc) {
      if ((print_step > 0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, exc_len)), collapse = '/'), e))
      i = i+1
      dat_lst[[e]] = temp[exchange == e]
    }
    
  }
  if (any("hkex" %in% XCHG)) {
    exc = intersect(c('hkex'),XCHG)
    for (e in exc) {
      if ((print_step > 0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, exc_len)), collapse = '/'), e))
      i = i+1
      dat_lst[[e]] = md_stock_symbol_hk()
    }
  }
  if (any(c("amex", "nasdaq", "nyse") %in% XCHG)) {
    exc = intersect(c("amex", "nasdaq", "nyse"),XCHG)
    for (e in exc) {
      if ((print_step > 0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, exc_len)), collapse = '/'), e))
      i = i+1
      dat_lst[[e]] = md_stock_symbol_us(e)
    }
  }
  # dat_lst = rbindlist(dat_lst, fill = TRUE)
  return(dat_lst)
}


# stock symbol constituent of index ------
# China securities index, csindex

# query constituent of securities index
# 
# \code{md_index_cons} provides an interface to query the current constituent of securities index. 
# 
# @param symbol the symbol of securities index. It supports Chinese securities index only at this moment.
# 
# @source \url{http://www.csindex.com.cn/zh-CN}
# 
# @examples 
# \donttest{
# dt50 = md_index_cons("000016")
# 
# dt300 = md_index_cons("000300")
# 
# dt500 = md_index_cons("000905")
# }
# 
# @import data.table
# @export
# 
stk_syb_idx1 = function(syb) {
  exchange = NULL
  
  url = sprintf("http://www.csindex.com.cn/uploads/file/autofile/cons/%scons.xls",syb)
  dat = load_read_xl(url)
  setDT(dat)
  setnames(dat, c("date","index_symbol","index_name","index_name_en","stock_symbol","stock_name", "stock_name_en","exchange"))
  dat = dat[, exchange := ifelse(exchange=="SHH","sse", ifelse(exchange=="SHZ","szse", exchange))]
  # cat(sprintf("For more detials go to:\nhttp://www.csindex.com.cn/zh-CN/indices/index-detail/%s\n", symbol))
  return(dat)
}
md_stock_symbol_index = function(symbol, print_step=1L) {
  dat_list = load_dat_loop(symbol, "stk_syb_idx1", args = list(), print_step=print_step)
  return(dat_list)
}


# 'http://www.sse.com.cn/assortment/stock/list/share/'
# 'http://www.szse.cn/market/stock/list/index.html'
# @param index the stock index symbol provided by China Securities Index Co.Ltd (\url{http://www.csindex.com.cn}).
# get stock components of a stock index (only in sse and szse)
# index_syb = md_stock_symbol(index = c('000001', '000016', '000300', '000905'))

#' symbol components of exchange
#' 
#' \code{md_stock_symbol} returns all stock symbols by exchange
#' 
#' @param exchange the available stock exchanges are sse, szse, hkex, amex, nasdaq, nyse.
#' 
#' @examples 
#' \dontrun{
#' # get stock symbols in a stock exchange
#' ## specify the exchanges
#' ex_syb1 = md_stock_symbol(exchange = c('sse', 'szse'))
#' 
#' ## choose exchanges interactivly
#' ex_syb2 = md_stock_symbol()
#' 
#' }
#' 
#' @export
md_stock_symbol = function(exchange=NULL) {
    md_stock_symbol_exchange(exchange)
}

