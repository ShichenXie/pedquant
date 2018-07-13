# get market data # getmd
# - froex
# - bond
# - index
# - stock
# - commodity
# - ETF
# - found

# library(data.table)
# library(jsonlite)
#' @import data.table 
#' @importFrom jsonlite fromJSON
getmd_163_stockall = function() {
  
  fun_stock_163 = function(link) {
    code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = hs = volume = turnover = mcap = tcap = pe = mfsum = net_income = revenue = plate_ids = time = NULL
    # c("code", "five_minute" "high", "hs", "lb", "low", "mcap", "mfratio", "mfsum", "name", "open", "pe", "percent", "plate_ids", "price", "sname", "symbol", "tcap", "turnover", "updown", "volume", "wb", "yestclose", "zf", "no", "announmt", "uvsnews")
    
    jsonDat = fromJSON(link)
    
    jsonDF = jsonDat$list
    jsonDF$net_income = jsonDF$MFRATIO$MFRATIO2
    jsonDF$revenue = jsonDF$MFRATIO$MFRATIO10
    jsonDF[,c("MFRATIO", "UVSNEWS","ANNOUNMT","NO")] = NULL 
    names(jsonDF) = tolower(names(jsonDF))
    
    jsonDF = setDT(jsonDF)[,`:=`(
      date = as.Date(substr(jsonDat$time,1,10)), 
      time = jsonDat$time,
        #strptime(jsonDat$time, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai"), 
      exchange = ifelse(substr(code, 1, 1) == 1, "szse", "sse")
    )][ ,`:=`(
      symbol = paste(symbol, substr(exchange,1,2), sep="."),
      board = ifelse(substr(symbol,1,3)=="002", "sme", 
                     ifelse(substr(symbol,1,3)=="300", "chinext", "main"))
    )][, .(date, symbol, name, open, high, low, close=price, prev_close=yestclose, change=updown, pct_change=percent, turnover=hs, volume, value=turnover, market_cap=mcap, total_cap=tcap, pe, eps=mfsum, net_income, revenue, plate_ids, time)]
    
    return(jsonDF)
  }
  
  stock_url_163 = list(
    A = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=CODE&order=desc&count=100000&type=query", 
    B = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=100000&type=query"
  )
  df_stock_cn = rbindlist(lapply(stock_url_163, fun_stock_163), fill = TRUE)
  
  return(df_stock_cn)
}


#' @import data.table 
#' @importFrom jsonlite fromJSON
getmd_163_indexall = function() {
  code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = volume = turnover = time = NULL
  
  fun_index_163 = function(link) {
    # "code", "high", "low", "name", "open"     
    # "percent", "price", "symbol", "time", "turnover" 
    # "updown", "volume", "yestclose", "no", "zhenfu" 
    jsonDat = fromJSON(link)
    
    jsonDF = jsonDat$list
    names(jsonDF) = tolower(names(jsonDF))
    
    jsonDF = setDT(jsonDF)[,`:=`(
      date = as.Date(substr(jsonDat$time,1,10)), 
      exchange = ifelse(substr(code, 1, 1) == "1", "szse", "sse")
    )][ ,`:=`(
      symbol = paste(symbol, substr(exchange,1,2), sep=".")
    )][, .(date, symbol, name, open, high, low, close=price, prev_close=yestclose, change=updown, pct_change=percent, volume, value=turnover, time)]
    
    return(jsonDF)
  }
  
  # index 1SH 2SZ
  index_url_163 = list(
    sse = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query",
    szse = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query"
  )
  df_index_cn = rbindlist(lapply(index_url_163, fun_index_163), fill = TRUE)
  
  return(df_index_cn)
}


#' @import data.table
#' @importFrom readr read_csv locale col_date col_character col_double col_integer
getmd_163_stock1 = function(symbol, from="1900-01-01", to=Sys.Date(), fillzero=FALSE) {
  pct_change = NULL
  # http://quotes.money.163.com/service/chddata.html?code=0000001&start=19901219&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # http://quotes.money.163.com/service/chddata.html?code=1399001&start=19910403&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # https://query1.finance.yahoo.com/v7/finance/download/^SSEC?period1=1526631424&period2=1529309824&interval=1d&events=history&crumb=mO08ZCtWRMI
  # symbol
  symbol = check_symbol_for_163(symbol)

  # date range
  fromto = lapply(list(from=from,to=to), function(x) format(check_fromto(x), "%Y%m%d"))
  
  # create link
  link = paste0("http://quotes.money.163.com/service/chddata.html?code=",symbol,"&start=",fromto$from,"&end=",fromto$to,"&fields=TOPEN;HIGH;LOW;TCLOSE;CHG;PCHG;TURNOVER;VOTURNOVER;VATURNOVER;TCAP;MCAP")
  # 开盘价   # TOPEN:       open
  # 最高价   # HIGH:        high
  # 最低价   # LOW:         low
  # 收盘价   # TCLOSE:      close
  # 涨跌额   # CHG:         chg
  # 涨跌幅   # PCHG:        chg percent
  # 换手率   # TURNOVER:    turnour
  # 成交量   # VOTURNOVER:  volume turnover
  # 成交金额 # VATURNOVER:  value turnover
  # 总市值   # TCAP:        total market capitalisation
  # 流通市值 # MCAP:        tradable market capitalisation
             # LCLOSE:      last close
   
  
  # download data from 163
  dt = read_csv(
    file=link, locale = locale(encoding = "GBK"), na=c("", "NA", "None"),
    col_types=list(col_date(format = ""), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  # dt = load_read_csv(link, "GBK")
  
  cols_name = c("date", "symbol", "name", "open", "high", "low", "close", "change", "pct_change", "turnover", "volume", "value", "total_cap", "market_cap")
  setnames(dt, cols_name)
  
  dt = setDT(setDF(dt), key="date")[,`:=`(
    symbol = sub("'","",symbol),
    pct_change = pct_change/100
  )]#[, (cols_name[-c(1:3)]) := lapply(.SD, as.numeric), .SDcols = cols_name[-c(1:3)] ]
  
  # fill zeros in dt
  if (fillzero) {
    cols_name = c("open", "high", "low", "close")
    dt = dt[, (cols_name) := lapply(.SD, fill0), .SDcols = cols_name]
  }
  
  return(dt)
}


#' get stock market data from 163
#' 
#' \code{getmd_163} gets Chinese stock and index data from 163, and returns a list of dataframes.
#' 
#' @param symbol symbol of Chinese stock and index in 163, the index symbol should be starts with ^.
#' @param from the start date. Default is '1900-01-01'.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer, which will print variable names by each print_step-th iteration. Default is 1. 
#' @param frequency currently only daily data is available. 
#' @param fillzero logical. Defualt is TRUE. If it is TRUE, the zeros in dataset will be filled with last non-zero values.
#' 
#' @source \url{http://quotes.money.163.com/stock}
#' 
#' @examples 
#' \dontrun{
#' dat = getmd_163(symbol=c('600000', '000001'))
#' 
#' ssec_szsec = getmd_163(symbol = c('^000001', '^399001'))
#' }
#' 
#' @import data.table rvest
#' @export
getmd_163 = function(symbol, from="1900-01-01", to=Sys.Date(), print_step=1, frequency = "daily", fillzero=FALSE) {
  if (any(symbol=="stockall")) {
    return(getmd_163_stockall())
    
  } else if (any(symbol=="indexall")) {
    return(getmd_163_indexall())
    
  } else {
    md_list = NULL
    symbol_len = length(symbol)
    for (i in 1:symbol_len) {
      si = symbol[i]
      # print
      if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i,symbol_len)),collapse = "/"), si,"\n")
      
      md_list[[si]] = getmd_163_stock1(symbol = si, from = from, to = to, fillzero = fillzero)
    }
    return(md_list)
    
  }
}


#' get stock and index symbols from 163
#' 
#' \code{getmd_163_symbol} gets all Chinese stock and index symbols from 163. 
#' 
#' @source \url{http://quotes.money.163.com/stock}
#' 
#' @examples 
#' \dontrun{
#' dt = getmd_163_symbol()}
#' 
#' @import data.table
#' @importFrom jsonlite fromJSON 
#' @export
getmd_163_symbol = function() {
  
    
  fun_symbol_163 = function(link, mkt) {
    plate_ids = . = symbol = name = sec = province = industry = sector = NULL
    # rt = GET(link, add_headers(
    #   'Connection' = 'keep-alive',
    #   'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36',
    #   'Upgrade-Insecure-Requests'='1'
    # ))
    jsonDat = fromJSON(link)
    jsonDF = setDT(jsonDat$list)[,date:=as.Date(jsonDat$time)]
    setnames(jsonDF, sub("\\.", "", tolower(names(jsonDF))) )
    
    # industry sector for stock
    if (mkt == "stock") {
      jsonDF2 = jsonDF[,`:=`(
        province = sub(".*(dy\\d+).*", "\\1", plate_ids),
        plate_ids = sub("dy\\d+","",plate_ids)
      )][,`:=`(
        industry = sub(".*(hy\\d{3}0{3}).*", "\\1", plate_ids),
        sector = sub(".*(hy\\d+).*", "\\1", sub("hy\\d{3}0{3}","",plate_ids))
      )][,.(symbol, name, province, industry, sector)]
    } else {
      jsonDF2 = jsonDF[, symbol := paste0("^",symbol)][,.(symbol, name)]
    }
    
    return(jsonDF2)
  }
  
  
  symbol_url_163 = list(
    A = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CSNAME%2CCODE&sort=CODE&order=desc&count=100000&type=query",
    B = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CSNAME%2CCODE&sort=PERCENT&order=desc&count=100000&type=query",
    ind_sse = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=SYMBOL,NAME&sort=SYMBOL&order=asc&count=10000&type=query",
    ind_szse = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=SYMBOL,NAME&sort=SYMBOL&order=asc&count=10000&type=query"
  )
  mkt_163 = list("stock","stock","index","index")
  
  df_symbol = try(rbindlist(mapply(fun_symbol_163, symbol_url_163, mkt_163, SIMPLIFY = FALSE), fill = TRUE), silent = TRUE)
  if ('try-error' %in% class(df_symbol)) {
    df_symbol = setDT(copy(symbol_name_163))
  }
    
  # Internal data # http://r-pkgs.had.co.nz/data.html
  # devtools::use_data(symbol_name_163, prov_indu_163, internal = TRUE, overwrite = TRUE)
  symbol_163_format = function(df_symbol) {
    type = . = id = name = symbol = tags = market = submarket = region = exchange = board = prov = indu = sec = NULL
    
    # merge jsonDF with prov_indu_163
    prov_indu_163 = setDT(copy(prov_indu_163))
    
    df_symbol = merge(df_symbol, prov_indu_163[type=="prov",.(province=id, prov=name)], by = "province", all.x = TRUE)
    df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(industry=id, indu=name)], by = "industry", all.x = TRUE)
    df_symbol = merge(df_symbol, prov_indu_163[type=="indu",.(sector=id, sec=name)], by = "sector", all.x = TRUE)
    
    df_symbol = copy(df_symbol)[, `:=`(
      market = ifelse(grepl("\\^", symbol), "index", "stock"),
      region = "cn"
    )][, tags:=mapply(tags_symbol_stockcn, symbol, market)
     ][, c("exchange","submarket","board"):=tstrsplit(tags,",")
     ][, .(market, submarket, region, exchange, board, symbol, name, province=prov, industry=indu, sector=sec)
     ][order(-market, exchange, symbol)]
    
    return(df_symbol)
  }
  
  return(symbol_163_format(df_symbol))
}
