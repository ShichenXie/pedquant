
#' @import data.table 
#' @importFrom jsonlite fromJSON
getmd_stock_spotall_163 = function(symbol = "a,index", only_symbol = FALSE) {
  tags = market = exchange = time = . = submarket = region = board = name = NULL
    
  fun_stock_163 = function(urli, mkt) {
    code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = hs = volume = turnover = mcap = tcap = pe = mfsum = net_income = revenue = plate_ids = time = NULL
    # stock
    # c("code", "five_minute" "high", "hs", "lb", "low", "mcap", "mfratio", "mfsum", "name", "open", "pe", "percent", "plate_ids", "price", "sname", "symbol", "tcap", "turnover", "updown", "volume", "wb", "yestclose", "zf", "no", "announmt", "uvsnews")
    # index
    # c("code", "high", "low", "name", "open" "percent", "price", "symbol", "time", "turnover" "updown", "volume", "yestclose", "no", "zhenfu") 
    
    jsonDat = fromJSON(urli)
    
    jsonDF = jsonDat$list
    if (mkt == "stock") {
      jsonDF$net_income = jsonDF$MFRATIO$MFRATIO2
      jsonDF$revenue = jsonDF$MFRATIO$MFRATIO10
      jsonDF[,c("MFRATIO", "UVSNEWS","ANNOUNMT","NO")] = NULL 
      names(jsonDF) = tolower(names(jsonDF))
      
      jsonDF = setDT(jsonDF)[,`:=`(
        date = as.Date(substr(jsonDat$time,1,10)), 
        time = jsonDat$time#,
        #strptime(jsonDat$time, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")
      )][, .(date, symbol, name, open, high, low, close=price, prev_close=yestclose, change=updown, change_pct=percent, turnover=hs, volume, amount=turnover, cap_market=mcap, cap_total=tcap, pe, eps=mfsum, net_income, revenue, plate_ids, time)]
    } else if (mkt == "index") {
      names(jsonDF) = tolower(names(jsonDF))
      
      jsonDF = setDT(jsonDF)[,`:=`(
        date = as.Date(substr(jsonDat$time,1,10))
      )][, .(date, symbol, name, open, high, low, close=price, prev_close=yestclose, change=updown, change_pct=percent, volume, amount=turnover, time)]
    }
    
    return(jsonDF[, `:=`(market = mkt, region = "cn")])
  }
  
  urls_163 = list(
    a = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=CODE&order=desc&count=100000&type=query", 
    b = "http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=100000&type=query",
    index = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query",
    index = "http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query"
  )
  idx = which(names(urls_163) %in% unlist(strsplit(symbol,",")))
  
  df_stock_cn = rbindlist(mapply(
    fun_stock_163, urls_163[idx], c("stock","stock","index","index")[idx], SIMPLIFY = FALSE
  ), fill = TRUE
  )[, tags := mapply(tags_symbol_stockcn, symbol, market)
  ][, c("exchange","submarket","board"):=tstrsplit(tags,",")
  ][, tags := NULL][order(-market, exchange, symbol)]
  
  datetime = gsub("[^(0-9)]","",df_stock_cn[1,time])
  if (datetime < paste0(substr(datetime,1,8), '150000')) cat("The close price in returned dataframe is spot price at", datetime, "\n")
  
  if (only_symbol) df_stock_cn = df_stock_cn[, .(market, submarket, region, exchange, board, symbol, name)]
  
  return(df_stock_cn)
}
# @import data.table
getmd_stockall_sina = function(symbol = "a,index", only_symbol = FALSE) {
  tags=market=exchange=.=submarket=region=board=name=NULL
  
  fun_stock_sina = function(urli, mkt) {
    V1=.=code=name=high=low=trade=settlement=pricechange=changepercent=turnoverratio=volume=amount=mktcap=nmc=pb=per=ticktime= NULL
    
    dt_list = list()
    doc = "init"
    num = 1
    while (doc != "null") {
      doc = readLines(sprintf(urli, num), warn = FALSE)
      
      if (doc != "null") {
        dt = data.table(
          doc = doc
        )[, doc := iconv(doc, "GB18030", "UTF-8")
          ][, doc := gsub("(\\[\\{)|(\\]\\})", "", doc)
            ][, strsplit(doc, "\\},\\{")]
        
        if (mkt == "stock") {
          dt = dt[
            , (c("symbol", "code", "name", "trade", "pricechange", "changepercent", "buy", "sell", "settlement", "open", "high", "low", "volume", "amount", "ticktime", "per", "pb", "mktcap", "nmc", "turnoverratio")) := tstrsplit(V1, ",", fixed=TRUE)
            ][, V1 := NULL
              ][, lapply(.SD, function(x) gsub(".+:|\"", "", x))
                ][,.(symbol=code, name, open, high, low, close=trade, prev_close=settlement, change=pricechange, change_pct=changepercent, turnover=turnoverratio, volume, amount, cap_market=mktcap, cap_total=nmc, pb, pe=per, time=ticktime)] 
        } else {
          dt = dt[
            , (c("symbol","name","trade","pricechange","changepercent","buy","sell","settlement","open","high","low","volume","amount","code","ticktime")) := tstrsplit(V1, ",", fixed=TRUE)
            ][, V1 := NULL
              ][, lapply(.SD, function(x) gsub(".+:|\"", "", x))
                ][,.(symbol=code, name, open, high, low, close=trade, prev_close=settlement, change=pricechange, change_pct=changepercent, volume, amount, time=ticktime)] 
        }
        
        dt_list[[num]] = dt
        num=num+1
      }
      # Sys.sleep(1)
    }
    
    return(rbindlist(dt_list)[,`:=`(market = mkt, region = "cn")])
  }
  
  
  urls_sina <- c(
    a = "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeData?page=%s&num=100&sort=symbol&asc=1&node=hs_a&symbol=&_s_r_a=init",
    b = "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeData?page=%s&num=100&sort=symbol&asc=1&node=hs_b&symbol=&_s_r_a=page",
    index = "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeDataSimple?page=%s&num=100&sort=symbol&asc=1&node=hs_s&_s_r_a=auto"
  )
  idx = which(names(urls_sina) %in% unlist(strsplit(symbol,",")))
  
  df_stock_cn = rbindlist(
    mapply(fun_stock_sina, urls_sina[idx], c("stock", "stock", "index")[idx], SIMPLIFY = FALSE), fill=TRUE
  )[, tags := mapply(tags_symbol_stockcn, symbol, market)
    ][, c("exchange","submarket","board"):=tstrsplit(tags,",")
      ][, `:=`(tags = NULL)][order(-market, exchange, symbol)]
  
  if (only_symbol) df_stock_cn = df_stock_cn[, .(market, submarket, region, exchange, board, symbol, name)]
  
  return(df_stock_cn)
}


# get spot data from tx
getmd_stock_spot1_tx = function(symbol) {
  dat = doc = . = name = high = low = prev_close = change = change_pct = volume = amount = turnover = cap_market = cap_total = pb = pe_last = pe_trailing = pe_forward = buy = sell = bid1 = bid1_volume = bid2 = bid2_volume = bid3 = bid3_volume = bid4 = bid4_volume = bid5 = bid5_volume = ask1 = ask1_volume = ask2 = ask2_volume = ask3 = ask3_volume = ask4 = ask4_volume = ask5 = ask5_volume = NULL
  
  symbol = paste0(sapply(symbol, check_symbol_for_tx),collapse=",")
  
  dt = readLines(sprintf("http://qt.gtimg.cn/q=%s", symbol))
  # ff_ 资金流量 # s_pk 盘口 # s_ 简要信息
  
  dt = data.table(
    doc = dt
  )[, doc := iconv(doc, "GB18030", "UTF-8")
    ][, doc := sub(".+=\"\\d+~(.+)\".+", "\\1", doc)
      ][, tstrsplit(doc, "~")]
  
  # colnames_cn = c("名字", "代码", "当前价格", "昨收", "今开", 
  #   "成交量（手）", "外盘", "内盘", 
  #   "买一", "买一量（手）", "买二","买二","买三","买三","买四","买四","买五","买五", 
  #   "卖一", "卖一量", "卖二","卖二","卖三","卖三","卖四","卖四","卖五","卖五", 
  #   "最近逐笔成交", "时间", "涨跌", "涨跌%", "最高", "最低", 
  #   "价格/成交量(手)/成交额", "成交量(手)", "成交额(万)", "换手率", 
  #   "市盈率(TTM)", "", "最高", "最低", "振幅", "流通市值", "总市值", "市净率", "涨停价", "跌停价", "量比", "", "均价", "市盈率(动)", "市盈率(静)")
  
  colnames_en = c("name", "symbol", "close", "prev_close", "open",
                  "volume", "buy", "sell", 
                  "bid1", "bid1_volume", "bid2", "bid2_volume", "bid3", "bid3_volume", "bid4", "bid4_volume", "bid5", "bid5_volume",
                  "ask1", "ask1_volume", "ask2", "ask2_volume", "ask3", "ask3_volume", "ask4", "ask4_volume", "ask5", "ask5_volume",
                  "last_trade", "date", "change", "change_pct", "high", "low", 
                  "", "volume", "amount", "turnover", 
                  "pe_trailing", "", "high", "low", "", "cap_market", "cap_total", "pb", "", "", "", "", "average", "pe_forward", "pe_last" )
  setnames(dt, colnames_en)
  
  num_cols = c("open", "high", "low", "close", "prev_close", "change", "change_pct", "volume", "amount", "turnover", "cap_market", "cap_total", "pb", "pe_last", "pe_trailing", "pe_forward")
  dt = dt[,.(
    date, symbol, name, open, high, low, close, prev_close, change, change_pct, volume, amount, turnover, cap_market, cap_total, pb, pe_last, pe_trailing, pe_forward#, 
    #buy, sell, 
    #bid1, bid1_volume, bid2, bid2_volume, bid3, bid3_volume, bid4, bid4_volume, bid5, bid5_volume, 
    #ask1, ask1_volume, ask2, ask2_volume, ask3, ask3_volume, ask4, ask4_volume, ask5, ask5_volume
    )][, (num_cols) := lapply(.SD, as.numeric), .SDcols= num_cols
     ][, `:=`(
       time = strptime(date, format="%Y%m%d%H%H%S", tz="Asia/Shanghai"),
       date = as.Date(date, format="%Y%m%d%H%H%S")
     )]
  
  if (dt[1,date] < paste0(substr(dt[1,date],1,8), '150000')) cat("The close price in returned dataframe is spot price at", dt[1,date], "\n")
  
  return(dt)
}


#' @import data.table
#' @importFrom readr read_csv locale col_date col_character col_double col_integer
getmd_stock_hist1_163 = function(symbol, from="1900-01-01", to=Sys.Date(), fillzero=FALSE) {
  change_pct = NULL
  # http://quotes.money.163.com/service/chddata.html?code=0000001&start=19901219&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # http://quotes.money.163.com/service/chddata.html?code=1399001&start=19910403&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # https://query1.finance.yahoo.com/v7/finance/download/^SSEC?period1=1526631424&period2=1529309824&interval=1d&events=history&crumb=mO08ZCtWRMI
  
  # "http://api.finance.ifeng.com/akmonthly/?code=sh600000&type=last"
  # {'D': 'akdaily', 'W': 'akweekly', 'M': 'akmonthly'}
  
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
  # 成交金额 # VATURNOVER:  amount turnover
  # 总市值   # TCAP:        total market capitalisation
  # 流通市值 # MCAP:        tradable market capitalisation
             # LCLOSE:      last close
   
  
  # download data from 163
  dt = read_csv(
    file=link, locale = locale(encoding = "GBK"), na=c("", "NA", "None"),
    col_types=list(col_date(format = ""), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  # dt = load_read_csv(link, "GBK")
  
  cols_name = c("date", "symbol", "name", "open", "high", "low", "close", "change", "change_pct", "turnover", "volume", "amount", "cap_total", "cap_market")
  setnames(dt, cols_name)
  
  dt = setDT(setDF(dt), key="date")[,`:=`(
    symbol = sub("'","",symbol),
    change_pct = change_pct/100
  )]#[, (cols_name[-c(1:3)]) := lapply(.SD, as.numeric), .SDcols = cols_name[-c(1:3)] ]
  
  # fill zeros in dt
  if (fillzero) {
    cols_name = c("open", "high", "low", "close")
    dt = dt[, (cols_name) := lapply(.SD, fill0), .SDcols = cols_name]
  }
  
  return(dt)
}


#' @import data.table
getmd_163 = function(symbol, from="1900-01-01", to=Sys.Date(), print_step=1L, frequency = "daily", fillzero=FALSE) {
  if (frequency == "spot") {
    if (all(unlist(strsplit(symbol,",")) %in% c('a','b','index'))) {
      return(getmd_stock_spotall_163(symbol))
    } else {
      return(getmd_stock_spot1_tx(symbol))
    }
    
  } else if (frequency == "daily") {
    dat_list = load_dat_loop(symbol, "getmd_stock_hist1_163", args = list(from = from, to = to, fillzero = fillzero), print_step=print_step)
    
    return(dat_list)
    
  }
}


