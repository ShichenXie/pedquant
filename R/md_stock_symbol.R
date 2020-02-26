# hk ------
#' @import data.table
#' @importFrom stringi stri_unescape_unicode
md_stock_symbol_hk = function(source="163", return_price=FALSE) {
  symbol = exchange = market_symbols = stock_list_hk2 = NULL

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
    fun_listcomp_hk_sina = function(urli) {
      num = doc = V1 = symbol = NULL

      dt = data.table(
        doc = readLines(urli, warn = FALSE)
      )[, doc := iconv(doc, "GB18030", "UTF-8")
      ][, doc := gsub("(\\[\\{)|(\\]\\})", "", doc)
      ][, strsplit(doc, "\\},\\{")
      ][, (c("symbol","name","engname","tradetype","lasttrade","prevclose","open","high","low","volume","currentvolume","amount","ticktime","buy","sell","high_52week","low_52week","eps","dividend","stocks_sum","pricechange","changepercent")) := tstrsplit(V1, ",", fixed=TRUE)
      ][, V1 := NULL
      ][, lapply(.SD, function(x) gsub(".+:|\"", "", x))
      ][, `:=`(
        market = "stock", exchange = "hkex",
        board = ifelse(substr(symbol,1,2)=="08", "gem", "main")
      )]

      return(dt)
    }


    url_hk <- c(
      "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHKStockData?page=1&num=100000&sort=symbol&asc=1&node=qbgg_hk&_s_r_a=init"
    )
    stock_list_hk = fun_listcomp_hk_sina(url_hk)
    }


  stock_list_hk = stock_list_hk[, symbol := paste(symbol, substr(exchange,1,2) ,sep=".")]
  if (return_price == FALSE) stock_list_hk = stock_list_hk[, c("market", "exchange", "board", "symbol", "name"), with=FALSE]
  # data("market_symbols", envir = environment())
  # stock_list_hk2 = rbindlist(
  #   list(setDT(market_symbols)[exchange=="hkex"], stock_list_hk))[, .SD[1], by=symbol]

  return(stock_list_hk)
}

# 163 ------
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
  . = board = exchange = indu = market = name = prov = sec = submarket = symbol = NULL 
  df_syb = md_stock_spotall_163(symbol = c('a', 'b', 'index'), only_symbol=TRUE, show_tags=TRUE)[,.(
    market, submarket, exchange, board, symbol, name, sector = sec, industry = indu, province = prov
  )]
  return(df_syb)
}

# nasdaq ------
md_stock_symbol_nasdaq = function(exchange) {
  .=Symbol=Name=Sector=industry=NULL
  
  # c("AMEX", "NASDAQ", "NYSE")
  url = sprintf(
    "https://old.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download",
    # 'http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download', 
    exchange)
  
  dat = load_read_csv(url)[,.(market='stock', exchange=exchange, board=NA, symbol=Symbol, name=Name, sector=Sector, industry)]
  dat[dat=='n/a'] <- NA
  
  return(dat)
}










# exchange index ------
# stock symbol in exchange
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
      dat_lst[[e]] = md_stock_symbol_nasdaq(e)
    }
  }
  # dat_lst = rbindlist(dat_lst, fill = TRUE)
  return(dat_lst)
}



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

#' symbol components of exchange or index
#' 
#' \code{md_stock_symbol} returns all stock symbols of stock exchange or index.
#' 
#' @param exchange the available stock exchanges are sse, szse, hkex, amex, nasdaq, nyse.
#' @param index the stock index symbol provided by China Securities Index Co.Ltd (\url{http://www.csindex.com.cn}).
#' 
#' @examples 
#' \donttest{
#' # get stock symbols in a stock exchange
#' ## specify the name of exchange
#' ex_syb1 = md_stock_symbol(exchange = c('sse', 'szse'))
#' 
#' ## choose stock exchanges interactivly
#' ex_syb2 = md_stock_symbol()
#' 
#' 
#' # get stock components of a stock index (only in sse and szse)
#' index_syb = md_stock_symbol(index = c('000001', '000016', '000300', '000905'))
#' 
#' }
#' 
#' @export
md_stock_symbol = function(exchange=NULL, index=NULL) {
  if (is.null(index)) {
    dt = md_stock_symbol_exchange(exchange)
  } else {
    dt = md_stock_symbol_index(index)
  }
  return(dt)
}

