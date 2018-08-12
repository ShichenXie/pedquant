# get china stock symbol list from cninfo.com.cn
# @import RSelenium rvest data.table
sym_stock_cn_cninfo = function(return_url=FALSE) {
  code_name = submarket = type = . = board = delist_date = exchange = name = read_html = remoteDriver = suspend_date = symbol = NULL
  html_nodes = `%>%` = html_text = html_attr = NULL
  # RSelenium ref:
  # [Selenium](http://www.seleniumhq.org)
  # [Selenium with Python](https://selenium-python.readthedocs.io/installation.html)
  # [RSelenium: Basics](https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html)
  
  # docker toolbox ref: 
  # https://docs.docker.com/docker-for-mac/docker-toolbox/
  # open docker via <Docker Q..t Terminal> in launchpad
  # docker run hello-world
  # docker run -d -p 4445:4444 selenium/standalone-firefox
  # docker ps
  # sudo docker stop $(docker ps -q)
  
  # comapny list ------
  # java -jar selenium-server-standalone-3.8.1.jar
  # http://selenium-release.storage.googleapis.com/index.html
  ssrunning = menu(c("Yes", "No"), 
    title="Is selenium server running? 
      \nIf not, download it from 'http://selenium-release.storage.googleapis.com/index.html' and run 'java -jar selenium-server-standalone-3.8.1.jar' in OS command. 
      \nDetails see 'https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html'")
  
  if (ssrunning == 2) stop("selenium server is not running")

  remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")
  
  # via a docker container
  # remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L)
  remDr$open(silent = TRUE)
  
  # navigate
  remDr$navigate("http://www.cninfo.com.cn/cninfo-new/information/companylist")
  Sys.sleep(sample(5))
  
  wb = read_html(remDr$getPageSource()[[1]])
  dt_list = lapply(
    lapply(paste0("div #con-a-",1:6," ul li a"), function(x) html_nodes(wb, x)),
    function(x) {data.table(
      code_name = x %>% html_text(),
      url = x %>% html_attr("href")
    )}
  )
  
  dt_list = rbindlist(dt_list)[,`:=`(
    symbol = sub("^([0-9]+) (.+)$", "\\1", code_name), 
    name = gsub(" ","",sub("^([0-9]+) (.+)$", "\\2", code_name)),
    code_name = NULL
  )][, c("exchange","submarket","board") := tstrsplit(mapply(tags_symbol_stockcn, symbol, "stock"), ",")]
  
    
  
  # delisting ------
  # navigate
  remDr$navigate("http://www.cninfo.com.cn/cninfo-new/information/delistinglist")
  # delist szse
  Sys.sleep(sample(5))
  webElem <- remDr$findElement(using = "id", value = "a1")
  webElem$clickElement()
  wb2 = read_html(remDr$getPageSource()[[1]])
  # delist sse
  Sys.sleep(sample(5))
  webElem <- remDr$findElement(using = "id", value = "a2")
  webElem$clickElement()
  wb3 = read_html(remDr$getPageSource()[[1]])
  
  dt_delist = lapply(
    list(wb2, wb3), 
    function(x) {data.table(
      symbol = x %>% html_nodes("div .t1") %>% html_text(),
      name = x %>% html_nodes("div .t2") %>% html_text(),
      delist_date = x %>% html_nodes("div .t3") %>% html_text()
    )[-1]}
  )
  dt_delist = rbindlist(dt_delist)[
    , c("exchange","submarket","board") := tstrsplit(mapply(tags_symbol_stockcn, symbol, "stock"), ",")
  ][, name := gsub(" ", "", name)]
  # close remDr
  # remDr$close()
  
  # suspend list ------
  # navigate
  remDr$navigate("http://www.cninfo.com.cn/cninfo-new/information/suspendlist")
  # szse
  Sys.sleep(2)
  webElem <- remDr$findElement(using = "id", value = "a1")
  webElem$clickElement()
  wb4 = read_html(remDr$getPageSource()[[1]])
  # sse
  Sys.sleep(1)
  webElem <- remDr$findElement(using = "id", value = "a2")
  webElem$clickElement()
  wb5 = read_html(remDr$getPageSource()[[1]])
  
  dt_suspend = lapply(
    list(wb4, wb5), 
    function(x) {data.table(
      symbol = x %>% html_nodes("div .t1") %>% html_text(),
      name = x %>% html_nodes("div .t2") %>% html_text(),
      suspend_date = x %>% html_nodes("div .t4") %>% html_text() )[-1]}
  )
  dt_suspend = rbindlist(dt_suspend)[
    , c("exchange","submarket","board") := tstrsplit(mapply(tags_symbol_stockcn, symbol, "stock"), ",")
    ][, name := gsub(" ", "", name)]
  
  # close remDr
  remDr$close()
  
  # symbols of china stock
  symbols_stock_cn = rbindlist(
    list(list = dt_list, delist = dt_delist, suspend = dt_suspend), fill = TRUE, idcol = "type"
  )[, .(market="stock", submarket, region="cn", exchange, board, symbol, name, url, delist_date, suspend_date, type)]
  
  if (return_url==FALSE) 
    symbols_stock_cn = symbols_stock_cn[, c("url", "suspend_date", "delist_date") := NULL]
  
  
  return(symbols_stock_cn)
}
# library(RSelenium)
# library(rvest)
# library(data.table)
# market_symbols = setDF(sym_stock_cn_cninfo())
# save(market_symbols, file = "./data/market_symbols.RData")


#' @import data.table
#' @importFrom stringi stri_unescape_unicode
getmd_stock_symbol_hk = function(source="163", return_price=FALSE) {
  symbol = exchange = market_symbols = stock_list_hk2 = NULL

  if (source == "163") {
    # function
    fun_listcomp_hk = function(urli, mkt_board) {
      dt = V1 = stri_unescape_unicode = name = NULL

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
        market = "stock", region = "hk", exchange = "hkex"
      )][, c("market", "region", "exchange", "board", "symbol", "name", "date", "open", "high", "low", "price", "percent", "updown", "volume", "turnover", "exchange_rate", "zf", "pe", "market_capital", "eps", "financedata.net_profit", "financedata.totalturnover_"), with=FALSE]

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
        market = "stock", region = "hk", exchange = "hkex",
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
  if (return_price == FALSE) stock_list_hk = stock_list_hk[, c("market", "region", "exchange", "board", "symbol", "name"), with=FALSE]
  # data("market_symbols", envir = environment())
  # stock_list_hk2 = rbindlist(
  #   list(setDT(market_symbols)[exchange=="hkex"], stock_list_hk))[, .SD[1], by=symbol]

  return(stock_list_hk2)
}

#' @import data.table
#' @importFrom jsonlite fromJSON 
getmd_stock_symbol_163 = function() {
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
  
  df_symbol = try(rbindlist(mapply(fun_symbol_163, symbol_url_163, c("stock","stock","index","index"), SIMPLIFY = FALSE), fill = TRUE), silent = TRUE)
  if ('try-error' %in% class(df_symbol)) {
    df_symbol = setDT(copy(symbol_stock_163))
  }
  
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

# get stock and index symbols
# 
# \code{getmd_163_symbol} gets the stock and index symbols in sse (Shanghai Stock Exchange), szse (Shenzhen Stock Exchange) and hkex (Hong Kong Stock Exchange). 
#
# @param exchanges name of stock exchange. Accepted values including sse, szse and hkex. Default is c("sse", "szse").
getmd_symbol_163 = function(exchanges = c("sse", "szse")) {
  exchange = region = syb = NULL
  
  if (any(c("sse", "szse") %in% exchanges)) {
    syb = getmd_stock_symbol_163()[exchange %in% exchanges]
    
  } else if (any("hk" %in% region)) {
    syb = rbindlist(syb, getmd_stock_symbol_hk(), fill = TRUE)
  }
  
  return(syb)
}