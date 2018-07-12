# get china stock symbol list from cninfo.com.cn
# @import RSelenium rvest data.table
sym_stock_cn_cninfo = function(return_url=FALSE) {
  code_name = submarket = type = . = board = delist_date = exchange = name = read_html = remoteDriver = suspend_date = symbol = NULL
  
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


# # @import data.table rvest
# # @importFrom jsonlite fromJSON
# sym_stock_cn = function(source="163", return_price=FALSE) {
#   symbol = exchange = stock_list_cn = NULL
# 
#   if (source=="sina") {
#     fun_listcomp_cn_sina = function(urli) {
#       V1 = symbol = code = dt_list = NULL
# 
#       doc = "init"
#       num = 1
#       while (doc != "null") {
#         doc = readLines(sprintf(urli, num), warn = FALSE)
# 
#         if (doc != "null") {
#           dt = data.table(
#             doc = doc
#           )[, doc := iconv(doc, "GB18030", "UTF-8")
#           ][, doc := gsub("(\\[\\{)|(\\]\\})", "", doc)
#           ][, strsplit(doc, "\\},\\{")
#           ][, (c("symbol", "code", "name", "trade", "pricechange", "changepercent", "buy", "sell", "settlement", "open", "high", "low", "volume", "amount", "ticktime", "per", "pb", "mktcap", "nmc", "turnoverratio")) := tstrsplit(V1, ",", fixed=TRUE)
#           ][, V1 := NULL
#           ][, lapply(.SD, function(x) gsub(".+:|\"", "", x))
#           ][, `:=`(
#             market = "stock", region = "cn",
#             exchange = ifelse(grepl("sh", symbol), "sse", "szse"),
#             board = ifelse(substr(code,1,3)=="002", "sme",
#                            ifelse(substr(code,1,3)=="300", "chinext", "main")),
#             symbol = code
#             )]
# 
#           dt_list[[num]] = dt
#           num=num+1
#         }
#         # Sys.sleep(1)
#       }
# 
#       return(rbindlist(dt_list))
#     }
# 
# 
#     urls <- c(
#       A = "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeData?page=%s&num=100&sort=symbol&asc=1&node=hs_a&symbol=&_s_r_a=init",
#       B = "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeData?page=%s&num=100&sort=symbol&asc=1&node=hs_b&symbol=&_s_r_a=page"
#     )
#     stock_list_cn = sapply(urls, fun_listcomp_cn_sina, simplify=FALSE)#(, , SIMPLIFY = FALSE)
#     stock_list_cn = rbindlist(stock_list_cn)
# 
#   }
# 
# 
#   stock_list_cn = stock_list_cn[, symbol := paste(symbol, substr(exchange,1,2) ,sep=".")]
#   if (return_price == FALSE) stock_list_cn = stock_list_cn[, c("market", "region", "exchange", "board", "symbol", "name"), with=FALSE]
# 
#   return(stock_list_cn)
# }
# 
# 
# # @import data.table
# # @importFrom stringi stri_unescape_unicode
# getmd_symbol_stock_hkse = function(source="163", return_price=FALSE) {
#   symbol = exchange = market_symbols = NULL
# 
#   if (source == "163") {
#     # function
#     fun_listcomp_hk = function(urli, mkt_board) {
#       dt = V1 = stri_unescape_unicode = name = NULL
# 
#       doc = readLines(urli, warn = FALSE)
# 
#       # date
#       datetime = as.Date(sub(".+time\":\"([0-9\\-]+).+", "\\1", doc))
# 
#       # list company
#       listcomp = data.table(dt = doc)[
#         , strsplit(sub(".+\\[\\{(.+)\\}\\].+", "\\1", dt), "\\},\\{")
#       ][, c("eps", "exchange_rate", "financedata.net_profit", "financedata.totalturnover_", "high", "low", "market_capital", "name", "open", "pe", "percent", "price", "symbol", "turnover", "updown", "volume", "yestclose", "zf", "no") := tstrsplit(V1, ",", fixed=TRUE)
#       ][, lapply(.SD, function(x) gsub("\".+\"\\:|\\}|\"", "", x))
#       ][, `:=`(
#         V1 = NULL, date = datetime, board = mkt_board,
#         name = stri_unescape_unicode(name),
#         market = "stock", region = "hk", exchange = "hkex"
#       )][, c("market", "region", "exchange", "board", "symbol", "name", "date", "open", "high", "low", "price", "percent", "updown", "volume", "turnover", "exchange_rate", "zf", "pe", "market_capital", "eps", "financedata.net_profit", "financedata.totalturnover_"), with=FALSE]
# 
#       # # string to number
#       # cols_str_to_num = c("eps", "exchange_rate", "financedata.net_profit", "financedata.totalturnover_", "high", "low", "market_capital", "open", "pe", "percent", "price", "turnover", "updown", "volume", "yestclose", "zf", "no")
#       # listcomp[, (cols_str_to_num) := lapply(.SD, as.numeric), .SDcols = cols_str_to_num]
#       return(listcomp)
#     }
# 
# 
#     urls = c(
#       # main
#       main = "http://quotes.money.163.com/hk/service/hkrank.php?host=/hk/service/hkrank.php&page=0&query=CATEGORY:MAIN;TYPE:1;EXCHANGE_RATE:_exists_true&fields=no,time,SYMBOL,NAME,PRICE,PERCENT,UPDOWN,OPEN,YESTCLOSE,HIGH,LOW,VOLUME,TURNOVER,EXCHANGE_RATE,ZF,PE,MARKET_CAPITAL,EPS,FINANCEDATA.NET_PROFIT,FINANCEDATA.TOTALTURNOVER_&sort=SYMBOL&order=desc&count=300000&type=query&callback=callback_1620346970&req=12219",
#       # GEM
#       gem = "http://quotes.money.163.com/hk/service/hkrank.php?host=/hk/service/hkrank.php&page=0&query=CATEGORY:GEM;TYPE:1;EXCHANGE_RATE:_exists_true&fields=no,SYMBOL,NAME,PRICE,PERCENT,UPDOWN,OPEN,YESTCLOSE,HIGH,LOW,VOLUME,TURNOVER,EXCHANGE_RATE,ZF,PE,MARKET_CAPITAL,EPS,FINANCEDATA.NET_PROFIT,FINANCEDATA.TOTALTURNOVER_&sort=SYMBOL&order=desc&count=20000&type=query&callback=callback_2057363736&req=12220"
#     )
# 
# 
#     # hongkong stock list
#     stock_list_hk = mapply(fun_listcomp_hk, urls, names(urls), SIMPLIFY = FALSE)
#     stock_list_hk = rbindlist(stock_list_hk)
# 
#   } else if (source=="sina") {
#     fun_listcomp_hk_sina = function(urli) {
#       num = doc = V1 = symbol = NULL
# 
#       dt = data.table(
#         doc = readLines(sprintf(urli, num), warn = FALSE)
#       )[, doc := iconv(doc, "GB18030", "UTF-8")
#       ][, doc := gsub("(\\[\\{)|(\\]\\})", "", doc)
#       ][, strsplit(doc, "\\},\\{")
#       ][, (c("symbol","name","engname","tradetype","lasttrade","prevclose","open","high","low","volume","currentvolume","amount","ticktime","buy","sell","high_52week","low_52week","eps","dividend","stocks_sum","pricechange","changepercent")) := tstrsplit(V1, ",", fixed=TRUE)
#       ][, V1 := NULL
#       ][, lapply(.SD, function(x) gsub(".+:|\"", "", x))
#       ][, `:=`(
#         market = "stock", region = "hk", exchange = "hkex",
#         board = ifelse(substr(symbol,1,2)=="08", "gem", "main")
#       )]
# 
#       return(dt)
#     }
# 
# 
#     url_hk <- c(
#       "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHKStockData?page=1&num=100000&sort=symbol&asc=1&node=qbgg_hk&_s_r_a=init"
#     )
#     stock_list_hk = fun_listcomp_hk_sina(url_hk)
#     }
# 
# 
#   stock_list_hk = stock_list_hk[, symbol := paste(symbol, substr(exchange,1,2) ,sep=".")]
#   if (return_price == FALSE) stock_list_hk = stock_list_hk[, c("market", "region", "exchange", "board", "symbol", "name"), with=FALSE]
#   data("market_symbols", envir = environment())
#   stock_list_hk2 = rbindlist(
#     list(setDT(market_symbols)[exchange=="hkex"], stock_list_hk))[, .SD[1], by=symbol]
# 
#   return(stock_list_hk2)
# }