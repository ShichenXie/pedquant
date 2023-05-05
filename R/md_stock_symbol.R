
#' @import data.table
#' @importFrom stringi stri_unescape_unicode
md_stk_syb_hk = function(source="eastmoney", return_price=TRUE) {
  symbol = exchange = market_symbols = stock_list_hk2 = ticktime = f1 = name = market = mktcode = sybcode = NULL

  if (source=="sina") {
    url_hk_sina <- c(
      "http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHKStockData?page=1&num=100000&sort=symbol&asc=1&node=qbgg_hk&_s_r_a=init"
    )
    stock_list_hk = setDT(fromJSON(url_hk_sina))[, `:=`(
        market = "stock", exchange = "hkex",
        board = ifelse(substr(symbol,1,2)=="08", "gem", "main")
    )] 
    setnames(stock_list_hk, c('engname', 'lasttrade', 'prevclose', 'pricechange', 'changepercent', 'market_value', 'pe_ratio'), c('name_eng', 'close', 'close_prev', 'change', 'change_pct', 'cap_total', 'pe'))
  } else if (source == 'eastmoney') {
        fid = 
            c(1, 12, 14, 124, 17, 15, 16, 2, 5, 6, 8, 29, 62, 23, 20, 21)
            # setdiff(1:149,c(36:61,100:108, 112:123,128:138))
        # "f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f62,f128,f136,f115,f152", 
        
        url_hk_em = sprintf(
            "http://72.push2.eastmoney.com/api/qt/clist/get?pn=1&pz=50000&po=1&np=1&ut=%s&fltt=2&invt=2&fid=f3&fs=%s&fields=%s&_=%s", 
            'bd1d9ddb04089700cf9c27f6f7426281', 
            "m:128%20t:1", 
            paste0('f', fid, collapse = ','),
            date_num(Sys.time(), 'ms')
            ) # m:128%20t:1,m:128%20t:2,m:128%20t:3,m:128%20t:4,
        
        datem = read_apidata_eastmoney(url_hk_em, type = 'real_cn')
        setnames(datem, fidnam('fi'), fidnam('ni'), skip_absent=TRUE)
        
        datem = datem[
            , ticktime := as.POSIXct(ticktime, origin = '1970-01-01')
        ][mktcode == 1024, market := 'bullbear'
        ][mktcode == 64, market := 'warrant']
        
        stock_list_hk = datem[f1==3] 
    }


  stock_list_hk = stock_list_hk[,`:=`(
      symbol = paste0(sybcode, '.HK'),
      date = as_date(ticktime)
  )][is.na(market)]
  
  return(stock_list_hk)
}

md_stk_syb_us = function(exchange) {
  . = symbol = name = sector = industry = country = ipoyear = marketCap = ticktime = sybcode = NULL
  # c("AMEX", "NASDAQ", "NYSE")
  exchange = toupper(exchange)
  exchange2code = list(NASDAQ = '105', NYSE='106', AMEX = '107')
  fid = c(1, 12, 14, 124, 17, 15, 16, 2, 5, 6, 8, 29, 62, 23, 20, 21)
  # "f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f12,f13,f14,f15,f16,f17,f18,f20,f21,f23,f24,f25,f26,f22,f33,f11,f62,f128,f136,f115,f152", 
  
  url = sprintf(
      "http://72.push2.eastmoney.com/api/qt/clist/get?pn=1&pz=20000&po=1&np=1&ut=bd1d9ddb04089700cf9c27f6f7426281&fltt=2&invt=2&fid=f3&fs=m:%s&fields=%s&_=%s", 
      exchange2code[[exchange]], 
      paste0('f', fid, collapse = ','),
      date_num(Sys.time(), 'ms')
  )
  
  datem = read_apidata_eastmoney(url, type = 'real_us')
  setnames(datem, fidnam('fi'), fidnam('ni'), skip_absent=TRUE)
  
  datem = datem[, ticktime := as.POSIXct(ticktime, origin = '1970-01-01')
              ][, `:=`(
                  date = as_date(ticktime), 
                  symbol = sybcode
              )] 
  
  if (FALSE) {
      url = sprintf(
          # "https://old.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download",
          # 'http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=%s&render=download', 
          "https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=%s&download=true",
          exchange)
      
      dat = fromJSON(url)
      dat2 = setDT(dat$data$rows)[,.(market='stock', exchange=exchange, board=NA, symbol, name, sector, industry, cap_market=marketCap, ipoyear, country)] 
  }
  
  
  return(datem)
}



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
    dtmp = md_stocka_eastmoney()
    temp = rbindlist(dtmp[c('stock', 'index', 'fund')])[exchange %in% XCHG]
    exc = intersect(c("sse", "szse", 'bse'), XCHG)
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
      dat_lst[[e]] = md_stk_syb_hk()
    }
  }
  if (any(c("amex", "nasdaq", "nyse") %in% XCHG)) {
    exc = intersect(c("amex", "nasdaq", "nyse"),XCHG)
    for (e in exc) {
      if ((print_step > 0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, exc_len)), collapse = '/'), e))
      i = i+1
      dat_lst[[e]] = md_stk_syb_us(e)
    }
  }
  # dat_lst = rbindlist(dat_lst, fill = TRUE)
  return(dat_lst)
}


#' symbol components of exchange
#' 
#' \code{md_stock_symbol} returns all stock symbols by exchange
#' 
#' @param exchange the available stock exchanges are sse, szse, hkex, amex, nasdaq, nyse.
#' @param ... ignored parameters
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
md_stock_symbol = function(exchange=NULL, ...) {
    datlst = md_stock_symbol_exchange(exchange)
    datlst2 = lapply(datlst, function(x) {
        cols = c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'volume', 'amount', 'turnover', 'market', 'mktcode', 'pe_ttm', 'pb', 'cap_total', 'cap_market')
        
        x[, intersect(cols, names(x)), with = FALSE]
    } )
}













































# stock symbol constituent of index
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

