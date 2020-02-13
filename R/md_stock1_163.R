
#' @import data.table 
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_unescape_unicode
md_stock_spotall_163 = function(symbol = c('a','index'), only_symbol = FALSE, show_tags = FALSE, ...) {
  prov = tags = market = exchange = time = . = submarket = region = board = name = mkt = indu = sec = NULL
    
  fun_stock_163 = function(urli, mkt) {
    code = symbol = exchange = . = name = high = low = price = yestclose = updown = percent = hs = volume = turnover = mcap = tcap = pe = mfsum = net_income = revenue = plate_ids = time = NULL
    # stock
    # c('code', 'five_minute' 'high', 'hs', 'lb', 'low', 'mcap', 'mfratio', 'mfsum', 'name', 'open', 'pe', 'percent', 'plate_ids', 'price', 'sname', 'symbol', 'tcap', 'turnover', 'updown', 'volume', 'wb', 'yestclose', 'zf', 'no', 'announmt', 'uvsnews')
    # index
    # c('code', 'high', 'low', 'name', 'open' 'percent', 'price', 'symbol', 'time', 'turnover' 'updown', 'volume', 'yestclose', 'no', 'zhenfu') 
    
    jsonDat = fromJSON(urli)
    
    jsonDF = jsonDat$list
    if (mkt == 'stock') {
      jsonDF$net_income = jsonDF$MFRATIO$MFRATIO2
      jsonDF$revenue = jsonDF$MFRATIO$MFRATIO10
      jsonDF[,c('MFRATIO', 'UVSNEWS','ANNOUNMT','NO')] = NULL 
      names(jsonDF) = tolower(names(jsonDF))
      
      jsonDF = setDT(jsonDF)[,`:=`(
        date = as.Date(substr(jsonDat$time,1,10)), 
        time = jsonDat$time#,
        #strptime(jsonDat$time, '%Y-%m-%d %H:%M:%S', tz = 'Asia/Shanghai')
      )][, .(symbol, name, date, open, high, low, close=price, prev_close=yestclose, change=updown, change_pct=percent*100, volume, amount=turnover, turnover=hs*100, cap_market=mcap, cap_total=tcap, pe_last=pe, eps=mfsum, net_income, revenue, plate_ids, time=as.POSIXct(time))
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
        date = as.Date(substr(jsonDat$time,1,10))
      )][, .(symbol, name, date, open, high, low, close=price, prev_close=yestclose, change=updown, change_pct=percent*100, volume, amount=turnover, time=as.POSIXct(time))]
    }
    
    return(jsonDF[, `:=`(market = mkt, region = 'cn')])
  }
  
  urls_163 = list(
    a = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQA&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=CODE&order=desc&count=100000&type=query', 
    b = 'http://quotes.money.163.com/hs/service/diyrank.php?host=http%3A%2F%2Fquotes.money.163.com%2Fhs%2Fservice%2Fdiyrank.php&page=0&query=STYPE%3AEQB&fields=NO%2CSYMBOL%2CNAME%2CPLATE_IDS%2CPRICE%2CPERCENT%2CUPDOWN%2CFIVE_MINUTE%2COPEN%2CYESTCLOSE%2CHIGH%2CLOW%2CVOLUME%2CTURNOVER%2CHS%2CLB%2CWB%2CZF%2CPE%2CMCAP%2CTCAP%2CMFSUM%2CMFRATIO.MFRATIO2%2CMFRATIO.MFRATIO10%2CSNAME%2CCODE%2CANNOUNMT%2CUVSNEWS&sort=PERCENT&order=desc&count=100000&type=query',
    index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESH&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query',
    index = 'http://quotes.money.163.com/hs/service/hsindexrank.php?host=/hs/service/hsindexrank.php&page=0&query=IS_INDEX:true;EXCHANGE:CNSESZ&fields=no,TIME,SYMBOL,NAME,PRICE,UPDOWN,PERCENT,zhenfu,VOLUME,TURNOVER,YESTCLOSE,OPEN,HIGH,LOW&sort=SYMBOL&order=asc&count=10000&type=query'
  )
  idx = which(names(urls_163) %in% unlist(strsplit(symbol,',')))
  
  df_stock_cn = try(rbindlist(mapply(
    fun_stock_163, urls_163[idx], c('stock','stock','index','index')[idx], SIMPLIFY = FALSE
  ), fill = TRUE), silent = TRUE)#, idcol = 'mkt')#[mkt %in% c('a','b'), mkt := 'stock']
  
  # date time of download
  datetime = gsub('[^(0-9)]','',df_stock_cn[1,time])
  # if (df_stock_cn[1,time] < as.POSIXct(paste(df_stock_cn[1,date], '15:00:00'))) 
  #   cat('The close price is spot price at', as.character(datetime), '\n')

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
  
  cols_rm = intersect(names(df), c('sector', 'industry', 'province', 'plate_ids', 'region', 'prev_close')) # , 
  if (length(cols_rm)>0) df = df[, (cols_rm) := NULL]
  return(df)
}


# query spot data from tx
# hq.sinajs.cn/list=sz150206
# http://qt.gtimg.cn/q=sz000001
md_stock_spot_tx = function(symbol1, only_syb_nam = FALSE, ...) {
  V1=V2=doc=.=symbol=name=high=low=prev_close=change=change_pct=volume=amount=turnover=cap_market=cap_total=pb=pe_last=pe_trailing=pe_forward=NULL
  
  syb = check_symbol_for_tx(symbol1)
  dt = readLines(sprintf('http://qt.gtimg.cn/q=%s', paste0(syb, collapse=',')))

  dt = data.table(
    doc = dt
  )[, doc := iconv(doc, 'GB18030', 'UTF-8')
  ][, doc := sub('.+=\"\\d+~(.+)\".+', '\\1', doc)
  ][, tstrsplit(doc, '~')]
  
  if (only_syb_nam) {
    return(dt[,.(symbol = check_symbol_for_yahoo(symbol1), name = V1)])
  }
  
  colnames_en = c('name', 'symbol', 'close', 'prev_close', 'open',
                  'volume', 'buy', 'sell', 
                  'bid1', 'bid1_volume', 'bid2', 'bid2_volume', 'bid3', 'bid3_volume', 'bid4', 'bid4_volume', 'bid5', 'bid5_volume',
                  'ask1', 'ask1_volume', 'ask2', 'ask2_volume', 'ask3', 'ask3_volume', 'ask4', 'ask4_volume', 'ask5', 'ask5_volume',
                  'last_trade', 'date', 'change', 'change_pct', 'high', 'low', 
                  '', 'volume', 'amount', 'turnover', 
                  'pe_trailing', '', 'high', 'low', '', 'cap_market', 'cap_total', 'pb', '', '', '', '', 'average', 'pe_forward', 'pe_last' )
  if (ncol(dt) <= 53) {
    colnames_en = colnames_en[seq_len(ncol(dt))]
  } else {
    colnames_en = c(colnames_en, rep('', ncol(dt)-53))
  }
  setnames(dt, colnames_en)
  
  num_cols = c(
    'open', 'high', 'low', 'close', 'prev_close', 'change', 'change_pct', 'volume', 'amount', 'turnover', 'cap_market', 'cap_total', 'pb', 'pe_last', 'pe_trailing', 'pe_forward'
  )
  dt = dt[,.(
    symbol, name, date, open, high, low, close, prev_close, change, change_pct, volume, amount, turnover, cap_market, cap_total, pb, pe_last, pe_trailing, pe_forward#, 
    #buy, sell, 
    #bid1, bid1_volume, bid2, bid2_volume, bid3, bid3_volume, bid4, bid4_volume, bid5, bid5_volume, 
    #ask1, ask1_volume, ask2, ask2_volume, ask3, ask3_volume, ask4, ask4_volume, ask5, ask5_volume
    )][, (num_cols) := lapply(.SD, as.numeric), .SDcols= num_cols
     ][, `:=`(
       symbol = check_symbol_for_yahoo(symbol1),
       volume = volume*100,
       amount = amount*10000,
       cap_market = cap_market*10^8, 
       cap_total = cap_total*10^8,
       time = as.POSIXct(date, format='%Y%m%d%H%M%S', tz='Asia/Shanghai'),
       date = as.Date(date, format='%Y%m%d%H%M%S')
     )]
  
  # if (dt[1,time] < as.POSIXct(paste(dt[1,date], '15:00:00')))
  #   cat('The close is the spot price at', dt[1, as.character(time)], '\n')
  
  return(dt[, `:=`(unit = 'CNY')])
}


#' @import data.table
#' @importFrom readr read_csv locale col_date col_character col_double col_integer
md_stock_hist1_163 = function(symbol1, from='1900-01-01', to=Sys.Date(), zero_rm=TRUE, ...) {
  V1 = name = change_pct = symbol = NULL
  # http://quotes.money.163.com/service/chddata.html?code=0000001&start=19901219&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # http://quotes.money.163.com/service/chddata.html?code=1399001&start=19910403&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # https://query1.finance.yahoo.com/v7/finance/download/^SSEC?period1=1526631424&period2=1529309824&interval=1d&events=history&crumb=mO08ZCtWRMI
  
  # 'http://api.finance.ifeng.com/akmonthly/?code=sh600000&type=last'
  # {'D': 'akdaily', 'W': 'akweekly', 'M': 'akmonthly'}
  
  # symbol
  syb = check_symbol_for_163(symbol1) 

  # date range
  fromto = lapply(list(from=from,to=to), function(x) format(check_fromto(x), '%Y%m%d'))
  
  # create link
  fields = c('TOPEN','HIGH','LOW','TCLOSE','LCLOSE','CHG','PCHG','VOTURNOVER','VATURNOVER','TURNOVER','MCAP','TCAP')
  link = sprintf('http://quotes.money.163.com/service/chddata.html?code=%s&start=%s&end=%s&fields=%s', syb, fromto$from, fromto$to, paste0(fields, collapse = ';'))
    # paste0('http://quotes.money.163.com/service/chddata.html?code=',syb,'&start=',fromto$from,'&end=',fromto$to,'&fields=TOPEN;HIGH;LOW;TCLOSE;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER;TURNOVER;MCAP;TCAP')
             
   
  
  # download data from 163
  dt <- read_csv(
    file=link, locale = locale(encoding = 'GBK'), na=c('', 'NA', 'None'),
    col_types=list(col_date(format = ''), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
  attr(dt, 'spec') <- NULL
  setDT(dt)
  # dt <- load_read_csv(link, 'GBK')
  
  # set names of datatable
  cols_name = c('date', 'symbol', 'name', 'open', 'high', 'low', 'close', 'prev_close', 'change', 'change_pct', 'volume', 'amount', 'turnover', 'cap_market', 'cap_total')
  setnames(dt, cols_name)
  setkeyv(dt, 'date')
  
  # if (max(dt[['date']]) < lwd()) dt = rbindlist(list(dt, md_stock_spot1_tx(symbol1)[,names(dt), with=FALSE]), fill = FALSE)
  dt = dt[, symbol := check_symbol_for_yahoo(symbol1)][, (cols_name[c(2,3,1,4:15)]), with=FALSE]
  # if (max(dt[['date']]) < lwd()) dt = unique(dt, by='date')
  
  # fill zeros in dt
  if (zero_rm) {
    dt = dt[close != 0]
  } else {
    cols_name = c('open', 'high', 'low', 'close')
    dt = dt[, (cols_name) := lapply(.SD, fill0), .SDcols = cols_name]
  }
  
  
  # adding valuation ratios and adjust for dividend
  chk_syb = try(check_symbol_for_yahoo(dt[1, tstrsplit(symbol, '\\.')][,V1]), silent = TRUE)
  if (chk_syb == dt[1,symbol]) {
    valuation = list(...)[['valuation']]
    if (is.null(valuation)) valuation = FALSE
    if (valuation) dt = md_stock_pe1_163(dt)
    
    # adjust = list(...)[['adjust']]
    # if (is.null(adjust)) adjust = 'split'
    # adjust = ifelse(adjust, 'dividend', 'split') 
    dt = md_stock_adjust1(dt, source = '163', adjust = list(...)[['adjust']])
  }
  
  # create unit/name columns
  dt = dt[, `:=`(
    unit = 'CNY', 
    name = gsub('\\s', '', name)
  )]#[, name := name[.N]]
  setkeyv(dt, 'date')
  return(dt)
}

# valuation ratios pe, pb, ps, pcf
md_stock_pe1_163 = function(dat) {
  symbol=V1=var_id=value=fs_month_diff=REV_Q=REV=REV_Y=NP_Q=NP=NP_Y=fs_month=NP_Dec=NP_LY=date2=cap_total=BV=NIDCash=NULL
  
  
  # symbol1 = '000001'
  symbol1 = dat[1, tstrsplit(symbol, '\\.')][,V1]
  # main financial indicators
  mainfi = try(fs_type1_cn(symbol1, 'fi0_main'), silent = TRUE)
  if (inherits(mainfi, 'try-error')) return(dat)
  
  # main financial indicators
  # book value per share
  # Revenue from 
  # Net profit
  # Net Increase (Decrease) in Cash and Cash Equivalents
  mfi = dcast(
    merge(
      mainfi[
        var_id %in% c(18,4,10,13)
        ][, value := ifelse(value==0, NA, value)], 
      data.table(var_id = c(18,4,10,13),
                 var = c('BV', 'REV', 'NP', 'NIDCash')),
      by = 'var_id', all.x = TRUE
    ), 
    date ~ var
  )[, `:=`(fs_month = month(date), fs_month_diff = mean(month(date) - shift(month(date), type='lag', fill = 0))), by = year(date)
  # trailing REV/Income from main operation  
  ][fs_month_diff == 3,                  REV_Q   := REV-shift(REV, type='lag'), by = year(date)
  ][fs_month_diff == 3 & is.na(REV_Q),   REV_Q   := REV
  ][fs_month_diff == 3,                  REV_Y := runSum(REV_Q, 4)
  # trailing NP/Net Income
  ][fs_month_diff == 3,                  NP_Q := NP-shift(NP, type='lag'), by = year(date)
  ][fs_month_diff == 3 & is.na(NP_Q),    NP_Q := NP
  ][fs_month_diff == 3,                  NP_Y := runSum(NP_Q, 4)
  # last year NP/Net Income              
  ][fs_month==12, NP_Dec := NP
  ][, NP_LY := shift(NP_Dec, type='lag')
  ][, NP_LY := fillna(NP_LY)
  ][fs_month_diff ==  3, date2 := as.Date(paste(year(date), month(date)- 2, 1, sep='-'))
  ][fs_month_diff ==  6, date2 := as.Date(paste(year(date), month(date)- 5, 1, sep='-'))
  ][fs_month_diff == 12, date2 := as.Date(paste(year(date), month(date)-11, 1, sep='-'))
  ][, date := date2]
  
  # merge mfi with dat
  cols_fillna = c('fs_month','BV','NIDCash', 'REV_Y', 'NP', 'NP_Y', 'NP_LY')
  dat_pbpe = merge(
    dat, 
    mfi[, c('date', cols_fillna), with=FALSE], all = TRUE, by = 'date'
  )[, (cols_fillna) := lapply(.SD, fillna), .SDcols = cols_fillna
  ][!is.na(symbol)
  ][,`:=`(
    pb = cap_total/BV/10000,
    pe_last = cap_total/NP_LY/10000, # last year ratio
    pe_trailing = cap_total/NP_Y/10000, # trailing twelve month
    pe_forward = cap_total/(NP/(fs_month/12))/10000, # forward
    ps = cap_total/REV_Y/10000,
    pcf = cap_total/NIDCash/10000
  )][, (cols_fillna) := NULL]
  
  return(dat_pbpe)
}

# dividends
md_stock_divsplit1_163 = function(symbol1, from=NULL, to=NULL, ret = c('div', 'spl', 'rig')) {
  name = symbol = date2 = date1 = date0 = fenhong = . = songgu = spl = zhuanzeng = new_issues = old_issues = issue_price = issue_rate = splits = dividends = NULL
  
  # symbol1 = '000001'
  stk_price = md_stock_spot_tx(symbol1, only_syb_nam=TRUE)
  # return dts
  div_spl = list()
  
  # get dividends
  tbls = sprintf('http://quotes.money.163.com/f10/fhpg_%s.html#01d05', sub('.*?([0-9]{6}).*?','\\1', symbol1)) %>%
    read_html(.) %>% 
    rvest::html_table(., fill = TRUE, header = TRUE)
  
  
  tbl_divspl = setDT(tbls[[4]])[-1][, lapply(.SD, function(x) replace(x, x=='--', NA))][,c(3:7,8)]
  setnames(tbl_divspl, c('songgu', 'zhuanzeng', 'fenhong', 'date0', 'date1', 'date2'))
  if (length(unique(unlist(tbl_divspl))) == 1 & nrow(tbl_divspl) == 1) {
    tbl_divspl = tbl_divspl[.0]
  } 
  tbl_divspl = tbl_divspl[
    , (c('songgu', 'zhuanzeng', 'fenhong')) := lapply(.SD, as.numeric), .SDcols = c('songgu', 'zhuanzeng', 'fenhong')
    ][, (c('date0', 'date1', 'date2')) := lapply(.SD, as.Date), .SDcols = c('date0', 'date1', 'date2')
      ][is.na(date2), date2 := date1
        ][order(date1)]
  
  if ('div' %in% ret) { # dividend
    tbl_divident = tbl_divspl[fenhong > 0]
    if (nrow(tbl_divident) == 0) {
      div_spl[['div']] = data.table(
          date      = Sys.Date(),
          dividends = 1
        )[.0]
    } else {
      div_spl[['div']] = tbl_divident[,.(
          date      = date0,
          dividends = fenhong/10
        )]
    }
  }
  
  if ('spl' %in% ret) { # split
    tbl_split = rbind(
      tbl_divspl[, .(spl = songgu, date = date0)][spl > 0],
      tbl_divspl[, .(spl = zhuanzeng, date = date0)][spl > 0]
    )[, lapply(.SD, sum), keyby = date]
    
    if (nrow(tbl_split) == 0) {
      div_spl[['spl']] = data.table(
          date   = Sys.Date(),
          splits = 1
        )[.0]
    } else {
      div_spl[['spl']] = tbl_split[,.(
          date   = as.Date(date),
          splits = spl/10
        )]
    }
  }
  
  if ('rig' %in% ret) { # rights issue/offering
    rig = setDT(tbls[[5]])[,c(8,5,6,3)]
    setnames(rig, c('date', 'new_issues', 'old_issues', 'issue_price'))
    if (length(unique(unlist(rig))) == 1 & nrow(rig) == 1) {
      div_spl[['rig']] = data.table(
        date = Sys.Date(), 
        issue_rate = 1, 
        issue_price = 1
      )[.0]
    } else {
      div_spl[['rig']] = rig[
        , (c('new_issues', 'old_issues')) := lapply(.SD, function(x) as.numeric(gsub('[^0-9\\.]', '', x))), .SDcols = c('new_issues', 'old_issues')
        ][new_issues > 0
          ][, .(
            date   = as.Date(date), 
            issue_rate = new_issues/old_issues, 
            issue_price
          )]
    }
  }
  
  div_spl = Reduce(
    function(x,y) merge(x,y,all=TRUE,by='date'), div_spl
  )[,`:=`(
    symbol = stk_price$symbol,
    name   = stk_price$name
  )][,.(symbol, name, date, splits, dividends, issue_rate, issue_price)]
  
  setkeyv(div_spl, 'date')
  return(div_spl)
}



#' @import data.table
md_stock_163 = function(symbol, from='1900-01-01', to=Sys.Date(), print_step=1L, freq = 'daily', zero_rm=TRUE, ...) {
  # arguments
  arg_lst = list(...)
  # frequency
  freq = check_arg(freq, c('daily'))
  # type 
  type = arg_lst[['type']]
  # return valuation ratios
  if (!('valuation' %in% names(arg_lst))) arg_lst[['valuation']] = FALSE
  # if (!('adjust' %in% names(arg_lst))) arg_lst[['adjust']] = 'split'
  
  # query data
  if (type == 'spot') {
    fuc = 'md_stock_spot_tx'
    if (length(intersect(symbol, c('a','b','index'))) > 0) fuc = 'md_stock_spotall_163'
    dat_list <- try(do.call(fuc, args=list(symbol=symbol, ...)), silent = TRUE)
      
  }  else if (type == 'history') {
    dat_list = load_dat_loop(symbol, 'md_stock_hist1_163', args = c(list(from = from, to = to, zero_rm = zero_rm), arg_lst), print_step=print_step)
    
  } else if (type == 'adjfactor') (
    dat_list = load_dat_loop(symbol, 'md_stock_divsplit1_163', args = list(from = from, to = to), print_step=print_step)
    
  )
  
  rmcols_func = function(x) {
    cols_rm = intersect(names(x), c('prev_close', 'change')) #,'change_pct'
    if (length(cols_rm)>0) x = x[, (cols_rm) := NULL]
    return(x)
  }
  if (type != 'adjfactor') {
    if (inherits(dat_list, 'list')) {
      dat_list = lapply(dat_list, rmcols_func)
    } else if (inherits(dat_list, 'data.frame')) {
      dat_list = rmcols_func(dat_list)
    }
  }
  
  return(dat_list)
}

