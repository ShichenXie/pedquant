# stock info ------
md_stock1_info_163 = function(symbol1, rev_hist = FALSE, ...) {
  . = N = V1 = V2 = V3 = V4 = V5 = V6 = X1 = X2 = X3 = X4 = revenue_by = mkt = report_date = rid = NULL
  
  # symbol1 = '600036'
  # http://quotes.money.163.com/f10/gszl_600036.html#01f01
  # http://quotes.money.163.com/service/gszl_600036.html?type=cp
  # http://quotes.money.163.com/service/gszl_600036.html?type=hy
  # http://quotes.money.163.com/service/gszl_600036.html?type=dy
  
  # skip index
  chk_syb = check_symbol_cn(symbol1)
  symbol1 = chk_syb$syb
  if (chk_syb[, mkt == 'index' || is.na(mkt)]) return(NULL)
  # symbol1 = '000001'
  stk_price = md_stock_real_tx(symbol1, only_syb_nam=TRUE)
  # columns
  rev_str_cols = c('revenue', 'cost', 'profit', 'gross_margin', 'profit_proportion')
  staff_str_cols = c('number', 'proportion')
  
  if (rev_hist) {
    revhist_preprocess = function(dat) {
      if (is.null(dat)) return(invisible())
      
      rev_str_cols = c('revenue', 'cost', 'profit', 'gross_margin', 'profit_proportion')
      
      rbindlist(lapply(
        split(
          dat[
            , rid := 0
          ][V1 == dat[1,1] & V2 == dat[1,2], rid := 1
          ][, rid := cumsum(rid)][], 
          by = 'rid'
        ), 
        function(x) {
          cbind(
            stk_price, 
            setnames(
              x[, report_date := x[1,5]][-c(1:2)][, .(report_date, V1, V2, V3, V4, V5, V6)], 
              c('report_date', 'variable', rev_str_cols)
            )
          )
        }
      ))[, (rev_str_cols) := lapply(.SD, function(x) {
        as.numeric(gsub(',|\\s|(--)|%', '', x))
      }), .SDcols = rev_str_cols][]
    }
    
    rev_product = load_read_csv(
      sprintf('http://quotes.money.163.com/service/gszl_%s.html?type=cp', symbol1), 
      encode = 'GBK', csv_header = FALSE
    )
    
    rev_industry = load_read_csv(
      sprintf('http://quotes.money.163.com/service/gszl_%s.html?type=hy', symbol1), 
      encode = 'GBK', csv_header = FALSE
    )
    
    rev_location = load_read_csv(
      sprintf('http://quotes.money.163.com/service/gszl_%s.html?type=dy', symbol1), 
      encode = 'GBK', csv_header = FALSE
    )
    
    
    retdat = list(
      revenue = rbindlist(list(
        product = revhist_preprocess(rev_product), 
        industry = revhist_preprocess(rev_industry),
        location = revhist_preprocess(rev_location)
      ), idcol = 'revenue_by')[, report_date := as.Date(report_date)][]
    )
  } else {
    # load all tables
    datweb = read_html(sprintf(
      'http://quotes.money.163.com/f10/gszl_%s.html#01f01', 
      sub("^.*?([0-9]+).*$",'\\1', symbol1)
    ))
    
    rdlist = 
      data.table(
        revstaff = c('product', 'industry', 'location', 'education', 'work_type'), 
        report_date = html_text(html_nodes(datweb, 'div.report_date span'))
      )[, report_date := substr(gsub(' *', '', report_date), 6, nchar(report_date)) ][]
    # sub('.*([0-9]{4}-[0-9]{2}-[0-9]{2}).*', '\\1', rd) 
    
    
    tbls = rvest::html_table(datweb, fill = TRUE, header = FALSE)
    
    profile = 
      rbind(
        setDT(tbls[[4]])[1:9,   .(variable = X1, value = X2)], 
        setDT(tbls[[4]])[1:9,   .(variable = X3, value = X4)], 
        setDT(tbls[[4]])[10:12, .(variable = X1, value = X2)]
      )
    
    ipo = setDT(tbls[[5]])[, .(variable = X1, value = X2)][]
    info = cbind(stk_price, rbindlist(list(profile=profile, ipo=ipo), idcol = 'info_type'))
    
    
    str_revenue = tbls[7:9]
    names(str_revenue) = c('product', 'industry', 'location')
    str_revenue = rbindlist(
      lapply( str_revenue, function(x) setnames(x[-1,], c('variable', rev_str_cols)) ), 
      idcol = 'revenue_by'
    )[, (rev_str_cols) := lapply(.SD, function(x) {
      as.numeric(gsub(',|\\s|-|%', '', x))
    }), .SDcols = rev_str_cols][]
    
    str_revenue = merge(
      cbind(stk_price, setnames(rdlist[1:3], 'revstaff', 'revenue_by')), 
      str_revenue, by = 'revenue_by', all.y = TRUE
    )
    
    
    str_staff = tbls[10:11]
    names(str_staff) = c('education', 'work_type')
    str_staff = rbindlist(
      lapply(str_staff, function(x) setnames(x[-1,], c('variable', staff_str_cols))), 
      idcol = 'staff_by'
    )[, (staff_str_cols) := lapply(.SD, function(x) {
      as.numeric(gsub('\\s|%', '', x))
    }), .SDcols = staff_str_cols]
    
    str_staff = merge(
      cbind(stk_price, setnames(rdlist[4:5], 'revstaff', 'staff_by')), 
      str_staff, by = 'staff_by', all.y = TRUE
    )
    
    
    retdat = list(
      info = info, 
      revenue = str_revenue[report_date != '--'][, report_date := as.Date(report_date)], 
      staff = str_staff[, report_date := as.Date(report_date)]
    )
  }
  
  return(retdat)
}


# stock history ------
#' @import data.table
#' @importFrom readr read_csv locale col_date col_character col_double col_integer
md_stock1_history_163 = function(symbol1, from='1900-01-01', to=Sys.Date(), zero_rm=TRUE, ...) {
  V1 = name = change_pct = symbol = close_prev = unit = NULL
  # http://quotes.money.163.com/service/chddata.html?code=0000001&start=19901219&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # http://quotes.money.163.com/service/chddata.html?code=1399001&start=19910403&end=20180615&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER
  # https://query1.finance.yahoo.com/v7/finance/download/^SSEC?period1=1526631424&period2=1529309824&interval=1d&events=history&crumb=mO08ZCtWRMI
  
  # 'http://api.finance.ifeng.com/akmonthly/?code=sh600000&type=last'
  # {'D': 'akdaily', 'W': 'akweekly', 'M': 'akmonthly'}
  chk_syb1 = check_symbol_cn(symbol1)[]
  if (chk_syb1$mkt %in% 'fund') {
      return(md_fund1_history_163(symbol1, from, to))
  } else if (!(chk_syb1$exchg_code %in% c('sz', 'ss'))) {
      return(md_stock1_history_eastmoney(symbol1, from, to, ...))
  }
  
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
  cols_name = c('date', 'symbol', 'name', 'open', 'high', 'low', 'close', 'close_prev', 'change', 'change_pct', 'volume', 'amount', 'turnover', 'cap_market', 'cap_total')
  setnames(dt, cols_name)
  setkeyv(dt, 'date')
  
  # if (max(dt[['date']]) < lwd()) dt = rbindlist(list(dt, md_stock_real1_tx(symbol1)[,names(dt), with=FALSE]), fill = FALSE)
  dt = dt[, symbol := check_symbol_for_yahoo(symbol1)][, (cols_name[c(2,3,1,4:15)]), with=FALSE]
  # if (max(dt[['date']]) < lwd()) dt = unique(dt, by='date')
  
  # fill zeros in dt
  dt[close_prev == 0, close_prev := NA]
  if (zero_rm) {
    dt = dt[close != 0]
  } else {
    cols_name = c('open', 'high', 'low', 'close')
    dt = dt[, (cols_name) := lapply(.SD, fill0), .SDcols = cols_name]
  }
  
  
  # adding valuation ratios and adjust for dividend
  chk_syb = try(check_symbol_for_yahoo(dt[1, tstrsplit(symbol, '\\.')][,V1]), silent = TRUE)
  if (chk_syb == dt[1,symbol] && nrow(dt)>0) {
    valuation = list(...)[['valuation']]
    if (is.null(valuation)) valuation = FALSE
    if (valuation) dt = md_stock1_pe_163(dt)
  }
  
  # adjust = list(...)[['adjust']]
  # if (is.null(adjust)) adjust = 'split'
  # adjust = ifelse(adjust, 'dividend', 'split') 
  dt = md_stock_adj1ohlc(dt, source = '163', adjust = list(...)[['adjust']])

  # create unit/name columns
  dt = dt[, unit := 'CNY']#[, name := name[.N]]
  setkeyv(dt, 'date')
  return(dt)
}

# valuation ratios pe, pb, ps, pcf
md_stock1_pe_163 = function(dat) {
  symbol=V1=var_id=value=fs_month_diff=REV_Q=REV=REV_Y=NP_Q=NP=NP_Y=fs_month=NP_Dec=NP_LY=date2=cap_total=BV=NIDCash=NULL
  
  
  # symbol1 = '000001'
  symbol1 = dat[1, tstrsplit(symbol, '\\.')][,V1]
  # main financial indicators
  mainfi = try(fs_type1_cn('fi0_main', symbol1), silent = TRUE)
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
#' @importFrom stringi stri_isempty
md_stock1_divsplit_163 = function(symbol1, from=NULL, to=NULL, ret = c('div', 'spl', 'rig')) {
  name = symbol = date2 = date1 = date0 = fenhong = . = songgu = spl = zhuanzeng = new_issues = old_issues = issue_price = issue_rate = splits = dividends = mkt = NULL
  
  # skip index
  if (check_symbol_cn(symbol1)[, mkt == 'index' || is.na(mkt)]) return(NULL)
  # symbol1 = '000001'
  stk_price = md_stock_real_tx(symbol1, only_syb_nam=TRUE)
  # return dts
  div_spl = list()
  
  # get dividends
  tbls = sprintf('http://quotes.money.163.com/f10/fhpg_%s.html#01d05', sub("^.*?([0-9]+).*$",'\\1', symbol1)) %>%
    read_html(.) %>% 
    rvest::html_table(., fill = TRUE, header = TRUE)
  
  
  tbl_divspl = setDT(tbls[[4]])[-1][, lapply(.SD, function(x) replace(x, x=='--', NA))][,c(3:8)]
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
          date      = date1,
          dividends = fenhong/10
        )]
    }
  }
  
  if ('spl' %in% ret) { # split
    tbl_split = rbind(
      tbl_divspl[, .(spl = songgu, date = date1)][spl > 0],
      tbl_divspl[, .(spl = zhuanzeng, date = date1)][spl > 0]
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
        ][new_issues > 0 & !stri_isempty(date)
          ][, .(
            date   = as.Date(date), 
            issue_rate = new_issues/old_issues, 
            issue_price
          )]
    }
  }
  
  div_spl = Reduce(
    function(x,y) merge(x,y,all=TRUE,by='date'), div_spl
  )[,.(date, splits, dividends, issue_rate, issue_price)][!is.na(date)]
  if (nrow(div_spl) == 0) stk_price = stk_price[.0]
  div_spl = cbind(stk_price, div_spl)
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
  if (type == 'real') {
    fuc = 'md_stock_real_tx'
    if (length(intersect(symbol, c('a','b','index', 'fund'))) > 0) fuc = 'md_stockall_real_163'
    dat_list <- try(do.call(fuc, args=list(symbol=symbol, ...)), silent = TRUE)
      
  }  else if (type == 'history') {
    dat_list = load_dat_loop(symbol, 'md_stock1_history_163', args = list(from = from, to = to, zero_rm = zero_rm, ...), print_step=print_step)
    
  } else if (type == 'adjfactor') (
    dat_list = load_dat_loop(symbol, 'md_stock1_divsplit_163', args = list(from = from, to = to), print_step=print_step)
    
  ) else if (type == 'info') {
    dat_list = load_dat_loop(symbol, 'md_stock1_info_163', args = list(...), print_step=print_step)
  }
  
  rmcols_func = function(x) {
    name = NULL
    
    cols_rm = intersect(names(x), c('change')) #,'close_prev', 'change_pct'
    if (length(cols_rm)>0) x = x[, (cols_rm) := NULL]
    if ('name' %in% names(x)) x = x[, name := gsub('\\s', '', name)]
    return(x)
  }
  if (type %in% c('history', 'real')) {
    if (inherits(dat_list, 'list')) {
      dat_list = lapply(dat_list, rmcols_func)
    } else if (inherits(dat_list, 'data.frame')) {
      dat_list = rmcols_func(dat_list)
    }
  }
  
  return(dat_list[])
}


# fund real ------
md_fundall_real_163 = function() {
  . = fund = high = low = name = price = symbol = time = turnover = volume = yestclose = unit = percent = NULL
  
  dat_lst = lapply(list(
    fund_close = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=STYPE:FDC;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_11861237&req=0%s',
    fund_etf = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=find:/ETF/;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_1696032826&req=0%s',
    fund_lof = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=find:/LOF/;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_814011473&req=0%s',
    fund_leveraged = 'http://quotes.money.163.com/fn/service/fundtrade.php?host=/fn/service/fundtrade.php&page=0&query=FUND_TYPE4:_int_190701;UPDOWN:_exists_true&fields=no,SYMBOL,NAME,SNAME,PRICE,UPDOWN,PERCENT,VOLUME,TURNOVER,OPEN,HIGH,LOW,YESTCLOSE,CODE&sort=PERCENT&order=desc&count=%s&type=query&callback=callback_2112195916&req=0%s'
  ), function(u) {
    # fromJSON(sub('.+\((\\1)\)', readline(u)))
    url = sprintf(
      u, 20000,
      paste0(hour(Sys.time()), minute(Sys.time()))
    )
    
    # print(url)
    dt = read_lines(url)
    dt = fromJSON(sub('.+?\\((.+)\\)', '\\1', dt))
    dat = setDT(dt$list)[, time := dt$time]
    return(dat)
  })

  dat_df = rbindlist(dat_lst, idcol = 'fund')
  setnames(dat_df, tolower(names(dat_df)))
  
  dat_df = dat_df[, .(symbol = check_symbol_for_yahoo(symbol), date = md_stock_real_tx('^000001')$date, name, open, high, low, close = price, close_prev = yestclose, change_pct = percent*100, volume, amount = turnover, market = fund, unit = 'CNY')] # change=updown, sname, 
  
  return(dat_df)
}

# fund history ------
md_fund1_history_163 = function(symbol1, from='1900-01-01', to=Sys.Date()) {
  . = amount = change_pct = discount_pct = high = low = mkt = turnover = volume = unit = NULL
  
  # url = sprintf('http://api.finance.ifeng.com/akdaily/?code=%s&type=last', syb1)
  # {'D': 'akdaily', 'W': 'akweekly', 'M': 'akmonthly'}
  
  # from tx ------
  dat_tx = md_stock1_history_tx(symbol1, from = from, to = to, adjust = '')
  
  if (FALSE) {
      # syb1 = check_symbol_for_tx(symbol1)
      # dat1 = 'init'
      # i=1
      # datlst = list()
      # yrng = year(seq(as.Date(to), as.Date(from), by = '-1 years'))
      # while (!inherits(dat1, 'try-error')) {
      #     # print(yr1)
      #     yr1 = yrng[i]
      #     url = sprintf('http://data.gtimg.cn/flashdata/hushen/daily/%s/%s.js?visitDstTime=1', substr(yr1,3,4), syb1)
      #     dat1 = try(suppressWarnings(fread(url, showProgress=F)), silent = T)
      #     
      #     if(!inherits(dat1, 'try-error')) {
      #         setnames(dat1, c('date', 'open', 'high', 'low', 'close', 'volume'))
      #         datlst[[as.character(yr1)]] <- dat1
      #         i = i+1
      #     }
      # }
      # 
      # dat_tx = rbindlist(
      #     datlst
      # )[, volume := gsub('[^0-9.]', '', volume)
      # ][, lapply(.SD, as.numeric)
      # ][, date := as.Date(paste0(substr(yrng[i],1,2), date), format = '%Y%m%d')
      # ][order(date)]
  }
  
  
  # from 163 ------
  url0 = sprintf('http://quotes.money.163.com/fund/zyjl_%s_0.html?start=%s&end=%s', check_symbol_cn(symbol1)$syb, from, to)
  pagnum = read_html(url0) %>% 
    html_nodes('div.mod_pages a') %>% 
    html_text() %>% .[-length(.)] %>% 
    as.integer() %>% max(., na.rm = T)
  
  dat_lst2 = lapply(
    sprintf('http://quotes.money.163.com/fund/zyjl_%s_%s.html?start=%s&end=%s', check_symbol_cn(symbol1)[]$syb, seq(0,pagnum-1), from, to), 
    function(u) {
      read_html(u) %>% html_table(fill = TRUE) %>% .[[1]]
    }
  )
  dat_163 = rbindlist(dat_lst2)
  setnames(dat_163, c('date', 'close', 'change_pct', 'volume2', 'amount2', 'turnover', 'discount_pct'))
  
  volamt2num = function(x) {
      x = gsub(',', '', x)
      # unicode
      if (grepl('\u4e07', x)) {
          as.numeric(sub('\u4e07', '', x)) * 10^4
      } else if (grepl('\u4ebf', x)) {
          as.numeric(sub('\u4ebf', '', x)) * 10^8
      }
  }
  dat_1632 = copy(dat_163)[, `:=`(
    date = as.Date(date), 
    change_pct = as.numeric(sub('%', '', change_pct)),
    turnover = as.numeric(sub('%', '', turnover))/100,
    discount_pct = as.numeric(sub('%', '', discount_pct))
  )][, (c('volume', 'amount')) := lapply(.SD, volamt2num), by = date, .SDcols = c('volume2', 'amount2')
   ][, (c('volume2', 'amount2')) := NULL]
  
  dat = merge(
      dat_tx[,(c('close', 'volume')) := NULL], dat_1632, by = 'date', all.y=TRUE
  )[, unit := 'CNY'
  ][, c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'change_pct', 'volume', 'amount', 'turnover', 'discount_pct', 'unit'), with = FALSE
  ]
  
  return(dat)
}
# szse fund: 15, 16, 18
# sse fund: 50, 51, 52
