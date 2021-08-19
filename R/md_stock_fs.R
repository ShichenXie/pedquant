
# - balance sheet: a company's assets, liabilities, and owners equity at a given point in time.
# - income statement: a company's income, expenses, and profits over a stated period of time. 
# - statement of changes in equity or equity statement: the changes in equity of the company over a stated period of time.
# - cash flow statement: a company's cash flow activities, particularly its operating, investing and financing activities over a stated period of time.


# query one type financial statement of one symbol
fs_type1_cn = function(row_type, symbol1) {
    urls = fs_type = fs_name = patterns = value = type = . = fr_id = fr_name = name = name_en = var_name = var_id = NULL
    
    
    # types of financial statements
    fr_163 = setDT(copy(financial_statements_163))
    setkey(fr_163, 'type')
    
    # load data from 163
    fr_url = sprintf(fr_163[row_type,urls], symbol1)
    dat = suppressWarnings(read_csv(file=fr_url, col_types=cols(.default = "c"), locale = locale(encoding = "GBK"), na = c("", "NA", "--")))
    setnames(setDT(dat), c("var_name", names(dat)[-1]))
    
    # melt dataframe
    dat_melt = melt(
        dat[!(var_name %in% c(NA, "", "\t\t"))][, var_id := .I], 
        id=c("var_id","var_name"), measure=patterns("\\d{4}-"),
        variable.name = "date", value.name = "value"
    )[, date := as.Date(date)
    ][!is.na(date)][, `:=`(
        value = as.numeric(value),
        fs_type = fr_163[row_type, type],
        fs_name = fr_163[row_type, name]
    )][,.(fs_type, fs_name, var_id, var_name, date, value)]
    
    # setkey
    setkeyv(dat_melt, c("date", "var_id"))
    
    return(dat_melt)
} 

#' @import data.table
#' @importFrom readr read_csv stop_for_problems cols
md_stock1_fs_cn = function(symbol1, type=NULL, print_step=0L, ...) {
    . = name = name_en = mkt = NULL
  
    chk_syb = check_symbol_cn(symbol1)
    symbol1 = chk_syb$syb
    # skip index
    if (chk_syb[, mkt == 'index' || is.na(mkt)]) return(NULL)
    # type is summary
    if (any(type == 'dupont')) return(fs_dupont_cn(symbol1))
    
    # symbol name
    sn = md_stock_spot_tx(symbol1, only_syb_nam = TRUE)
    
    # type
    fs_type_163 = setDT(copy(financial_statements_163))
    type = select_rows_df(dt = fs_type_163[,.(type, name, name_en)], column = 'type', input_string=type)[,type]

    # data list
    dat_list = load_dat_loop(type, "fs_type1_cn", args = list(symbol1=symbol1), print_step=print_step)
    return(cbind(sn, rbindlist(dat_list, fill = TRUE)))
}


# financial statements summary indicators
fs_dupont_cn = function(symbol1=NULL, ...) {
  var_id = value = var14_2 = var14 = fs_num = var04_q = var04 = var10_q = var10 = var01 = var02 = var19 = var03 = var16 = . = EPS = revenue = revenueYOY = revenueQOQ = NP = NPYOY = NPQOQ = ROE_w = CFPS = asset_liability = asset_turnover = profit_margin = ROA = ROE = BVPS = name = symbol = fs_month = NULL
  
  # # DuPont Analysis
  # # https://en.wikipedia.org/wiki/DuPont_analysis
  # ROE
  # ROA, return on total assets
  # profit_margin
  # asset_turnover
  # asset-liability_ratio
  
  # ROE = (Profit margin)*(Asset turnover)*(Equity multiplier) 
  #     = (Net profit/Sales)*(Sales/Average Total Assets)*(Average Total Assets/Average Equity) 
  args = list(...)
  if ('fi_main' %in% names(args)) {
    # main financial indicators
    dat = setDT(args[['fi_main']])
    # symbol name
    sn = dat[,.(symbol, name)][.N]
  } else {
    # main financial indicators
    dat = fs_type1_cn('fi0_main', symbol1)
    # symbol name
    sn = md_stock_spot_tx(symbol1, only_syb_nam = TRUE)
  }
  
  mfi2 = dcast(
    dat[var_id %in% c(1:4,10,11,12,13,14,16,18,19)
      ][, var_id := sprintf('var%02i',var_id)
      ][, value := ifelse(value==0, NA, value)], 
    date ~ var_id
  )[, `:=`(fs_num = .N, fs_month = month(date)), by = year(date)
  ][, var14_2 := (var14+shift(var14,type='lag'))/2
  ][is.na(var14_2), var14_2 := var14
  ][fs_num > 2, var04_q := var04-shift(var04, type='lag'), by = year(date)
  ][(fs_num > 2 | fs_month==3) & is.na(var04_q), var04_q := var04
  ][fs_num > 2, var10_q := var10-shift(var10, type='lag'), by = year(date)
  ][(fs_num > 2 | fs_month==3) & is.na(var10_q), var10_q := var10
  ][, `:=`(
    # http://data.eastmoney.com/bbsj/yjbb/000001.html
    EPS = var01, # EPS
    revenue = var04,
    revenueQOQ = var04_q/shift(var04_q,n=1,type='lag')*100-100,
    NP = var10,
    NPQOQ = var10_q/shift(var10_q,n=1,type='lag')*100-100,
    BVPS=var02, 
    ROE_w = var19,
    CFPS = var03,
    # http://quotes.money.163.com/f10/dbfx_000001.html#01c08
    asset_liability   = var16/var14*100,
    asset_turnover = var04/var14_2*100,
    profit_margin   = var10/var04*100,
    ROA = var10/var14*100, 
    ROE = var10/var14*1/(1-var16/var14)*100
  )][, `:=`(
    revenueYOY = var04/shift(var04,n=1,type='lag')*100-100,
    NPYOY = var10/shift(var10,n=1,type='lag')*100-100
  ), by = month(date)
   ][,.(date, EPS, revenue, revenueYOY, revenueQOQ, NP, NPYOY, NPQOQ, BVPS, ROE_w, CFPS, asset_liability, asset_turnover, profit_margin, ROA, ROE)]
  
  return(cbind(sn,mfi2))
}



#' query financial statements
#' 
#' \code{md_stock_financials} provides an interface to query financial statements and indicators of listed companies in SSE and SZSE.
#' 
#' @param symbol symbol of stock shares.
#' @param type the type of financial statements. 
#' @param print_step A non-negative integer. Print symbol name by each print_step iteration. Default is 1L.
#' 
#' @examples 
#' \dontrun{
#' # interactively specify type of financial table 
#' dat1 = md_stock_financials("000001")
#' 
#' # manually specify type of financial table
#' # type = "fr0"
#' dat2 = md_stock_financials("000001", type="fs0")
#' # or type = "fr0_summary"
#' dat3 = md_stock_financials("000001", type="fs0_summary")
#' 
#' # multiple symbols and statements
#' dat4 = md_stock_financials(c("000001", "600000"), type = "fi")
#' 
#' # dupont analysis indicators
#' fs_idx = md_stock_financials(c('000001', '^000001'), type = 'dupont')
#' 
#' }
#' 
#' @export
md_stock_financials = function(symbol, type=NULL, print_step=1L) {
  . = name = name_en = NULL
  check_internet('www.163.com')
  
  # type
  if (!any(type == 'dupont')) {
    fs_type_163 = setDT(copy(financial_statements_163))
    type = select_rows_df(dt = fs_type_163[,.(type, name, name_en)], column = 'type', input_string=type)[,type]
  }
  # if (source == "163") 
  dat_list = load_dat_loop(symbol, 'md_stock1_fs_cn', args = list(type = type), print_step=print_step)
  
  return(dat_list)
}





