
# - balance sheet: a company's assets, liabilities, and owners equity at a given point in time.
# - income statement: a company's income, expenses, and profits over a stated period of time. 
# - statement of changes in equity or equity statement: the changes in equity of the company over a stated period of time.
# - cash flow statement: a company's cash flow activities, particularly its operating, investing and financing activities over a stated period of time.


# query one type financial statement of one symbol
fs_type1_cn = function(symbol, row_type=NULL) {
    patterns = value = type = . = fr_id = fr_name = name = name_en = var_name = var_id = NULL
    
    
    # types of financial statements
    fr_163 = setDT(copy(financial_statements_163))
    setkey(fr_163, 'type')
    
    # load data from 163
    fr_url = sprintf(fr_163[row_type,urls], symbol)
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
# query financial statements of one symbol 
fs_symbol1_cn = function(symbol, type) {
    dat_list = list()
    for (t in type) {
        dat_list[[t]] = fs_type1_cn(symbol, t)
    }
    
    return(rbindlist(dat_list, fill = TRUE))
}


#' @import data.table
#' @importFrom readr read_csv stop_for_problems cols
fs_cn = function(symbol, type=NULL, print_step=1L) {
    # type
    fs_type_163 = setDT(copy(financial_statements_163))
    type = select_rows_df(dt = fs_type_163[,.(type, name, name_en)], column = 'type', input_string=type)[,type]

    # data list
    dat_list = load_dat_loop(symbol, "fs_symbol1_cn", args = list(type=type), print_step=print_step)
    return(dat_list)
}


# financial statements summary indicators
fs_cn1_summary = function(symbol1) {
    # '1, 每股收益',                      EPS, Earning Per Share, EPS
    # '2, 每股净资产',                    BVPS, book value per share
    # '3, 每股现金流',                    Cash Flow From Operations Per Share
    # 
    # '4 , 主营业务收入',                  Income from main operation,
    # '10, 净利润',                       Net Income,
    # '11, 净利润_扣除非经常性损益后',
    # '12, 经营活动产生的现金流量净额',
    # '13, 现金及现金等价物净增加额',
    # '14, 总资产',                      Total Assets
    # '16, 总负债',                      Total Liabilities
    # '18, 股东权益',                    Common Stockholders’ Equity
    # '19, 净资产收益率',                ROE, Rate of Return on Common Stockholders’ Equity
    # 
    # 
    # # DuPont Analysis
    # # https://en.wikipedia.org/wiki/DuPont_analysis
    # 净资产收益率 = 总资产收益率 * 1/(1-资产负债率)
    # 总资产收益率 = 销售净利率 * 总资产周转率
    # 销售净利率   = 净利润/营业收入
    # 总资产周转率 = 营业收入/总资产
    # 资产负债率   = 总负债/总资产

    
    mfi2 = dcast(
        fs_type1_cn(symbol1, 'fi0_main')[
            var_id %in% c(1:4,10,11,12,13,14,16,18,19)
        ][, var_id := sprintf('var%02i',var_id)
        ][, value := ifelse(value==0, NA, value)], 
        date ~ var_id
    )[, `:=`(fs_num = .N, fs_month = month(date)), by = year(date)
    
    ][,               var14_2 := (var14+shift(var14,type='lag'))/2
    ][is.na(var14_2), var14_2 := var14

    ][fs_num > 2,                  var04_q := var04-shift(var04, type='lag'), by = year(date)
    ][fs_num > 2 & is.na(var04_q), var04_q := var04
      
    ][fs_num > 2,                  var10_q := var10-shift(var10, type='lag'), by = year(date)
    ][fs_num > 2 & is.na(var10_q), var10_q := var10
      
    ][, `:=`(
        # http://data.eastmoney.com/bbsj/yjbb/000001.html
        每股收益 = var01, # 每股收益
        主营业务收入 = var04,
        主营业务收入QOQ = var04_q/shift(var04_q,n=1,type='lag')*100-100,
        净利润 = var10,
        净利润QOQ = var10_q/shift(var10_q,n=1,type='lag')*100-100,
        每股净资产=var02, 
        roe = var19,
        每股经营现金流量 = var03,
        # http://quotes.money.163.com/f10/dbfx_000001.html#01c08
        资产负债率   = var16/var14*100,
        总资产周转率 = var04/var14_2*100,
        销售净利率   = var10/var04*100,
        总资产收益率 = var10/var14*100, 
        净资产收益率 = var10/var14*1/(1-var16/var14)*100
    )][, `:=`(
        主营业务收入YOY = var04/shift(var04,n=1,type='lag')*100-100,
        净利润YOY = var10/shift(var10,n=1,type='lag')*100-100
    ), by = month(date)
    ][,.(date, 每股收益, 主营业务收入, 主营业务收入YOY, 主营业务收入QOQ, 净利润, 净利润YOY, 净利润QOQ, 每股净资产, roe, 每股经营现金流量, 资产负债率, 总资产周转率, 销售净利率, 总资产收益率, 净资产收益率)]
    
    return(mfi2)
}



#' get financial statements
#' 
#' \code{md_stock_financials} provides an interface to query financial statements and indicators of listed companies in SSE and SZSE.
#' 
#' @param symbol symbol of stock in China.
#' @param type the type of financial statements. 
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1.
#' @param region only cn (China) is available.
#' 
#' @examples 
#' \dontrun{
#' # interactively specify type of financial table 
#' dat1 = md_stock_financials("000001")
#' 
#' # manually specify type of financial table
#' # type = "fr0"
#' dat2 = md_stock_financials("000001", type="fr0")
#' # or type = "fr0_summary"
#' dat3 = md_stock_financials("000001", type="fr0_summary")
#' 
#' # multiple symbols and statements
#' dat4 = md_stock_financials(c("000001", "600000"), type = "fi")
#' 
#' }
#' 
#' @export
md_stock_financials = function(symbol, type=NULL, source="163", print_step=1L) {
    if (source == "163") return(fs_cn(symbol, type, print_step))
}





