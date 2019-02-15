# financial statements
# 每股收益(元)	                44
# 每股收益(扣除)(元)            45
# 营业收入: 营业收入(元)  同比增长(%)  季度环比增长(%)
# 净利润: 净利润(元)  同比增长(%)  季度环比增长(%),     
# 每股净资产(元)
# 净资产收益率(%)
# 每股经营现金流量(元)
# 销售毛利率(%)
# 利润分配
# 股息率(%)
# http://data.eastmoney.com/bbsj/yjbb/002415.html
# http://data.eastmoney.com/bbsj/yjbb/000651.html

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
    while ((is.null(type) || length(type)==0)) {
        type = select_rows_df(dt = fs_type_163[,.(type, name, name_en)], column = 'type')[,type]
    }

    # data list
    dat_list = load_dat_loop(symbol, "fs_symbol1_cn", args = list(type=type), print_step=print_step)
    return(dat_list)
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