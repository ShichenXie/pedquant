# get financial reports

# check the name of report type
check_fr_type = function(type) {
    fr_163 = setDT(copy(finance_report_163))
    
    if (is.null(type)) {
        print(fr_163[,1:3][, lapply(.SD, format)])
        sel_type = readline("Select rowids of type: ")
        
        sel_type = sprintf('c(%s)', gsub('-',':',gsub(' ','',sel_type))) 
        sel_type = eval(parse(text = sel_type))
    } else {
        sel_type = which(grepl(gsub(",|;","|",paste0(type,collapse = "|")), fr_163$type))
    }
    
    return(intersect(sel_type, fr_163[,.I]))
}

# get one type financial report of one symbol
getfr_type1_cn = function(symbol, row_type=NULL) {
    patterns = value = type = . = fr_id = fr_name = name = name_en = var_name = var_id = NULL
    
    
    # types of financial reports
    fr_163 = setDT(copy(finance_report_163))
    
    # load data from 163
    fr_url = sprintf(fr_163[row_type,]$urls, symbol)
    dat = suppressWarnings(read_csv(file=fr_url, col_types=cols(.default = "c"), locale = locale(encoding = "GBK"), na = c("", "NA", "--")))
    setnames(setDT(dat), c("var_name", names(dat)[-1]))
    
    # melt dataframe
    dat_melt = melt(
        dat[!(var_name %in% c(NA, "", "\t\t"))][, var_id := .I], 
        id=c("var_id","var_name"), measure=patterns("\\d{4}-"),
        variable.name = "date", value.name = "value"
    )[, `:=`(date = as.Date(date))
    ][!is.na(date)][, `:=`(
        value = as.numeric(value),
        fr_id = fr_163[row_type, type],
        fr_name = fr_163[row_type, name]
    )][,.(fr_id, fr_name, var_id, var_name, date, value)]
    
    # setkey
    setkeyv(dat_melt, c("date","var_id"))
    
    return(dat_melt)
} 
# get financial reports of one symbol 
getfr_symbol1_cn = function(symbol, type) {
    # check type of fr
    type = check_fr_type(type)
    
    dat_list = list()
    for (t in type) {
        dat_list[[t]] = getfr_type1_cn(symbol, t)
    }
    
    return(rbindlist(dat_list, fill = TRUE))
}


#' @import data.table
#' @importFrom readr read_csv stop_for_problems cols
getfr_cn = function(symbol, type=NULL, print_step=1) {
    
    dat_list = load_dat_loop(symbol, "getfr_symbol1_cn", args = list(type=type), print_step=print_step)
    
    return(dat_list)
}


#' get financial reports and indicators
#' 
#' \code{getfr} provides an interface to get financial reports and indicators of listed companies in China.
#' 
#' @param symbol symbol of stock in China.
#' @param type the type of financial reports. 
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1.
#' @param region only cn (China) is available.
#' 
#' @examples 
#' \dontrun{
#' # interactively specify type of financial table 
#' dat1 = getfr("000001")
#' 
#' # manually specify type of financial table
#' # type = "fr0"
#' dat2 = getfr("000001", type="fr0")
#' # or type = "fr0_summary"
#' dat3 = getfr("000001", type="fr0_summary")
#' 
#' # multiple symbols and reports
#' dat4 = getfr(c("000001", "600000"), type = "fi")
#' 
#' }
#' 
#' @export
getfr = function(symbol, type=NULL, print_step=1L, region="cn") {
    if (region == "cn") return(getfr_cn(symbol, type, print_step))
}