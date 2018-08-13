# get financial statements

# check the name of report type
check_fs_type = function(type) {
    fs_163 = setDT(copy(finance_statement_163))
    
    if (is.null(type)) {
        print(fs_163[,1:3][, lapply(.SD, format)])
        sel_type = readline("Select rowids of type: ")
        
        sel_type = sprintf('c(%s)', gsub('-',':',gsub(' ','',sel_type))) 
        sel_type = eval(parse(text = sel_type))
    } else {
        sel_type = which(grepl(gsub(",|;","|",paste0(type,collapse = "|")), fs_163$type))
    }
    
    return(intersect(sel_type, fs_163[,.I]))
}

# get one type financial report of one symbol
getfs_type1_cn = function(symbol, row_type=NULL) {
    patterns = value = type = . = fs_id = fs_name = name = name_en = var_name = var_id = NULL
    
    
    # types of financial statements
    fs_163 = setDT(copy(finance_statement_163))
    
    # load data from 163
    fs_url = sprintf(fs_163[row_type,]$urls, symbol)
    dat = suppressWarnings(read_csv(file=fs_url, col_types=cols(.default = "c"), locale = locale(encoding = "GBK"), na = c("", "NA", "--")))
    setnames(setDT(dat), c("var_name", names(dat)[-1]))
    
    # melt dataframe
    dat_melt = melt(
        dat[!(var_name %in% c(NA, "", "\t\t"))][, var_id := .I], 
        id=c("var_id","var_name"), measure=patterns("\\d{4}-"),
        variable.name = "date", value.name = "value"
    )[, `:=`(date = as.Date(date))
    ][!is.na(date)][, `:=`(
        value = as.numeric(value),
        fs_id = fs_163[row_type, type],
        fs_name = fs_163[row_type, name]
    )][,.(fs_id, fs_name, var_id, var_name, date, value)]
    
    # setkey
    setkeyv(dat_melt, c("date","var_id"))
    
    return(dat_melt)
} 
# get financial reports of one symbol 
getfs_symbol1_cn = function(symbol, type) {
    # check type of fs
    type = check_fs_type(type)
    
    dat_list = list()
    for (t in type) {
        dat_list[[t]] = getfs_type1_cn(symbol, t)
    }
    
    return(rbindlist(dat_list, fill = TRUE))
}


#' @import data.table
#' @importFrom readr read_csv stop_for_problems cols
getfs_cn = function(symbol, type=NULL, print_step=1) {
    
    dat_list = load_dat_loop(symbol, "getfs_symbol1_cn", args = list(type=type), print_step=print_step)
    
    return(dat_list)
}


#' get financial statements and indicators
#' 
#' \code{getfs} provides an interface to get financial statements and indicators of listed companies in China.
#' 
#' @param symbol symbol of stock in China.
#' @param type the type of financial statements. 
#' @param print_step A non-negative integer, which will print variable names by each print_step-th iteration. Default is 1.
#' @param region only cn (China) is available.
#' 
#' @examples 
#' \dontrun{
#' # interactively specify type of financial table 
#' dat1 = getfs("000001")
#' 
#' # manually specify type of financial table
#' # type = "fs0"
#' dat2 = getfs("000001", type="fs0")
#' # or type = "fs0_summary"
#' dat3 = getfs("000001", type="fs0_summary")
#' 
#' # multiple symbols and reports
#' dat4 = getfs(c("000001", "600000"), type = "fi")
#' 
#' }
#' 
#' @export
getfs = function(symbol, type=NULL, print_step=1L, region="cn") {
    if (region == "cn") return(getfs_cn(symbol, type, print_step))
}