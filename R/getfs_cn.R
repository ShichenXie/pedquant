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

# get one type report
getfs1_cn = function(symbol, row_type=NULL) {
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



#' @import data.table
#' @importFrom readr read_csv stop_for_problems cols
getfs_cn = function(symbol, type=NULL, print_step=1) {
    # check type of fs
    type = check_fs_type(type)
    
    dt_list = NULL
    dt_list = NULL
    symbol_len = length(symbol)
    for (i in 1:symbol_len) {
        # symbol i
        si = symbol[i]
        # print
        if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i,symbol_len)),collapse = "/"), si,"\n")
        
        for (t in type) {
            dt_list[[si]][[t]] = getfs1_cn(si, t)
        }
        dt_list[[si]] = rbindlist(dt_list[[si]], fill = TRUE)
    }
    return(dt_list)
}


#' get financial statements and indicators
#' 
#' `getfs` provides an interface to get financial statements and indicators of listed companies in China.
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
getfs = function(symbol, type=NULL, print_step=1, region="cn") {
    if (region == "cn") return(getfs_cn(symbol, type, print_step))
}