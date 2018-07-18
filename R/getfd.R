# get financial data

#' get financial statement data of Chinese stock
#' 
#' `getfd_cn` gets the financial statement tables of Chinese stock.  
#' 
#' @param symbol symbol of Chinese stock
#' @param type the type of financial statement table
#' 
#' @examples 
#' \dotrun{
#' dat1 = getfd_cn("000001")
#' 
#' dat2 = getfd_cn("000001", type="fs0")
#' 
#' dat3 = getfd_cn("000001", type="fs0_summary")
#' 
#' }
#' 
#' @import data.table
#' @importFrom readr read_csv
#' @export
getfd_cn = function(symbol, type=NULL) {
    fs_163 = setDT(copy(finance_statement_163))
    
    if (is.null(type)) {
        type_no = menu(fs_163[, paste(sub("(.*)_.+","\\1",type), format(name), format(name_en))], cat('Specify the type of financial statement table:'))
        type = fs_163$type[type_no]
    }
    
    # print the name of finacial statement table
    row_type = which(grepl(type, fs_163$type))
    name_type = fs_163[row_type, 2:3]
    names(name_type) = NULL
    cat(unlist(name_type), "\n")
    
    # load data from 163
    fs_url = sprintf(fs_163[row_type,]$urls, symbol)
    dat = try(setDT(read_csv(file=fs_url, locale = locale(encoding = "GBK")))) 
    dat[dat=="--"] = NA
    setnames(dat, c("var_name", names(dat)[-1]))
    
    
    dat_melt = melt(dat[which(!grepl("\\s", dat$var_name))][!is.na(var_name)][, var_id := .I], id=c("var_id","var_name"), variable.name = "date")[, date := as.Date(date)][!is.na(date)]
    setkeyv(dat_melt, c("date","var_id"))
    
    return(dat_melt)
} 
