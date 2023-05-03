#' query financial statements
#' 
#' \code{md_stock_financials} provides an interface to query financial statements for all listed companies in SSE and SZSE by specified report date.
#' 
#' @param type the type of financial statements. 
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is '3y'.
#' @param from the start date. Default is NULL.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer. Print financial statements name by each print_step iteration. Default is 1L.
#' @param ... Additional parameters.
#' 
#' @examples 
#' \dontrun{
#' # interactively specify type of financial table 
#' dtfs1 = md_stock_financials(type="fs0_summary", to = '2022-12-31')
#' dtfs2 = md_stock_financials(type="fs0_summary", to = c('2022-12-31', '2023-03-31'))
#' dtfs3 = md_stock_financials(type="fs0_summary", from = '2022-12-31', to = Sys.Date())
#' 
#' # all statements
#' dtfs4 = md_stock_financials(type = "fs", to = '2022-12-31')
#' 
#' # setting column names to Chinese
#' dtfs5 = md_stock_financials(type="fs0_summary", to = '2022-12-31', colnam_chn = TRUE)
#' 
#' }
#' 
#' @export
md_stock_financials = function(type=NULL, date_range='1q', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = typ = nam = nam_en = NULL
    
    if (length(to) == 1) {
        from = date_from(date_range, to)
        date = unique(date_eop('q', seq.Date(as_date(from), as_date(to), by = 'quarter'))) 
    } else if (length(to) > 1) {
        date = unique(as_date(to))
    }
        
    fs_typ_em = setDT(copy(fs_typ_em))
    if (is.null(type)) type = select_rows_df(dt = fs_typ_em[,.(type=typ, name=nam, name_en=nam_en)], column = 'type', input_string=type)[,type]
    
    datlst = load_dat_loop(fs_typ_em[typ %in% type, typ], 'md_stock_fs1', args = list(date = date, ...), print_step=print_step)
    
}


md_stock_fs1 = function(typ1='fs0_summary', date='2022-12-31', colnam_chn = FALSE) {
    typ = report_name = type1 = NULL
    
    
    date = as_date(date)
    
    datlst = lapply(date, function(di) {
        # print(di)
        pgi = 1
        msg = 'ok'
        fslst = list()
        
        while (msg == 'ok') {
            reportdate = ifelse(typ1 == 'fs0_summary', 'REPORTDATE', 'REPORT_DATE')
            url = sprintf("https://datacenter-web.eastmoney.com/api/data/v1/get?sortColumns=NOTICE_DATE,SECURITY_CODE&sortTypes=-1,-1&pageSize=500&pageNumber=%s&reportName=%s&columns=ALL&filter=(%s='%s')", pgi, setDT(copy(fs_typ_em))[typ == typ1, report_name], reportdate, as_date(di))
            # filter=(SECURITY_TYPE_CODE in ('058001001','058001008'))(TRADE_MARKET_CODE!='069001017')(REPORT_DATE='2020-12-31')
            
            dmp = fromJSON(read_api_eastmoney(url))
            msg = dmp$message
            # cat(pgi, msg, '\n')
            
            if (msg == 'ok') fslst[[pgi]] = dmp$result$data 
            pgi = pgi + 1
        }
        
        fsdt = rbindlist(fslst, fill = TRUE)
    })
    
    dat = rbindlist(datlst, fill = TRUE)
    setnames(dat, c('SECUCODE', 'SECURITY_NAME_ABBR', 'REPORTDATE', 'REPORT_DATE', 'NOTICE_DATE'), c('symbol', 'name', 'date_report', 'date_report', 'date_notice'), skip_absent = TRUE)
    
    
    if (isTRUE(colnam_chn)) {
        colnam = setDT(copy(fs_colnam_em))[typ == typ1 & colnam_chn != '_']
        setnames(dat, colnam$colnam_eng, colnam$colnam_chn, skip_absent = TRUE)
    }
    
    
    # "TRADE_MARKET_CODE", "SECURITY_TYPE_CODE", "ORG_CODE"
    
    # [1] "SECUCODE", "SECURITY_CODE", "INDUSTRY_CODE", "ORG_CODE", "SECURITY_NAME_ABBR" "INDUSTRY_NAME", "MARKET", "SECURITY_TYPE_CODE", "TRADE_MARKET_CODE", "DATE_TYPE_CODE"     "REPORT_TYPE_CODE", "DATA_STATE", "NOTICE_DATE", "REPORT_DATE"  
    
    return(dat)
}