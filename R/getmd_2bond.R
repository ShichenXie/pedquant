# http://www.chinabond.com.cn/

#' @import xml2
paths_bond_cn = function() {
    . = read_html = NULL
    
    path0 = "http://www.chinabond.com.cn"
    
    path_bond = "http://yield.chinabond.com.cn/cbweb-mn/yc/downYearBzqxList?wrjxCBFlag=0&&zblx=txy&&ycDefId=2c9081e50a2f9606010a3068cae70001&&locale=zh_CN"
    path_cur_his = xml_attr(xml_find_all(read_html(path_bond), "//td//a"), "href")
    
    # path of bond yeild in current year
    b_cur = paste0("http://yield.chinabond.com.cn", path_cur_his[2])
    
    
    # path of bond yeild in historical years
    # "http://www.chinabond.com.cn/cb/cn/zzsj/cywj/syqx/sjxz/zzgzqx/list.shtml"
    wb = read_html(path_cur_his[1])
    b_his = lapply(
        paste0(path0, xml_attr(xml_find_all(wb, "//li//span//a"), "href")),
        function(x) {
            paste0(path0, xml_attr(xml_find_all(read_html(x), "//li//span//a"), "href"))
        }
    )
    b_his = unlist(b_his)
    
    
    return(list(b_cur, b_his))
}


#' @import data.table
getmd_bond_cn = function(from = NULL, to = Sys.Date()) {
    yield = NULL
    
    dflist = lapply(unlist(paths_bond_cn()), function(x) {
        df = load_read_xl(x)
        setDT(df)
        setnames(df, c("date", "maturity", "maturity_year", "yield"))
        df = df[, `:=`(
            date = as.Date(date),
            yield = as.numeric(yield)
        )]
    })
    
    return(rbindlist(dflist, fill = TRUE))
}


