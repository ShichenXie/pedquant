# http://www.chinabond.com.cn/
# http://www.worldgovernmentbonds.com/

# bond symbol of us, fred 
freq_usbond = c('1m', '3m', '6m', '1y', '2y', '3y', '5y', '7y', '10y', '20y', '30y')
bond_symbol_fred1 = setDT(list(
    symbol = paste0('us', freq_usbond, 'dy_b'),
    name = paste('United States', toupper(freq_usbond), 'Bond Daily Yield'),
    symbol_fred = c('DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS3', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30')
))

# bond symbol of other, fred
reg_nam = c('AU','AT','BE','CA','CL','DK','FI','FR','DE','GR','HU','IS','IE','IL','IT','JP','MX','NZ','NO','PL','PT','SI','ZA','ES','SE','CH','CZ','NL','KR','RU','SK','GB')
bond_symbol_fred2 = setDT(list(
    symbol = paste0(tolower(reg_nam),'10ymy_b'),
    name = paste(c('Australia','Austria','Belgium','Canada','Chile','Denmark','Finland','France','Germany','Greece','Hungary','Iceland','Ireland','Israel','Italy','Japan','Mexico','New Zealand','Norway','Poland','Portugal','Slovenia','South Africa','Spain','Sweden','Switzerland','Czech Republic','Netherlands','South Korea','Russia','Slovakia','United Kingdom'),'10Y Bond Monthly Yield'),
    symbol_fred = paste0('IRLTLT01', reg_nam, 'M156N')
))

bond_symbol_fred = rbind(bond_symbol_fred1,bond_symbol_fred2)

# bond symbol of China, chinabond.com
freq_chinabond = c('0d','1m','2m','3m','6m','9m','1y','3y','5y','7y','10y','15y','20y','30y','40y','50y')
bond_symbol_chinabond = setDT(list(
    symbol = paste0('cn', freq_chinabond, 'dy_b'),
    name = paste('China', toupper(freq_chinabond), 'Bond Daily Yield')
))

# bond symbol
bond_symbol = rbind(bond_symbol_fred, bond_symbol_chinabond, fill=TRUE)[c(9,43,27,20,39,19,37,30,28,26,14,35,54,41),main:=TRUE]#[order(main)]
func_bond_symbol = function() bond_symbol

#' @import xml2
urls_bond_chinabond = function() {
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
md_bond_chinabond = function(symbol, from=NULL, to=Sys.Date(), print_step=1L) {
    value = . = maturity = NULL
    
    symbol = tolower(symbol)
    syb_len = length(symbol)
    # years between from and to
    fromto_y = lapply(list(from=from, to=to), function(x) {
        # year of from/to
        y = as.integer(substr(x,1,4))
        # current year
        cur_year = as.integer(substr(Sys.Date(),1,4))
        # check year
        y = ifelse(y < 2002, 2002, ifelse(y > cur_year, cur_year, y))
        return(y)
    })
    years = seq(fromto_y$from, fromto_y$to)
    # urls between from and to
    urls_chinabond = unlist(urls_bond_chinabond())
    urls = urls_chinabond[which(grepl(paste0(years,collapse = '|'), urls_chinabond))]
    
    # load data
    dflist = lapply(urls, function(x) {
        df = load_read_xl(x)
        setDT(df)
        setnames(df, c("date", "maturity", "maturity_year", "value"))
        df = df[, `:=`(
            date = as.Date(date),
            value = as.numeric(value)
        )][,.(symbol=paste0('cn',maturity,'dy_b'), name=paste('China', toupper(maturity), 'Bond Daily Yield'), date, value)]
    })
    dflist = rbindlist(dflist, fill = TRUE)[,`:=`(
        geo = 'china', unit = 'Percent'
    )]
    
    # return data list
    dt_list = list()
    for (i in seq_len(syb_len)) {
        syb_i = symbol[i]
        # print step info
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, syb_len)), collapse = '/'), syb_i))
        dt_list[[syb_i]] = setDT(dflist[symbol == syb_i], key = 'date')
    }
    
    return(dt_list)
}


# query bond from FRED
md_bond1_fred = function(syb, from, to) {
    symbol = symbol_fred = . = name = value = geo = NULL
    
    # libor in history
    dt_bond_hist = ed_fred(
        bond_symbol_fred[symbol == syb, symbol_fred], from=from, to=to, print_step=0L
    )[[1]][,`:=`(symbol_fred = symbol, symbol = NULL, name = NULL
    )][bond_symbol_fred, on='symbol_fred', nomatch=0
       ][, .(symbol, name, date, value, geo, unit)
         ][!is.na(value)]
    
    setkey(dt_bond_hist, 'date')
    # return
    return(dt_bond_hist)
}
md_bond_fred = function(symbol, from=NULL, to=Sys.Date(), print_step=1L) {
    # symbols
    syb = tolower(symbol)
    syb_len = length(syb)
    if (syb_len == 0) return(NULL)
    
    # data
    dat_list = load_dat_loop(syb, "md_bond1_fred", args = list(from = from, to = to), print_step=print_step)
    return(dat_list)
}

# query bond data
# 
# 
# @export
md_bond = function(symbol=NULL, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = name = NULL
    
    # arguments
    syb = tolower(symbol)
    ## symbol
    if (is.null(symbol)) {
        syb = select_rows_df(bond_symbol[,.(symbol,name)], column='symbol')[,symbol]
    } else if (length(symbol)==1) {
        syb = select_rows_df(bond_symbol[,.(symbol,name)], column='symbol', input_string=syb)[,symbol]
    }
    syb = intersect(syb, bond_symbol$symbol)
    ## from/to
    ft = get_fromto(date_range, from, to, min_date = "1000-01-01", default_date_range = '3y')
    from = ft$f
    to = ft$t
    
    # data
    dt_list = c(
        do.call(md_bond_fred, args = list(symbol=syb[!grepl('^cn',syb)], from=from, to=to, print_step=print_step)), 
        do.call(md_bond_chinabond, args = list(symbol=syb[grepl('^cn',syb)], from=from, to=to, print_step=print_step))
    )
    return(dt_list)
}

