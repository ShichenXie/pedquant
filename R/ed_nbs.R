# nbs ------
# http://data.stats.gov.cn

# selecting nbs_url in Chinese or English
sel_nbs_url = function(eng) {
  url = 'http://data.stats.gov.cn/easyquery.htm'
  if (eng) url = 'http://data.stats.gov.cn/english/easyquery.htm'
  return(url)
}

# dimension of NBS database
#' @import data.table
dim_nbs_db = function() {
  nbs_db = setDT(list(
    dim_region   = rep('cn',8),
    dim_geo_type = rep(c('national', 'province', 'city'), c(3,3,2)),
    dim_freq     = c("monthly","quarterly","yearly", "monthly","quarterly","yearly", "monthly","yearly"),
    dim_sta_db   = c("hgyd","hgjd","hgnd","fsyd","fsjd","fsnd","csyd","csnd")
  ))
  return(nbs_db)
}

# check http status
# @import httr
# check_http_status_nbs = function(x) {
#   if (http_status(x)$category != "Success") stop(http_status(x)$message)
# }

#' @importFrom webdriver run_phantomjs Session install_phantomjs
#' @importFrom rvest html_nodes html_text %>%
nbs_read_json = function(url) {
  pjs <- try(run_phantomjs(), silent = TRUE)
  if (inherits(pjs, 'try-error')) {
    cat('Installing phantomjs via webdriver::install_phantomjs ...\n')
    install_phantomjs()
  }
  ses <- Session$new(port = pjs$port)
  ses$go(url)
  
  dt = read_html(ses$getSource()) %>% 
    html_nodes('pre') %>% 
    html_text() %>% 
    fromJSON()
  return(dt)
}
# query a symbol from nbs
#' @import data.table httr 
#' @importFrom jsonlite fromJSON 
nbs_symbol1 = function(geo_type=NULL, freq=NULL, symbol='zb', eng=TRUE) {
  dim_geo_type = dim_freq = dim_region = dim_sta_db = . = id = name = isParent = pid = NULL
  
  #param
  url_nbs = sel_nbs_url(eng)
  # time_sec = as.character(date_to_sec()*100)
  if (is.null(symbol)) symbol = 'zb'
  # name of geography in NBS
  nbs_geo = dim_nbs_db()[
    dim_region=='cn' & dim_geo_type==geo_type & dim_freq==freq , dim_sta_db]
  
  # query symbol list from nbs
  url_syb = sprintf('%s?id=%s&dbcode=%s&wdcode=zb&m=getTree', url_nbs, symbol, nbs_geo)
  # zb_query = list(m="getTree", dbcode=nbs_geo, wdcode="zb", id=symbol)
  # zb_req = POST(url_nbs, body=zb_query, encode="form")
  # check_http_status_nbs(zb_req)
  # zb_list = fromJSON(content(url_syb, "text", encoding="utf-8"))
  
  zb_list = try(nbs_read_json(url_syb), silent = TRUE)
  if (inherits(zb_list, 'try-error')) stop('The data from NBS is not available.')
  zb_list = setDT(zb_list)[,.(symbol=id, name, is_parent=isParent, parent_symbol=pid)]
  return(zb_list)
}

#' symbol of NBS economic data
#' 
#' \code{ed_nbs_symbol} provides an interface to query symbols of economic indicators from NBS.
#' 
#' @param geo_type geography type in NBS, including 'national', 'province', 'city'. Default is NULL.
#' @param freq the frequency of NBS indicators, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param eng logical. The language of the query results is in English or in Chinese. Default is FALSE.
#' 
#' @examples 
#' # query symbol interactively
#' \donttest{
#' sym = ed_nbs_symbol()}
#' 
#' @import data.table httr
#' @importFrom jsonlite fromJSON 
#' @importFrom utils menu data
#' @export
ed_nbs_symbol = function(geo_type=NULL, freq=NULL, eng=TRUE) {
  symbol = is_parent = NULL
  
  if (eng == FALSE) warning('The Chinese characters cannot be encoded when using phantomjs in webdriver package.')
  
  # geography type
  geo_type = check_arg(geo_type, choices = c("national", "province", "city"), arg_name = 'geo_type')
  # frequency
  if (geo_type=="city") {
    freq = check_arg(freq, choices = c("monthly", "yearly"), arg_name = 'freq')
  } else {
    freq = check_arg(freq, choices = c("monthly", "quarterly", "yearly"), arg_name = 'freq')
  }
  
  sel_symbol = NULL
  is_parent = TRUE
  while (is_parent) {
    symbol_df = nbs_symbol1(geo_type, freq, sel_symbol, eng)
    sel_symbol = select_rows_df(symbol_df, column='symbol', onerow=TRUE)
    is_parent = sel_symbol[, is_parent]
    sel_symbol = sel_symbol[, symbol]
  }
  return(sel_symbol)
}



#' subregion code of NBS economic data
#' 
#' \code{ed_nbs_subregion} query province or city code from NBS
#' 
#' @param geo_type geography type in NBS, including 'province', 'city'. Default is NULL.
#' @param eng logical. The language of the query results is in English or in Chinese. Default is TRUE.
#' 
#' @examples 
#' \donttest{
#' # province code 
#' prov1 = ed_nbs_subregion(geo_type = 'province') 
#' # or using 'p' represents 'province'
#' prov2 = ed_nbs_subregion(geo_type = 'p') 
#' 
#' # city code in Chinese
#' # city = ed_nbs_subregion(geo_type = 'c', eng = FALSE) 
#' # city code in English
#' city = ed_nbs_subregion(geo_type = 'c', eng = TRUE) 
#' }
#' @importFrom jsonlite fromJSON 
#' @export
ed_nbs_subregion = function(geo_type=NULL, eng=TRUE) {
  dim_region = dim_geo_type = dim_sta_db = . = code = name = NULL
  
  if (eng == FALSE) warning('The Chinese characters cannot be encoded when using phantomjs in webdriver package.')
  
  # param
  url_nbs = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  # geography type
  geo_type = check_arg(geo_type, c("province", "city"), default = NULL, arg_name = 'geo_type')
  if (geo_type == 'national') return(NULL)
  # name of geography in NBS
  nbs_geo = dim_nbs_db()[
    dim_region=='cn' & dim_geo_type==geo_type, ][.N,dim_sta_db]
  
  # wds
  wds='[{"wdcode":"reg","valuecode":"00"}]'
  if (geo_type == 'city') wds='[{"wdcode":"reg","valuecode":"000000"}]'
  
  # # query subregion
  # query_list = list(
  #   m="getOtherWds",
  #   dbcode=nbs_geo,
  #   rowcode='zb',
  #   colcode='sj',
  #   wds=wds, 
  #   # dfwds=paste0('[{"wdcode":"sj","valuecode":"LAST10"}]'),
  #   k1=time_sec
  # )
  # req = GET(url_nbs, query=query_list)
  # check_http_status_nbs(req)
  # jsondat = fromJSON(content(req, "text", encoding="utf-8"))
  
  url_reg = sprintf('%s?m=getOtherWds&dbcode=%s&rowcode=zb&colcode=sj&wds=%s&k1=%s', url_nbs, nbs_geo, wds, time_sec)
  jsondat = try(nbs_read_json(url_reg), silent = TRUE)
  if (inherits(jsondat, 'try-error')) stop('The data from NBS is not available.')
  regdf = setDT(jsondat$returndata$nodes[[1]])[,.(code, name)]
  return(regdf)
}

#  query data # zb symbol, sj date, reg subregion
#' @importFrom jsonlite fromJSON 
ed1_nbs = function(nbs_geo, symbol1, subregion=NULL, from, eng=TRUE) {
  url_nbs = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  # date range
  freq_mqa = which(substr(nbs_geo, 3, 3) == c('y','j','n'))
  date_rng = Sys.Date() - as.Date(from)
  date_rng = floor( date_rng/(c(30, 90, 365)[freq_mqa]) )
  sj_value = paste0("LAST",date_rng)
  
  # subregion parm
  wds = ifelse( is.null(subregion), '[]',
    paste0('[{"wdcode":"reg","valuecode":"',subregion,'"}]') )
  
  rowcode = 'zb'
  if (!is.null(subregion)) {
    if (grepl('^fs|^cs', nbs_geo) & ('all' %in% subregion || length(subregion)>1)) rowcode = 'reg'
  }
  
  dfwds=paste0('[{"wdcode":"zb","valuecode":"',symbol1,'"},{"wdcode":"sj","valuecode":"',sj_value,'"}]')
  
  # # query list
  # query_list = list(
  #   m="QueryData",
  #   dbcode=nbs_geo,
  #   rowcode=rowcode,
  #   colcode='sj',
  #   wds=wds,
  #   dfwds=dfwds,
  #   k1=time_sec
  # )
  # req = GET(url_nbs, query=query_list)
  # check_http_status_nbs(req)
  # jsondat = fromJSON(content(req, "text", encoding="utf-8"))
  url_dat = sprintf('%s?m=QueryData&dbcode=%s&rowcode=%s&colcode=sj&wds=%s&dfwds=%s&k1=%s', url_nbs, nbs_geo, rowcode, wds, dfwds, time_sec)
  jsondat = try(nbs_read_json(url_dat), silent = TRUE)
  if (inherits(jsondat, 'try-error')) stop('The data from NBS is not available.')
  return(jsondat)
}

# transform json data into dataframe
# symbol, variable, date, value, unit, desc
nbs_jsondat_format = function(jsondat) {
  symbol=name=value=geo_code=geo = code = . = unit = cname = strdata = NULL
  
  # data
  dat = jsondat$returndata$datanodes$data
  setDT(dat)
  
  # code names
  code_names = jsondat$returndata$wdnodes$wdcode
  code_names2 = sub('sj','date',sub('reg','geo_code',sub('zb','symbol',code_names)))
  # code dataframe
  code_df = data.table(code = jsondat$returndata$datanodes$code)[
    , tstrsplit(code, "_", fixed=TRUE) 
  ][,lapply(.SD, function(x) sub('([a-z]*.)','',x))]
  setnames(code_df, code_names2)
  

  # merge symbol with data
  syb_df = setDT(jsondat$returndata$wdnodes$nodes[[which('zb' == code_names)]])[,.(unit, name=cname, symbol=code)]
  dat2 = cbind(
    code_df, dat[,.(value= as.numeric(gsub(',| ','',strdata)))]
  )[syb_df, on='symbol']
  
  # merge subregion with data
  if ('reg' %in% code_names) {
    reg_df = setDT(jsondat$returndata$wdnodes$nodes[[which('reg' == code_names)]])[,.(geo=cname, geo_code=code)]
    
    dat2 = dat2[reg_df, on = 'geo_code']
  } else {
    dat2 = dat2[, `:=`(geo_code = 'cn', geo = 'china')]
  }
  dat2 = dat2[,.(symbol, name, date, value, geo_code, geo, unit)]
  
  # date formating
  if (dat2[,unique(nchar(date))]==6) { # monthly
    dat2[, date := paste0(date,'01')]
  } else if (dat2[,unique(nchar(date))]==5) { # quarterly
    dat2[grepl('A$',date), date := paste0(substr(date,1,4),'0101')
       ][grepl('B$',date), date := paste0(substr(date,1,4),'0401')
       ][grepl('C$',date), date := paste0(substr(date,1,4),'0701')
       ][grepl('D$',date), date := paste0(substr(date,1,4),'1001')]
  } else { # yearly
    dat2[, date := paste0(date,'0101')]
  }
  dat2[, date := as.Date(date, format='%Y%m%d')]
  
  # set date as key
  setkeyv(dat2, c("symbol", "date"))
  return(dat2)
}


#' query NBS economic data
#' 
#' \code{ed_nbs} provides an interface to query economic data from National Bureau of Statistics of China (NBS, http://data.stats.gov.cn/).
#' 
#' @param symbol symbols of NBS indicators. It is available via \code{ed_nbs_symbol}. Default is NULL.
#' @param freq the frequency of NBS indicators, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param geo_type geography type in NBS, including 'national', 'province', 'city'. Default is NULL.
#' @param subregion codes of province or city, which is available via \code{ed_nbs_subregion}. Default is NULL.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is '10y'.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param na_rm logical. Whether to remove missing values from datasets. Default is FALSE.
#' @param eng logical. The language of the query results is in English or in Chinese Default is TRUE.
#' 
#' @examples 
#' \donttest{
#' # query NBS data without setting any parameters
#' dt = ed_nbs()
#' 
#' # specify paratmeters
#' dt1 = ed_nbs(geo_type='national', freq='quarterly', symbol='A010101')
#' # or using 'n'/'q' represents 'national'/'quarterly'
#' dt2 = ed_nbs(geo_type='n', freq='q', symbol='A010101')
#' 
#' 
#' # query data in one province
#' dt3 = ed_nbs(geo_type='province', freq='quarterly', 
#'   symbol='A010101', subregion='110000')
#'   
#' # query data in all province
#' dt4 = ed_nbs(geo_type='province', freq='quarterly', 
#'   symbol='A010101', subregion='all')
#' }
#' 
#' @import data.table
#' @export
ed_nbs = function(symbol=NULL, freq=NULL, geo_type=NULL, subregion=NULL, date_range='10y', from=NULL, to=Sys.Date(), na_rm=FALSE, eng=TRUE) {
  code=dim_geo_type=dim_freq=dim_sta_db=geo_code=value=NULL
  
  if (eng == FALSE) warning('The Chinese characters cannot be encoded when using phantomjs in webdriver package.')
  # arguments
  ## geography type
  geo_type = check_arg(geo_type, c("national", "province", "city"), arg_name = 'geo_type')
  ## frequency
  if (geo_type=="city") {
    freq = check_arg(freq, choices = c("monthly", "yearly"), arg_name = 'freq')
  } else {
    freq = check_arg(freq, choices = c("monthly", "quarterly", "yearly"), arg_name = 'freq')
  }
  ## symbol
  if (is.null(symbol)) symbol = ed_nbs_symbol(geo_type, freq, eng)
  ## subregion
  if (geo_type %in% c('province', 'city')) {
    subregion_df = ed_nbs_subregion(geo_type, eng)
    subregion = select_rows_df(subregion_df, column='code', input_string=subregion)[,code]
  }
  ## from/to
  ft = get_fromto(date_range, from, to, min_date = "1000-01-01", default_date_range = '10y')
  from = ft$f
  to = ft$t
  
  # jsondat
  jsondat_list = NULL
  # name database
  nbs_geo = dim_nbs_db()[dim_geo_type==geo_type & dim_freq==freq, dim_sta_db]
  for (s in symbol) {
    temp = ed1_nbs(nbs_geo, symbol1=s, subregion, from, eng)
    temp = nbs_jsondat_format(temp)[date>=from & date<=to,]
    if (!is.null(subregion)) temp = temp[geo_code %in% subregion]
    if (na_rm) temp = temp[!is.na(value)]
    jsondat_list[[s]] = temp
  }
  dat = rbindlist(jsondat_list, fill = TRUE)
  
  # data list
  sybs = dat[,unique(symbol)]
  dat_lst = NULL
  for (i in sybs) {
    temp = dat[symbol==i]
    setkeyv(temp, c('geo_code','date'))
    dat_lst[[i]] = temp
  }
  return(dat_lst)
}

# library(data.table)
# library(httr)
# library(jsonlite)

# eu, jp, us ------