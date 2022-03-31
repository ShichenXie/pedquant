# nbs ------
# http://data.stats.gov.cn

# selecting nbs_url in Chinese or English
sel_nbs_url = function(eng) {
  url = 'https://data.stats.gov.cn/easyquery.htm'
  if (eng) url = 'https://data.stats.gov.cn/english/easyquery.htm'
  return(url)
}

# dimension of NBS database
#' @import data.table
dim_nbs_db = function() {
  nbs_db = setDT(list(
    dim_region   = rep('cn',8),
    dim_geo_type = rep(c('nation', 'province', 'city'), c(3,3,2)),
    dim_freq     = c("monthly","quarterly","yearly", "monthly","quarterly","yearly", "monthly","yearly"),
    dim_sta_db   = c("hgyd","hgjd","hgnd","fsyd","fsjd","fsnd","csyd","csnd")
  ))
  return(nbs_db)
}



#' @importFrom rvest html_nodes html_text %>%
nbs_read_json = function(url, eng=FALSE) {
  wb = url# load_web_source(url)
  
  dt = read_html(wb) %>% 
    html_nodes('pre') %>% 
    html_text() %>% 
    fromJSON()
  
  if (eng == FALSE) warning('The Chinese characters cannot be encoded when using phantomjs in webdriver package.')
  return(dt)
}
# query a symbol from nbs
#' @import data.table httr 
#' @importFrom jsonlite fromJSON 
nbs_symbol1 = function(geo_type=NULL, freq=NULL, symbol='zb', eng=FALSE) {
  dim_geo_type = dim_freq = dim_region = dim_sta_db = . = id = name = isParent = pid = NULL
  
  #param
  nbs_url = sel_nbs_url(eng)
  # time_sec = as.character(date_to_sec()*100)
  if (is.null(symbol)) symbol = 'zb'
  # name of geography in NBS
  nbs_dbcode = dim_nbs_db()[
    dim_region=='cn' & dim_geo_type==geo_type & dim_freq==freq , dim_sta_db]
  
  # query symbol list from nbs
  zb_func = function(nbs_url, nbs_dbcode, symbol) {
    zb_query = list(
      id=symbol, 
      dbcode=nbs_dbcode, 
      wdcode="zb", 
      m="getTree"
    )
    zb_req = POST(nbs_url, body=zb_query, encode="form")
    zb_list = fromJSON(content(zb_req, "text", encoding="utf-8"))
    return(zb_list)
  }
  zb_list = try(zb_func(nbs_url, nbs_dbcode, symbol), silent = TRUE)
  if (inherits(zb_list, 'try-error')) {
    httr::set_config(config(ssl_verifypeer = FALSE))
    zb_list = try(zb_func(nbs_url, nbs_dbcode, symbol), silent = TRUE)
  }
  
  if (inherits(zb_list, 'try-error')) {
    url_syb = sprintf('%s?id=%s&dbcode=%s&wdcode=zb&m=getTree', nbs_url, symbol, nbs_dbcode)
    zb_list = try(nbs_read_json(url_syb, eng), silent = TRUE)
  }
  if (inherits(zb_list, 'try-error')) stop('The data from NBS is not available.')
  zb_list = setDT(zb_list)[,.(symbol=id, name, is_parent=isParent, parent_symbol=pid)]
  return(zb_list)
}

#' symbol of NBS economic data
#' 
#' \code{ed_nbs_symbol} provides an interface to query symbols of economic indicators from NBS.
#' 
#' @param symbol symbols of NBS indicators.
#' @param geo_type geography type in NBS, including 'nation', 'province', 'city'. Default is NULL.
#' @param freq the frequency of NBS indicators, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param eng logical. The language of the query results is in English or in Chinese. Default is FALSE.
#' 
#' @examples 
#' # query symbol interactively
#' \dontrun{
#' sym = ed_nbs_symbol()}
#' 
#' @import data.table httr
#' @importFrom jsonlite fromJSON 
#' @importFrom utils menu data
#' @export
ed_nbs_symbol = function(symbol=NULL, geo_type=NULL, freq=NULL, eng=FALSE) {
  is_parent = NULL
  
  # geography type
  geo_type = check_arg(geo_type, choices = c("nation", "province", "city"), arg_name = 'geo_type')
  # frequency
  freq_choices = c("monthly", "quarterly", "yearly")
  if (geo_type=="city") freq_choices = c("monthly", "yearly")
  freq = check_arg(freq, choices = freq_choices, arg_name = 'freq')
  
  sel_symbol = symbol
  is_parent = TRUE
  len_symbol_df = 1
  while (is_parent & len_symbol_df>0) {
    symbol_df = nbs_symbol1(geo_type, freq, sel_symbol, eng)
    len_symbol_df = nrow(symbol_df)
    
    if (len_symbol_df>0) sel_symbol = select_rows_df(symbol_df, column='symbol', onerow=TRUE) else break
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
#' @param eng logical. The language of the query results is in English or in Chinese. Default is FALSE.
#' 
#' @examples 
#' \dontrun{
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
#' @importFrom httr set_config
#' @export
ed_nbs_subregion = function(geo_type=NULL, eng=FALSE) {
  dim_region = dim_geo_type = dim_sta_db = . = code = name = NULL
  
  # param
  nbs_url = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  # geography type
  geo_type = check_arg(geo_type, c("province", "city"), default = NULL, arg_name = 'geo_type')
  if (geo_type == 'nation') return(NULL)
  # name of geography in NBS
  nbs_dbcode = dim_nbs_db()[
    dim_region=='cn' & dim_geo_type==geo_type, ][.N,dim_sta_db]
  
  # wds
  wds='[{"wdcode":"reg","valuecode":"00"}]'
  if (geo_type == 'city') wds='[{"wdcode":"reg","valuecode":"000000"}]'
  
  # # query subregion
  dat_func = function(nbs_url, nbs_dbcode, wds, time_sec) {
    query_list = list(
      m="getOtherWds",
      dbcode=nbs_dbcode,
      rowcode='zb',
      colcode='sj',
      wds=wds,
      # dfwds=paste0('[{"wdcode":"sj","valuecode":"LAST10"}]'),
      k1=time_sec
    )
    req = try(GET(nbs_url, query=query_list), silent = TRUE)
    if (inherits(req, 'try-error')) {
        set_config(config(ssl_verifypeer = 0L))
        req = GET(nbs_url, query=query_list)
    }
    jsondat = fromJSON(content(req, "text", encoding="utf-8"))
    return(jsondat)
  }
  jsondat = try(dat_func(nbs_url, nbs_dbcode, wds, time_sec), silent = TRUE)
  
  if (inherits(jsondat, 'try-error')) {
    url_reg = sprintf('%s?m=getOtherWds&dbcode=%s&rowcode=zb&colcode=sj&wds=%s&k1=%s', nbs_url, nbs_dbcode, wds, time_sec)
    jsondat = try(nbs_read_json(url_reg, eng), silent = TRUE)
  }
  if (inherits(jsondat, 'try-error')) stop('The data from NBS is not available.')
  regdf = setDT(jsondat$returndata$nodes[[1]])[,.(code, name)]
  return(regdf)
}

#  query data # zb symbol, sj date, reg subregion
#' @importFrom jsonlite fromJSON 
ed1_nbs = function(symbol1, geo_type, subregion=NULL, from, freq, eng=FALSE) {
  dim_freq = dim_geo_type = dim_sta_db = NULL
  
  # name database
  nbs_dbcode = dim_nbs_db()[dim_geo_type==geo_type & dim_freq==freq, dim_sta_db]
  
  nbs_url = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  ## symbol
  symbol1 = ed_nbs_symbol(symbol=symbol1, geo_type, freq, eng)
  
  # date range
  freq_mqa = which(substr(nbs_dbcode, 3, 3) == c('y','j','n'))
  date_rng = Sys.Date() - as.Date(from)
  date_rng = floor( date_rng/(c(30, 90, 365)[freq_mqa]) )
  sj_value = paste0("LAST",date_rng)
  
  # subregion parm
  wds = ifelse( is.null(subregion), '[]',
    paste0('[{"wdcode":"reg","valuecode":"',subregion,'"}]') )
  
  rowcode = 'zb'
  if (!is.null(subregion)) {
    if (grepl('^fs|^cs', nbs_dbcode) & ('all' %in% subregion || length(subregion)>1)) rowcode = 'reg'
  }
  
  dfwds=paste0('[{"wdcode":"zb","valuecode":"',symbol1,'"},{"wdcode":"sj","valuecode":"',sj_value,'"}]')
  
  # query list
  dat_func = function(nbs_url, nbs_dbcode, rowcode, wds, dfwds, time_sec) {
    query_list = list(
      m="QueryData",
      dbcode=nbs_dbcode,
      rowcode=rowcode,
      colcode='sj',
      wds=wds,
      dfwds=dfwds,
      k1=time_sec
    )
    req = GET(nbs_url, query=query_list)
    jsondat = fromJSON(content(req, "text", encoding="utf-8"))
    return(jsondat)
  }
  jsondat = try(dat_func(nbs_url, nbs_dbcode, rowcode, wds, dfwds, time_sec), silent = TRUE)
  
  if (inherits(jsondat, 'try-error')) {
    url_dat = sprintf('%s?m=QueryData&dbcode=%s&rowcode=%s&colcode=sj&wds=%s&dfwds=%s&k1=%s', nbs_url, nbs_dbcode, rowcode, wds, dfwds, time_sec)
    jsondat = try(nbs_read_json(url_dat, eng), silent = TRUE)
  }
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
#' \code{ed_nbs} provides an interface to query economic data from National Bureau of Statistics of China (NBS, \url{http://www.stats.gov.cn/}).
#' 
#' @param symbol symbols of NBS indicators. It is available via \code{ed_nbs_symbol}. Default is NULL.
#' @param freq the frequency of NBS indicators, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param geo_type geography type in NBS, including 'nation', 'province', 'city'. Default is NULL.
#' @param subregion codes of province or city, which is available via \code{ed_nbs_subregion}. Default is NULL.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is '10y'.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param na_rm logical. Whether to remove missing values from datasets. Default is FALSE.
#' @param eng logical. The language of the query results is in English or in Chinese Default is FALSE.
#' 
#' @examples 
#' \dontrun{
#' # query NBS data without setting any parameters
#' dt = ed_nbs()
#' 
#' # specify paratmeters
#' dt1 = ed_nbs(geo_type='nation', freq='quarterly', symbol='A010101')
#' # or using 'n'/'q' represents 'nation'/'quarterly'
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
ed_nbs = function(symbol=NULL, freq=NULL, geo_type=NULL, subregion=NULL, date_range='10y', from=NULL, to=Sys.Date(), na_rm=FALSE, eng=FALSE) {
  code=dim_geo_type=dim_freq=dim_sta_db=geo_code=value=NULL

  # arguments
  ## geography type
  geo_type = check_arg(geo_type, c("nation", "province", "city"), arg_name = 'geo_type')
  ## frequency
  if (geo_type=="city") {
    freq = check_arg(freq, choices = c("monthly", "yearly"), arg_name = 'freq')
  } else {
    freq = check_arg(freq, choices = c("monthly", "quarterly", "yearly"), arg_name = 'freq')
  }
  
  ## subregion
  if (geo_type %in% c('province', 'city')) {
    subregion_df = ed_nbs_subregion(geo_type, eng)
    subregion = select_rows_df(subregion_df, column='code', input_string=subregion)[,code]
  }
  ## from/to
  to = check_to(to)
  from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '10y') 
  ## symbol
  if (is.null(symbol)) symbol = ed_nbs_symbol(geo_type = geo_type, freq = freq, eng = eng)
  
  # jsondat
  jsondat_list = NULL
  for (s in symbol) {
    temp = ed1_nbs(symbol1=s, geo_type, subregion, from, freq, eng)
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
    dat_lst[[i]] = temp[,geo_code := NULL]
  }
  return(dat_lst)
}

# library(data.table)
# library(httr)
# library(jsonlite)

# eu, jp, us ------