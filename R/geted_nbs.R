# nbs ------
# http://data.stats.gov.cn

# selecting nbs_url in Chinese or English
sel_nbs_url = function(eng) {
  nbs_url = 'http://data.stats.gov.cn/easyquery.htm'
  if (eng) nbs_url = 'http://data.stats.gov.cn/english/easyquery.htm'
  
  return(nbs_url)
}

#' @import data.table
df_nbs_geo = function() {
  dim_region = rep('cn',8)
  dim_geo_type = rep(c('national', 'province', 'city'), c(3,3,2))
  dim_freq = c("monthly","quarterly","yearly", "monthly","quarterly","yearly", "monthly","yearly")
  dim_sta_db = c("hgyd","hgjd","hgnd","fsyd","fsjd","fsnd","csyd","csnd")
  
  dfdim_sta_db = data.table(dim_region, dim_geo_type, dim_freq, dim_sta_db)
  return(dfdim_sta_db)
}

# check frequency
check_arg_nbsfreq = function(freq, geo_type) {
  # frequency_list
  freq_list = c("monthly", "quarterly", "yearly")
  if (geo_type=="city") freq_list = setdiff(freq_list, "quarterly")
  
  freq = check_arg(freq, freq_list)
  return(freq)
}

# check http status
#' @import httr
check_http_status_nbs = function(x) {
  if (http_status(x)$category != "Success") stop(http_status(x)$message)
}

# get symbols in nbs
#' @import data.table httr
#' @importFrom jsonlite fromJSON 
geted_nbs_1symbol = function(geo_type=NULL, freq=NULL, symbol='zb', eng=FALSE) {
  dim_geo_type = dim_freq = dim_region = dim_sta_db = . = id = name = isParent = pid = NULL
  
  #param
  url_nbs = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  if (is.null(symbol)) symbol = 'zb'
  # name of geography in NBS
  nbs_geo = df_nbs_geo()[
    dim_region=='cn' & dim_geo_type==geo_type & dim_freq==freq , dim_sta_db]
  
  # query symbol list from nbs
  zb_query = list(m="getTree", dbcode=nbs_geo, wdcode="zb", id=symbol)
  zb_req = POST(url_nbs, body=zb_query, encode="form")
  check_http_status_nbs(zb_req)
  zb_list = fromJSON(content(zb_req, "text", encoding="utf-8"))
  
  zb_list = setDT(zb_list)[,.(symbol=id, name, isParent, symbolParent=pid)]
  return(zb_list)
}
#' get symbol of China economic indicator from NBS
#' 
#' \code{geted_nbs_symbol} interactively get economic data symbol from NBS.
#' 
#' @param geo_type geography type in NBS, including 'national', 'province', 'city'. Default is NULL.
#' @param freq the frequency of indicators in NBS, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param eng logical. If it is FALSE, the result is show in Chinese, otherwise in English. Default is FALSE. 
#' 
#' @examples 
#' # get symbol interactively
#' \dontrun{
#' sym = geted_nbs_symbol()}
#' 
#' @import data.table httr
#' @importFrom jsonlite fromJSON 
#' @importFrom utils menu data
#' @export
geted_nbs_symbol = function(geo_type=NULL, freq=NULL, eng=FALSE) {
  symbol = isParent = NULL
  
  # geography type
  geo_type = check_arg(geo_type, c("national", "province", "city"))
  # frequency
  freq = check_arg_nbsfreq(freq, geo_type)
  
  selecting = TRUE
  while (selecting) {
  # type in selected symbol
    sel_symbol = NULL
    is_parent = TRUE
    while (is_parent) {
      symbol_df = geted_nbs_1symbol(geo_type, freq, sel_symbol, eng)
      print(setDF(copy(symbol_df)))
      
      sel_symbol = readline("Select a symbol (until isParent==FALSE): ")
      if ( sel_symbol %in% symbol_df$symbol ) {
        is_parent = symbol_df[symbol==sel_symbol, isParent]
      } else if (sel_symbol %in% symbol_df$symbolParent) {
        is_parent = TRUE
      } else {
        sel_symbol = readline("Incorrect input. Select the symbol again: ")
      }
    }
    selecting = menu(c("yes", "no"), title="Choose this symbol?")-1
  }
   
  return(sel_symbol)
}



#' get code of subregion in China from NBS
#' 
#' \code{geted_nbs_region} get province or city code from NBS
#' 
#' @param geo_type geography type in NBS, including 'national', 'province', 'city'. Default is NULL.
#' @param eng logical. Default is FALSE. If it is FALSE, the result is in Chinese, otherwise in English.
#' 
#' @examples 
#' # get province code 
#' prov1 = geted_nbs_region(geo_type = 'province') 
#' # or using 'p' represents 'province'
#' prov2 = geted_nbs_region(geo_type = 'p') 
#' 
#' \dontrun{
#' # get city code in Chinese
#' city = geted_nbs_region(geo_type = 'c', eng = FALSE) 
#' # get city code in English
#' city = geted_nbs_region(geo_type = 'c', eng = TRUE) 
#' }
#' 
#' @importFrom jsonlite fromJSON 
#' @export
geted_nbs_region = function(geo_type=NULL, eng=FALSE) {
  dim_region = dim_geo_type = dim_sta_db = . = code = name = NULL
  
  # param
  url_nbs = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  # geography type
  geo_type = check_arg(geo_type, c("national", "province", "city"))
  if (geo_type == 'national') return(NULL)
  # name of geography in NBS
  nbs_geo = df_nbs_geo()[
    dim_region=='cn' & dim_geo_type==geo_type, ][.N,dim_sta_db]
  
  # wds
  wds='[{"wdcode":"reg","valuecode":"00"}]'
  if (geo_type == 'city') wds='[{"wdcode":"reg","valuecode":"000000"}]'
  
  # query region
  query_list = list(
    m="getOtherWds",
    dbcode=nbs_geo,
    rowcode='zb',
    colcode='sj',
    wds=wds, 
    # dfwds=paste0('[{"wdcode":"sj","valuecode":"LAST10"}]'),
    k1=time_sec
  )
  req = GET(url_nbs, query=query_list)
  check_http_status_nbs(req)
  jsondat = fromJSON(content(req, "text", encoding="utf-8"))
  regdf = setDT(jsondat$returndata$nodes[[1]])[,.(code, name)]
  
  return(regdf)
}
# check region
check_nbs_region = function(region, geo_type, eng){
  code = NULL
  
  if (grepl('^n', geo_type)) {
    return(NULL)
  } else if (grepl('^p|^c', geo_type)) {
    if (any(region =='all')) return(region)
    
    # dataframe of subregions in CHina
    regdf = geted_nbs_region(geo_type, eng)
    
    # all regions?
    all_regioin_notin_regdf = is.null(region) || any((region %in% regdf$code) == FALSE)
    if (all_regioin_notin_regdf) {
      print(setDF(copy(regdf)))
      sel_region = readline("Select regions' rowid: ")
      
      if (sel_region=='all') return(sel_region)
      sel_region = sprintf('c(%s)', gsub('-',':',gsub(' ','',sel_region))) 
      sel_region = eval(parse(text = sel_region))
      sel_region = regdf[sel_region, code]
    }
    return(sel_region)
  }
  
}



# transform the from/to date to monthly, quarterly, or anuual 
# (ex:'2000-01-01' --> '200001', '2000A', '2000')
date_to_mqa = function(from, to, freq) {
  if (freq == 'monthly') {
    from2 = format(as.Date(from), '%Y%m')
    to2 = format(as.Date(to), '%Y%m')
  } else if (freq == 'quarterly') {
    y = sapply(list(from,to), function(x) format(as.Date(x), '%Y'))
    q = LETTERS[ceiling(sapply(list(from,to), function(x) as.integer(format(as.Date(x), '%m')))/3)]
    from2 = paste0(y[1], q[1])
    to2 = paste0(y[2], q[2])
  } else {
    from2 = format(as.Date(from), '%Y')
    to2 = format(as.Date(to), '%Y')
  }
  
  return(list(from2, to2))
}

# get query data # zb symbol, sj date, reg region
#' @importFrom jsonlite fromJSON 
geted1_nbs = function(nbs_geo, symbol1, region=NULL, from, eng=FALSE) {
  url_nbs = sel_nbs_url(eng)
  time_sec = as.character(date_to_sec()*100)
  
  # date range
  freq_mqa = which(substr(nbs_geo, 3, 3) == c('y','j','n'))
  date_rng = Sys.Date() - as.Date(from)
  date_rng = floor( date_rng/(c(30, 90, 365)[freq_mqa]) )
  sj_value = paste0("LAST",date_rng)
  
  # region parm
  wds = ifelse( is.null(region), '[]',
    paste0('[{"wdcode":"reg","valuecode":"',region,'"}]') )
  
  rowcode = 'zb'
  if (!is.null(region)) {
    if (grepl('^fs|^cs', nbs_geo) & ('all' %in% region || length(region)>1)) rowcode = 'reg'
  }
  
  # query list
  query_list = list(
    m="QueryData",
    dbcode=nbs_geo,
    rowcode=rowcode,
    colcode='sj',
    wds=wds,
    dfwds=paste0('[{"wdcode":"zb","valuecode":"',symbol1,'"},{"wdcode":"sj","valuecode":"',sj_value,'"}]'),
    k1=time_sec
  )
  req = GET(url_nbs, query=query_list)
  check_http_status_nbs(req)
  jsondat = fromJSON(content(req, "text", encoding="utf-8"))
  
  return(jsondat)
}

# transform json data into dataframe
# symbol, variable, date, value, unit, desc
nbs_jsondat_format = function(jsondat) {
  code = . = unit = cname = strdata = NULL
  
  # data
  dat = jsondat$returndata$datanodes$data
  setDT(dat)
  
  # code names
  code_names = jsondat$returndata$wdnodes$wdcode
  code_names2 = sub('sj','date',sub('reg','region_code',sub('zb','symbol',code_names)))
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
  
  # merge region with data
  if ('reg' %in% code_names) {
    reg_df = setDT(jsondat$returndata$wdnodes$nodes[[which('reg' == code_names)]])[,.(region_name=cname, region_code=code)]
    
    dat2 = dat2[reg_df, on='region_code'][, c("date", "symbol", "name", "value", "unit", "region_code", "region_name"), with=FALSE]
  } else {
    dat2 = dat2[, c("date", "symbol", "name", "value", "unit"), with=FALSE]
  }
  
  setkeyv(dat2, c("symbol", "date"))
  return(dat2)
}


#' get economic data from National Bureau of Statistics of China (NBS)
#' 
#' \code{geted_nbs} provides an interface to get economic data from National Bureau of Statistics of China (NBS).
#' 
#' @param geo_type geography type in NBS, including 'national', 'province', 'city'. Default is NULL.
#' @param freq the frequency of indicators in NBS, including 'monthly', 'quarterly', 'yearly'. Default is NULL.
#' @param symbol symbol of indicators in NBS, which is available via geted_nbs_symbol. Default is NULL.
#' @param region region codes of province or city, which is available via geted_nbs_region. Default is NULL.
#' @param from the start date. Default is '2010-01-01'.
#' @param to the end date. Default is current system date.
#' @param na_rm logical. If it is TRUE, the missing values will be removed. Default is FALSE.
#' @param eng logical. If it is FALSE, the query results are in Chinese, otherwise in English. Default is FALSE.
#' 
#' @source \url{http://data.stats.gov.cn/index.htm}
#' 
#' @examples 
#' # interactively setting paratmeters
#' \dontrun{
#' dt = geted_nbs()
#' 
#' # specify paratmeters
#' dt1 = geted_nbs(geo_type='national', freq='quarterly', 
#'   symbol='A010101')
#' 
#' # or using 'n'/'q' represents 'national'/'quarterly'
#' dt2 = geted_nbs(geo_type='n', freq='q', symbol='A010101')
#' 
#' 
#' # get data of one region
#' dt3 = geted_nbs(geo_type='province', freq='quarterly', 
#'   symbol='A010101', region='110000', from='2010-03-01', to='2010-03-01')
#'   
#' # get data of all province
#' dt4 = geted_nbs(geo_type='province', freq='quarterly', 
#'   symbol='A010101', region='all', from='2010-03-01', to='2010-03-01')
#' }
#' 
#' @import data.table
#' @export
geted_nbs = function(geo_type=NULL, freq=NULL, symbol=NULL, region=NULL, from='2010-01-01', to=Sys.Date(), na_rm=FALSE, eng=FALSE) {
  dim_region = dim_geo_type = dim_freq = dim_sta_db = value = NULL
  
  # geography type
  geo_type = check_arg(geo_type, c("national", "province", "city"))
  # frequency
  freq = check_arg_nbsfreq(freq, geo_type)
  
  # name of geography in NBS
  nbs_geo = df_nbs_geo()[
    dim_region=='cn' & dim_geo_type==geo_type & dim_freq==freq , dim_sta_db]
  
  # symbol
  if (is.null(symbol)) symbol = geted_nbs_symbol(geo_type, freq, eng)
  # region
  if (is.null(region)) region = check_nbs_region(region, geo_type, eng)
  # from/to
  from = check_fromto(from)
  to = check_fromto(to)
  fromto = date_to_mqa(from, to, freq)
  
  # jsondat
  jsondat = NULL
  for (s in symbol) {
    jsondat[[s]] = geted1_nbs(nbs_geo, symbol=s, region, from, eng)
  }
  # rbindlist jsondat
  dat = rbindlist(lapply(jsondat, nbs_jsondat_format), fill=TRUE)[date>=fromto[[1]] & date<=fromto[[2]],]
  
  # filter dat by region if length(region)>1
  if (length(region) > 1 & !("all" %in% region)) {
    row_idx = which(dat$region %in% region)
    dat = dat[row_idx,]
  }
  
  # remove na rows
  if (na_rm) dat = dat[!is.na(value)]
  setkey(dat, "date")
    
  return(dat)
}

# library(data.table)
# library(httr)
# library(jsonlite)

# eu, jp, us ------