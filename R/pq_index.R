# formula 
# weighted geometric mean
# I_t/I_{t-1} = \prod{(x_{j,t}/x_{j,t-1})^{w_{j,t}}}

# dat = getmd_stock(sybs, fillzero = FALSE)
# ssec = getmd_stock('^000001')

# create a fixed-base index from chain index
#' @import data.table
pq_fbi = function(dt, chain_index, num=1, base_index=1, base_date='2006-01-01') {
    index = NULL
    
    # the row index of base
    base_row = dt[,which.min(abs(as.Date(date) - as.Date(base_date)))]
    base_date = dt[base_row, date]
    # cat('fixed-base index in', base_date)
    dt[base_row, index := base_index]
    
    for (r in (base_row+1):nrow(dt)) {
        chain_index_r = dt[r, eval(parse(text=chain_index))]
        if ( is.na(chain_index_r) ) break
        dt[r, index := dt[r-num,index]*chain_index_r ]
    }
    
    for (r in (base_row-1):1) {
        chain_index_rplusn = dt[r+num, eval(parse(text=chain_index))]
        if ( is.na(chain_index_rplusn) ) break
        dt[r, index := dt[r+num,index]/chain_index_rplusn ]
    }
    
    return(dt)
}

# fixed-base index of cpi 
fbi_cpicn = function(sybs = c('A01010101', 'A01010201','A01030101', 'A01030201')) {
    value = . = index = NULL
    
    cpicn = ed_nbs(geo_type = 'n', freq = 'm', symbol = sybs, na_rm = TRUE, from='1900-01-01')
    
    cpicn2 = dcast(
        cpicn[, value := value/100], date~name, value.var = 'value'
    )[, date := as.Date(paste0(date,'01'),format='%Y%m%d')]
    setnames(cpicn2, c('date', 'cpi_yoy', 'cpi_mom'))
    
    cpi_fbi = pq_fbi(
        dt = pq_fbi(cpicn2, chain_index = 'cpi_mom'),
        chain_index = 'cpi_yoy', num=12)
    
    return(cpi_fbi[,.(date,index)])
}
# cpi = fbi_cpicn()



# ped in real price
pq_rp1 = function(dt, region='cn', columns = c('open', 'high', 'low', 'close')) {
    date2 = . = index = NULL
    
    if (is.list(dt) & !is.data.frame(dt)) dt = rbindlist(dt, fill = TRUE)
    
   dt_rp = merge(
        dt[, date2 := as.Date(sub('\\d{2}$', '01', date))],
        eval(parse(text = paste0('fbi_cpi',region,'()')))[,.(date2 = date, index)],
        all.x = TRUE
    )[, index := fillna(index)
      ][, (columns) := lapply(.SD, function(x) x/(index/index[.N])), .SDcols = columns
        ][, c('date', columns), with=FALSE]
   
   return(dt_rp)
}
# ssec_nomial = getmd('^000001', from='1900-01-01', source='163')
# ssec_real = getpedr:::pq_rp1(ssec_nomial)



#' creating weighted index 
#' 
#' \code{pq_index} creates a sector/industry index using the method of weighted geometric mean, based on a set of data and corresponding weights.
#' 
#' @param dt a list/dataframe of time series dataset
#' @param x the name of column to create index. Default is 'close|value'
#' @param w the name of weights column. Default is 'cap_total'. If x is not available or is NULL, then using equal weights.
#' @param base_value the base value of index. Default is 1.
#' @param base_date the base date of index. Default is the minimum date.
#' @param name the name of index. Default is NULL, then using 'index'.
#' 
#' @examples 
#' \donttest{
#' # Example I bank share index
#' # load data
#' bank_symbol = c('601988', '601288', '601398', '601939', '601328')
#' bank_dat = md_stock(bank_symbol, source='163', date_range = 'max')
#' 
#' # creating index
#' bank_index = pq_index(bank_dat, x='close', w='cap_total')
#' # pq_plot(bank_index)
#' 
#' }
#' 
#' @import data.table
#' @export
pq_index = function(dt, x='close|value', w='cap_total', base_value=1, base_date=NULL, name = NULL) {
  chain_index=weight=chain_index=cw=.=value=index=NULL
  
    ## index name
    if (is.null(name)) {
      name = 'index'
      symbol = 'index'
    }

    ## check columns of x, chain index, weight
    if (!inherits(dt, 'list') & inherits(dt, 'data.frame')) {
      dat_lst = list()
      for (s in dt[,unique(symbol)]) dat_lst[[s]] = dt[symbol == s]
      dt = dat_lst
      rm(dat_lst)
    }
    dt = lapply(dt, function(d) {
      x = intersect(names(d), unlist(strsplit(x,'\\|')))[1]
      if (x != 'close') d[['close']] = d[[x]]
      d[, chain_index := close/shift(close,type='lag')]
      
      # weight column
      if (is.null(w) || w %in% names(d)) {
        d[['weight']] = 1
      } else d[['weight']] = d[[w]]
      
      return(d)
    })
    
    ## bind list of dataframes
    dt = rbindlist(dt, fill = TRUE)
    dt = dt[, symbol := factor(symbol, levels = dt[,unique(symbol)])]
    setkeyv(dt, c('symbol','date'))
    
    ## weight geometric mean index
    wgm_idx = dt[, w := weight/sum(weight, na.rm = TRUE), keyby=date
     ][is.na(chain_index), chain_index := 0
     ][, cw := chain_index**w
     ][, .(value = prod(cw)), keyby=date
     ][value != 0]

    ## base date
    base_date = check_fromto(base_date)
    if (is.null(base_date) || (base_date < wgm_idx[1,date] || base_date > wgm_idx[.N,date])) base_date = wgm_idx[1,date]
    
    ## fixed base index
    fbi = pq_fbi(wgm_idx, 'value', base_index=base_value, base_date=base_date)[,.(symbol=symbol, name=name, date, value=index)]
    return(fbi)
}



# ref:
# https://en.wikipedia.org/wiki/Weighted_geometric_mean
# https://en.wikipedia.org/wiki/U.S._Dollar_Index