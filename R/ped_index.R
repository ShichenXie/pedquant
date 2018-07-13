# formula 
# weighted geometric mean
# I_t/I_{t-1} = \prod{(x_{j,t}/x_{j,t-1})^{w_{j,t}}}

# dat = getmd_163(sybs, fillzero = FALSE)
# ssec = getmd_163("^000001")

# create a fixed-base index from chain index
#' @import data.table
ped_fbi = function(dt, chain_index, num=1, base_index=1, base_date="2006-01-01") {
    index = NULL
    
    # the row index of base
    base_row = dt[,which.min(abs(as.Date(date) - as.Date(base_date)))]
    base_date = dt[base_row, date]
    # cat("fixed-base index in", base_date)
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
fbi_cpicn = function(sybs = c("A01010101", "A01010201","A01030101", "A01030201")) {
    value = . = index = NULL
    
    cpicn = geted_nbs(geo_type = "n", frequency = "m", symbol = sybs, na_rm = TRUE)
    
    cpicn2 = dcast(
        cpicn[, value := value/100], date~symbol_name, value.var = "value"
    )[, date := as.Date(paste0(date,"01"),format="%Y%m%d")]
    setnames(cpicn2, c("date", "cpi_yoy", "cpi_mom"))
    
    cpi_fbi = ped_fbi(
        ped_fbi(cpicn2, chain_index = "cpi_mom"),
        chain_index = "cpi_yoy", num=12)
    
    return(cpi_fbi[,.(date,index)])
}
# cpi = cpicn_fbi()

# ped in real price
ped_rp1 = function(dt, region="cn", columns = c("open", "high", "low", "close")) {
    date2 = . = index = NULL
    
    if (is.list(dt) & !is.data.frame(dt)) dt = rbindlist(dt, fill = TRUE)
    
   dt_rp = merge(
        dt[, date2 := as.Date(sub("\\d{2}$", "01", date))],
        eval(parse(text = paste0("fbi_cpi",region,"()")))[,.(date2 = date, index)],
        all.x = TRUE
    )[, index := fillna(index)
      ][, (columns) := lapply(.SD, function(x) x/(index/index[.N])), .SDcols = columns
        ][, c("date", columns), with=FALSE]
   
   return(dt_rp)
}
# ssec_real = ped_rp1(getmd_163("^000001", print_step = 0))



#' create a index
#' 
#' ped_index creates a index, which is geometrically weighted averages of chain index, based on multiple time series dataframes.
#' 
#' @param dt input datasets including many dataframes.
#' @param chain_index the name of chain index
#' @param weight the name of weight variable
#' @param base_index the base value of index, default is 1
#' @param base_date the date base of index, deafult is "2010-01-01"
#' 
#' @examples 
#' \dontrun{
#' banks_symbol = c("601988", "601288", "601398", "601939")
#' 
#' dat = getmd_163(banks_symbol)
#' 
#' library(data.table)
#' dat = lapply(dat, function(x) x[, pct_change:=pct_change+1])
#' 
#' bankindex = ped_index(dat, "pct_change", "total_cap")
#' }
#' 
#' @import data.table
#' @export
#' 
ped_index = function(dt, chain_index, weight, base_index=1, base_date="2010-01-01") {
    w = ci = cw = ped_fbi = V1 = . = index = NULL
    
    if (is.list(dt) & !is.data.frame(dt)) dt = rbindlist(dt, fill = TRUE)
    setkey(dt, "date")
    
    # weight
    dt = dt[, w := eval(parse(text=weight))/sum(eval(parse(text=weight)), na.rm = TRUE), by=date
     ][, ci := ifelse(is.na(eval(parse(text=chain_index))), 0, eval(parse(text=chain_index)))
     ][, cw := ci**w
     ][, prod(cw), by=date
     ][V1 != 0]
    
    return(ped_fbi(dt, "V1", base_index=base_index, base_date=base_date)[,.(date, index)])
}



