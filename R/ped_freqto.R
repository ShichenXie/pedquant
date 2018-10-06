ped1_dtow = function(dat) {
    w=wi=.=symbol=name=high=low=volume=NULL
    
    dat[, w := isoweek(date)
      ][, wi := w - shift(w, 1, type="lag")
      ][, wi := ifelse(wi!=0 | is.na(wi), 1, wi)
      ][, wi := cumsum(wi)]
    
    dat2 = dat[, .(
        date=date[.N],
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = wi][, wi := NULL]
    
    return(dat2)
}
ped1_dtom = function(dat) {
    .=symbol=name=high=low=volume=NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date), m=month(date))][, (c('y','m')) := NULL]
    
    return(dat2)
}
ped1_dtoq = function(dat) {
    . = symbol = name = high = low = volume = NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date), q=quarter(date))][, (c('y','q')) := NULL]
    
    return(dat2)
}
ped1_dtoy = function(dat) {
    .=symbol=name=high=low=volume=NULL
    
    dat2 = dat[, .(
        date=date[.N], 
        open=open[1], high=max(high), low=min(low), close=close[.N], 
        volume=sum(volume)
    ), by = .(y=year(date))][, (c('y')) := NULL]
    
    return(dat2)
}

check_freq_isdaily = function(dat) {
    # check freq of input data
    diff_date = dat[, as.numeric(mean(date - shift(date, n=1, type="lag"), na.rm=TRUE)) ]
    if (diff_date > 2) {stop("The frequency of input data should be daily.")}
}

ped1_dailyto = function(dat, freq) {
    . = high = low = name = symbol = volume = NULL
    setkeyv(dat, "date")
    # check freq of input data
    check_freq_isdaily(dat)
    # check freq argument
    freq = check_arg(freq, c("weekly","monthly","quarterly","yearly"), "weekly")

    # change
    dat2 = do.call(paste0("ped1_dto",substr(freq,1,1)), list(dat=dat))
    
    # symbol and name
    if (all(c("symbol","name") %in% names(dat))) {
      dat2 = dat2[, .(date, symbol=dat[1,symbol], name=dat[1,name], open, high, low, close, volume)]
    } else if ("symbol" %in% names(dat)) {
      dat2 = dat2[, .(date, symbol=dat[1,symbol], open, high, low, close, volume)]
    }
    
    return(dat2)
}

#' convert the frequency of daily data
#' 
#' @param dt time series datasets
#' @param freq the frequency input data will converted to It supports weekly, monthly, quarterly and yearly.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' dts = getmd(c("^000001", "000001"), from = "1990-01-01", source = "163")
#' 
#' dts_weekly = ped_dailyto(dts, "weekly")
#' 
#' @export
#' 
ped_dailyto = function(dt, freq, print_step=1L) {
    dt_list = list()
    
    if (is.list(dt) & !is.data.frame(dt)) {
        dt_names = names(dt)
        len_names = length(dt_names)
        dt = lapply(dt, setDT)
        
        for (i in 1:len_names) {
            if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i, len_names)),collapse = "/"), dt_names[i],"\n")
            
            dt_list[[i]] = do.call(ped1_dailyto, args = list(dat=dt[[i]], freq=freq))
        }
        
    } else if (is.data.frame(dt)) {
        setDT(dt)
        i = 1
        dt_list[[i]] =do.call(ped1_dailyto, args = list(dat=dt, freq=freq))
        
    }
    
    return(dt_list)
}