# d, w, m, q, ytd, y, 

ped1_perf = function(dt, y="open|close|value", date_range="max", from=NULL, to=Sys.Date()) {
    y = names(dt)[grepl(y, names(dt))]
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = dt[, tolower(class(date))], shift=1) 
    if (is.null(from)) from = get_from_daterange(dt, date_range, to)
    
    # set range for data
    dat = dt[date>=from & date<=to
           ][, (y) := lapply(.SD, function(x) fill0(x)/x[1]), .SDcols = y]
    
    cols = c("date", y)
    if ("symbol" %in% names(dat)) cols = c(cols, "symbol")
    if ("name" %in% names(dat)) cols = c(cols, "name")
    dat = dat[, cols, with = FALSE]
    
    return(dat)
}

# create performance of data sets
# 
# 
ped_perf = function(dt, y="open|close|value", date_range="max", from=NULL, to=Sys.Date()) {
    symbol = NULL
    
    date_range = check_arg(tolower(date_range), c(paste0(1:11,"m"), "ytd", "max", paste0(1:500,"y")), default = "max")
    
    # plot symbol
    dt_list = NULL
    dt_names = names(dt)
    if (is.list(dt) & !is.data.frame(dt)) {
        dt = lapply(dt, setDT)
        for (i in dt_names) {
            if (is.null(title)) title = i
            dt_list[[i]] = do.call(ped1_perf, args = list(dt=dt[[i]], y=y, date_range=date_range, from=from, to=to))
        }
        
    } else if (is.data.frame(dt)) {
        setDT(dt)
        i = 1
        if ("symbol" %in% dt_names) i = dt[1, symbol]
        
        dt_list[[i]] = do.call(ped1_perf, args = list(dt=dt, y=y, date_range=date_range, from=from, to=to))
        
    }
    
    return(dt_list)
}