# date time -----
# check date format of from/to
#' @importFrom lubridate as_date
check_fromto = function(fromto, type="date", shift = 0) {
    type = check_arg(type, c("date", "time"), "date")
    
    # type: dates or times
    if (inherits(fromto, "character")) {
        if (type == 'date') {
            fromto = as_date(fromto)
        } else if (type != "date") {
            fromto = as.POSIXct(paste(fromto+shift, "00:00:00"))
        }
    }
    
    return(fromto)
}


# check date range
check_date_range = function(date_range, default = "max") {
    if (!grepl("max|ytd|qtd|mtd|[1-9][0-9]*d|[1-9][0-9]*w|[1-9,10,11]m|[1-9][0-9]*y", tolower(date_range))) {
        date_range = default
        warning(sprintf('The \'date_range\' is set to %s.', date_range))
    }
    return(date_range)
}

check_to = function(to, default_to = Sys.Date()) {
    to = as_date(to)
    default_to = as_date(default_to)
    to = min(to, default_to)
    return(to)
}
check_from = function(date_range, from, to, default_from = '1000-01-01', default_date_range = 'max') {
    date_range = check_date_range(date_range, default = default_date_range)
    
    to = as_date(to)
    default_from = as_date(default_from)
    if (!is.null(from)) from = as_date(from)
    if ((inherits(from, 'character') & any(from == '')) || is.na(from) || length(from)==0) from = NULL
    
    if (is.null(from)) {
        from = date_from(date_range, to, default_from)
        if (is.null(from)) from = default_from
    }
    from = as_date(from)
    from = max(from, default_from)
    
    # set class
    if (inherits(to, "Date")) {
        from = as_date(from)
    } else {
        from = as.POSIXct(from)
    }
    
    return(from)
}

# is date/time class
isdatetime = function(x) {
    inherits(x, c("Date","POSIXlt","POSIXct","POSIXt"))
}

# convert date to second 
date_to_sec = function(x=Sys.time()) {
    datetime = as.POSIXct(as.Date(x, origin = "1970-01-01"))
    return(trunc(as.numeric(datetime))) 
}

# date of BOP, bebinning of period
date_bop = function(x, freq, workday = FALSE) {
    bop_mthday = NULL
    
    if (inherits(x, 'character')) x = as.Date(x)
    
    monthday = data.table(
        m = 1:12, 
        bop_mthday = sprintf('-%02i-01', 1:12), 
        key = 'm'
    )
    
    if (freq == 'yearly') {
        x = as.Date(sub('-[0-9]{2}-[0-9]{2}$', '-01-01', x))
    } else if (freq == 'quarterly') {
        x = monthday[(quarter(x)-1)*3+1, as.Date(paste0(year(x),bop_mthday))]
    } else if (freq == 'monthly') {
        x = as.Date(sub('[0-9]{2}$', '01', x))
    } else if (freq == 'weekly') {
        x = x - wday(x) + 1
    } else if (freq == 'daily') {
        return(x)
    }
    
    if (workday) x = lwd_num(-1, x)
    return(x)
}

# date of EOP, end of period
date_eop = function(x, freq, workday = FALSE) {
    if (inherits(x, 'character')) x = as.Date(x)
    
    if (freq == 'yearly') {
        x = as.Date(sub('-[0-9]{2}-[0-9]{2}$', '-12-31', x))
    } else if (freq == 'quarterly') {
        x = date_eop(sprintf('%s-%s-01', year(x), quarter(x)*3), freq = 'monthly')
    } else if (freq == 'monthly') {
        x = date_bop(date_bop(x, freq='monthly') + 45, freq='monthly') - 1
    } else if (freq == 'weekly') {
        x = x - wday(x) + 7
    } else if (freq == 'daily') {
        return(x)
    }
    
    if (workday) x = lwd_num(1, x)
    return(x)
}




# extract table from html via xml2 package
# @import data.table
# xml_table = function(wb, num=NULL, sup_rm=NULL, attr=NULL, header=FALSE) {
#     doc0 = xml_find_all(wb, paste0("//table",attr)) # attr = '[@cellpadding="2"]'
#     if (!is.null(num)) doc0 = doc0[num]
#     
#     doc = lapply(
#         doc0,
#         function(x) xml_text(xml_find_all(x, ".//tr"))
#     )
#     
#     dt = lapply(doc, function(x) {
#         if (!is.null(sup_rm)) x = gsub(sup_rm, "", x)
#         
#         dat = data.table(x = x)[, tstrsplit(x, "[\n\t\r]+")]
#         if (header) {
#             dat = setnames(dat[-1], as.character(dat[1]))
#         }
#         return(dat)
#     })
#     
#     return(dt)
# }

# last calendar day by xtd (ytd/qtd/mtd)
date_from_xtd = function(date_range, to = Sys.Date()) {
    if (inherits(to, 'character')) to = as.Date(to)
    to_year = year(to)
    to_quarter = quarter(to)
    to_month = month(to)
    
    if (date_range == 'ytd') {
        from_month = '01'
    } else if (date_range == 'qtd') {
        from_month = to_quarter*3-2
    } else if (date_range == 'mtd') {
        from_month = to_month
    }
    from = as.Date(sprintf('%s-%s-01', to_year, from_month))
    
    return(from)
}
date_from_xm = function(date_range, to = Sys.Date()) {
    if (inherits(to, 'character')) to = as.Date(to)
    to_year = year(to)
    to_month = month(to)
    to_day = mday(to)
    
    xm = as.integer(sub("m","",date_range))
    rng_year = floor(xm / 12)
    rng_month = xm %% 12
    
    if (to_month <= rng_month) {
        rng_year = rng_year + 1
        rng_month = rng_month - 12
    }
    
    from = as.Date(sprintf('%s-%s-%s', to_year-rng_year, to_month-rng_month, to_day)) 
    
    return(from)
}
date_from_ym = function(date_range, to = Sys.Date()) {
    from_year = year(to) - as.integer(sub("y","",date_range))
    from = as.Date(sub("^[0-9]{4}", from_year, to))
    return(from)
}
# the date from date_range before (calendar day)
date_from = function(date_range, to = Sys.Date(), default_from='1000-01-01') {
    to = as.Date(to)
    
    if (grepl("[yqm]td", date_range)) {
        from = date_from_xtd(date_range, to)
    } else if (grepl("[1-9][0-9]*d", date_range)) {
        from = to - as.integer(sub("d","",date_range))
    } else if (grepl("[1-9][0-9]*w", date_range)) {
        from = to - as.integer(sub("w","",date_range))*7
    } else if (grepl("[1-9][0-9]*m", date_range)) {
        for (i in c(0, 1, -1, 2, -2)) {
            from = try(date_from_xm(date_range, to+i), silent = TRUE)
            if (!inherits(from, 'try-error')) {
                if (i != 0) from = from - i/abs(i)
                break
            }
        }
    } else if (grepl("[1-9][0-9]*y", date_range)) {
        for (i in c(0, 1, -1, 2, -2)) {
            from = try(date_from_ym(date_range, to+i), silent = TRUE)
            if (!inherits(from, 'try-error')) {
                if (i != 0) from = from - i/abs(i)
                break
            }
        }
    } else if (date_range == 'max') {
        from = as.Date(default_from)
    } else {
        from = NULL
    }
    return(from)
}

# #' last workday date
# #' 
# #' @param n A number.
# #' @param d Current date in the format "%Y%m%d"
# #' @return The date which is n days before current date d
# #' @examples
# #' lwd(1)
# #' lwd(3, "20160101")
# #' 
lwd_num <- function(n, to = Sys.Date()) {
    ft = NULL
    # , tz = Sys.timezone()
    to = as.Date(to)
    
    n2 = abs(n) + ceiling(abs(n)/7)*2
    
    from = sapply(
        to, 
        function(x) {
            data.table(
                ft = seq(x, x - sign(n) * n2,  by = -sign(n))
            )[wday(ft) %in% 2:6
            ][abs(n), as.character(ft)]
        }
    )
    
    return(as.Date(from))
}