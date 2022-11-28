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

#' @import xefun
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

