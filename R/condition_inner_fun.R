# check date format of from/to
check_fromto = function(fromto) {
    if (grepl("-|/",fromto)) {
        fromto = as.Date(fromto)
    } else {
        if (nchar(fromto)==6) {
            fromto = as.Date(fromto, format="%y%m%d")
        } else if (nchar(fromto) == 8) {
            fromto = as.Date(fromto, format="%Y%m%d")
        }
    }
    
    return(fromto)
}

# get tags for a China stock symbol
tags_symbol_stockcn = function(symbol, market) {
    # 001×××国债现货；110×××120×××企业债券；129×××100×××可转换债券；201×××国债回购；310×××国债期货；
    #500×××550×××基金；600×××A股；700×××配股；710×××转配股；701×××转配股再配股；711×××转配股再转配股；720×××红利；730×××新股申购；735×××新基金申购；737×××新股配售；900×××B股。
    symbol2 = sub(".*?(\\d+).*","\\1", symbol)
    if (nchar(symbol2)==6) {
        if (market=="stock") {
            tags = switch (substr(symbol2,1,3),
                    "600"="sse,A,main",
                    "601"="sse,A,main",
                    "603"="sse,A,main",
                    "900"="sse,B",
                    "000"="szse,A,main",
                    "001"="szse,A,main",
                    "002"="szse,A,sme",
                    "300"="szse,A,chinext",
                    "200"="szse,B",
            )
        } else if (market=="index") {
            tags = switch (substr(symbol2,1,3),
                    "000"="sse",
                    "399"="szse"
            )
        }
    } else if (nchar(symbol2)==5) {
        tags = ifelse(substr(symbol2,1,2)=="08", "hkex,,gem", "hkex,,main")
    }
    return(tags)
    # hkex: 
    # 蓝筹股（Blue Chip）
    # 红筹股（Red Chip）
    # 国企股（H股）
}

# 
check_symbol_for_163 = function(symbol) {
    tags = tags_symbol_stockcn(symbol, 
        ifelse(grepl("\\^",symbol),"index","stock"))
    ex_code = ifelse(grepl("sse",tags), "0", 
        ifelse(grepl("szse",tags), "1", NULL))
    
    if (is.null(ex_code)) {
        return(NULL)
    } else {
        return(paste0(ex_code, sub(".*?(\\d+).*","\\1", symbol)))
    }
}


# download and read excel file from website
#' @importFrom readxl read_excel
#' @importFrom utils download.file
load_read_xl = function(link) {
    temp = tempfile()
    download.file(url=link, destfile=temp, quiet=TRUE)
    dat = read_excel(temp)
    unlink(temp)
    
    return(dat)
}

#download and read csv file from website
#' @importFrom utils download.file read.csv
load_read_csv = function(link, encode) {
    temp = tempfile()
    download.file(url=link, destfile=temp, quiet=TRUE)
    dat = read.csv(temp, fileEncoding = encode)
    unlink(temp)
    
    return(dat)
}


# fill 0/na in a vector with last non 0/na value
fill0 = function(x) {
    # index of x==0
    ind = which(x==0)
    while (length(ind) >0 & any(!(ind %in% 1:length(ind))) ) {
        # replace value with last
        x[ind] <- x[ind-1]
        # index of x==0
        ind = which(x==0)
    }
    return(x)
}
fillna = function(x) {
    # index of x==na
    ind = which(is.na(x))
    while (length(ind) >0 & any(!(ind %in% 1:length(ind))) ) {
        # replace value with last
        x[ind] <- x[ind-1]
        # index of x==na
        ind = which(is.na(x))
    }
    return(x)
}


# system time in milliseconds
sys_time_milli_sec = function() {
    ms = as.character(as.numeric(Sys.time())*1000)
    return(unlist(strsplit(ms, '\\.'))[1]) 
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
lwd <- function(n, d = NULL, tz = NULL) {
    #timezone
    if (is.null(tz)) {
        tz = "CET"
    }
    
    #the current or reference date
    if (is.null(d)) {
        d = as.Date(Sys.Date(), format("%Y%m%d"), tz = tz)
    }
    
    #weekday
    w <- as.numeric(format(d, format = "%w", tz = tz))
    
    if (w == 0) {
        d <- d - 2
    } else if (w == 6) {
        d <- d - 1
    } else {
        d <- d
    }
    w <- as.numeric(format(d, format = "%w", tz = tz))
    
    while (n > 0) {
        d <- d - 1
        
        w <- as.numeric(format(d, format = "%w", tz = tz))
        if (w == 0) {
            d <- d - 2
        } else if (w == 6) {
            d <- d - 1
        } else {
            d <- d
        }
        
        n <- n - 1
    }
    
    lwd <- format(d, format = "%Y%m%d", tz = tz)
    return(lwd)
}
