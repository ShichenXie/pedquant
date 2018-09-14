########################### condition functions ###########################
# check arguments
check_arg = function(arg, choices, default=NULL) {
    arg = try(match.arg(arg, choices), silent = TRUE)
    if (inherits(arg, "try-error")) {
        if (is.null(default)) {
            arg = choices[menu(choices, cat("Verify the argument"))]
        } else {
            arg = default
            warning("The argument is set to '",default,"'")
        }
    }
    return(arg)
}

# check date format of from/to
check_fromto = function(fromto, type="date", shift = 0) {
    type = check_arg(type, c("date", "time"), "date")
    
    # type: dates or times
    if (class(fromto) == "character") {
        if (grepl("-|/",fromto)) {
            fromto = as.Date(fromto)
        } else {
            if (nchar(fromto)==6) {
                fromto = as.Date(fromto, format="%y%m%d")
            } else if (nchar(fromto) == 8) {
                fromto = as.Date(fromto, format="%Y%m%d")
            }
        }
        
        if (type != "date") fromto = as.POSIXct(paste(fromto+shift, "00:00:00"))
    }
    
    return(fromto)
}

get_from_daterange = function(dt, date_range, to) {
    if (date_range == "max") {
        from = dt[, min(date)]
    } else if (date_range == "ytd") {
        from = sub("-[0-9]{2}-[0-9]{2}", "-01-01", as.character(to))
        
    } else if (grepl("[0-9]+m", date_range)) {
        month_range = as.integer(sub("m","",date_range))
        month_to = as.integer(sub("^[0-9]{4}-([0-9]{1,2})-.+$", "\\1", to))
        year_to = as.integer(format(as.Date(to), "%Y"))
        if (month_to <= month_range) {
            from = paste(year_to-1, 12+month_to-month_range, sub("[0-9]{4}-[0-9]{1,2}-","",to), sep="-")
        } else {
            from = paste(year_to, month_to-month_range, sub("[0-9]{4}-[0-9]{1,2}-","",to), sep="-")
        }
        
    } else if (grepl("[0-9]+y", date_range)) {
        year_range = as.integer(sub("y","",date_range))
        year_from = as.integer(format(as.Date(to), "%Y")) - year_range
        from = sub("^[0-9]{4}", year_from, to)
    }
    
    
    # set class
    if (class(to) == "Date") {
        from = as.Date(from)
    } else {
        from = as.POSIXct(from)
    }
    
    return(from)
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
    syb = sub("^.*?([0-9]+).*$","\\1",symbol)
    mkt = ifelse(grepl("\\^",symbol),"index","stock")
    tags = tags_symbol_stockcn(symbol, mkt)
    
    ex_code = ifelse(grepl("sse",tags), "0", 
                     ifelse(grepl("szse",tags), "1", NULL))
    if (!is.null(ex_code)) symbol = paste0(ex_code, syb)
    return(symbol)
}
check_symbol_for_tx = function(symbol) {
    syb = sub("^.*?([0-9]+).*$","\\1",symbol)
    mkt = ifelse(grepl("\\^",symbol),"index","stock")
    tags = tags_symbol_stockcn(symbol, mkt)
    
    ex_code = ifelse(grepl("sse",tags), "sh", 
                     ifelse(grepl("szse",tags), "sz", NULL))
    if (!is.null(ex_code)) symbol = paste0(ex_code, syb)
    return(symbol)
}
check_symbol_for_yahoo = function(symbol) {
    syb = sub("^.*?([0-9]+).*$","\\1",symbol)
    if (nchar(syb)==6 & (nchar(symbol)==7 | nchar(symbol)==6)) {
        mkt = ifelse(grepl("\\^",symbol),"index","stock")
        tags = tags_symbol_stockcn(symbol, mkt)
        
        ex_code = ifelse(grepl("sse|szse",tags), substr(tags,1,2), NULL)
        if (!is.null(ex_code)) symbol = paste(syb, ex_code, sep=".")
    }
    return(symbol)
}


# market data sources
check_mkt_src = function(market=NULL, source=NULL) {
    mkt = src = NULL
    
    mkts = c("currency", 
             # "bond", "money", 
             "index", "stock", "future")
    srcs = c("yahoo", "163", "sina", "original")
    market = market[which(market %in% mkts)]
    source = source[which(source %in% srcs)]
    
    ms_df = rbindlist(list(
        currency = data.frame(src = c("yahoo")),
        # bond = data.frame(src = c("original")),
        # money = data.frame(src = c("original")),
        index = data.frame(src = c("yahoo", "163")),
        stock = data.frame(src = c("yahoo", "163")),
        future = data.frame(src = c("yahoo", "sina"))
    ), idcol = "mkt")
    
    
    if ((is.null(market) & is.null(source)) || length(source) > 1) {
        # specify market
        market = mkts[menu(mkts, cat('Specify the market:'))]
        
        # specify data source
        srcs = ms_df[mkt %in% market, src]
        source = srcs[menu(srcs, cat('Specify the data source:'))]
    } else if (is.null(market)) {
        mkts = ms_df[src %in% source, mkt]
        market = mkts[menu(mkts, cat('Specify the market:'))]
    } else if (is.null(source)) {
        srcs = ms_df[mkt %in% market, src]
        source = srcs[menu(srcs, cat('Specify the data source:'))]
    } else {
        ms = ms_df[mkt %in% market & src %in% source]
        if (ms[,.N] > 1) {
            mkts = ms$mkt
            market = mkts[menu(mkts, cat('Specify the market:'))]
        }
    }
    return(list(mkt=market, src=source))
}


########################### inner functions ###########################
# download and read excel file from website
#' @importFrom readxl read_excel
#' @importFrom utils download.file
#' @importFrom curl curl_download new_handle
load_read_xl = function(url, handle=new_handle()) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    curl_download(url, destfile = temp, handle = handle)
    dat = read_excel(temp)
    
    return(dat)
}

#download and read csv file from website
#' @importFrom utils download.file read.csv
#' @importFrom curl curl_download new_handle
# load_read_csv = function(url, encode) {
#     temp = tempfile()
#     download.file(url=url, destfile=temp, quiet=TRUE)
#     dat = read.csv(temp, fileEncoding = encode)
#     unlink(temp)
#     
#     return(dat)
# }
load_read_csv = function(url, encode="UTF-8", handle=new_handle()) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    curl_download(url, destfile = temp, handle = handle)
    dat = read.csv(temp, fileEncoding = encode)
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


# convert date to second 
date_to_sec = function(date=Sys.time()) {
    datetime = as.POSIXct(as.Date(date, origin = "1970-01-01"))
    return(trunc(as.numeric(datetime))) 
}


# loop on get_data1
load_dat_loop = function(symbol, func, args=list(), print_step) {
    runif = dt_list = NULL
    symbol_len = length(symbol)
    for (i in 1:symbol_len) {
        symbol_i = symbol[i]
        # print
        if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i,symbol_len)),collapse = "/"), symbol_i,"\n")
        
        dt_list[[symbol_i]] = do.call(eval(parse(text = func)), c(symbol_i, args))
        
        # sleep for 1s
        Sys.sleep(runif(1))
    }
    return(dt_list)
}


# extract table from html via xml2 package
#' @import data.table
xml_table = function(wb, num=NULL, sup_rm = NULL) {
    doc0 = xml_find_all(wb, "//table")
    if (!is.null(num)) doc0 = doc0[num]
    
    doc = lapply(
        doc0,
        function(x) xml_text(xml_find_all(x, ".//tr"))
    )
    
    dt = lapply(doc, function(x) {
        if (!is.null(sup_rm)) x = gsub(sup_rm, "", x)
        
        data.table(x = x)[, tstrsplit(x, "[\n\t\r]+")]
    })
    
    return(dt)
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
lwd <- function(n = 0, d = Sys.Date(), tz = Sys.timezone()) {
    
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
    
    lwd <- as.Date(d)#format(d, format = "%Y%m%d", tz = tz)
    return(lwd)
}


api_key = function(src){
    if (src=="fred") return("4330f2f7aab9a42ab9b950cec4428b91")
}