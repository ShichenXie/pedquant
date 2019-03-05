########################### condition functions ###########################
# check arguments
check_arg = function(arg, choices, default=NULL, arg_name = 'argument') {
    while (is.null(arg) || length(arg)==0) {
        arg = choices[menu(choices, cat(sprintf("Select the value of %s", arg_name)))]
    }
    arg = try(match.arg(arg, choices), silent = TRUE)
    if (inherits(arg, "try-error")) {
        arg = default
        warning(sprintf("The argument is set to \'%s\'", default))
    }
    return(arg)
}

# check date range
check_date_range = function(date_range, default = "max") {
    if (!grepl("max|ytd|[1-9][0-9]*d|[1-9][0-9]*w|[1-9,10,11]m|[1-9][0-9]*y", tolower(date_range))) {
        date_range = default
        warning(sprintf('The \'date_range\' is set to %s.', date_range))
    }
    return(date_range)
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

get_from_daterange = function(date_range, from, to, min_date) {
    if (is.null(from)) {
        if (date_range == "max") {
            from = min_date
        } else if (date_range == "ytd") {
            from = sub("-[0-9]{2}-[0-9]{2}", "-01-01", as.character(to))
            
        } else if (grepl("[1-9][0-9]*d", date_range)) {
            from = as.Date(to) - as.integer(sub("d","",date_range))
        } else if (grepl("[1-9][0-9]*w", date_range)) {
            from = as.Date(to) - as.integer(sub("d","",date_range))*7
        } else if (grepl("[1-9,10,11]m", date_range)) {
            month_range = as.integer(sub("m","",date_range))
            month_to = as.integer(sub("^[0-9]{4}-([0-9]{1,2})-.+$", "\\1", to))
            year_to = as.integer(format(as.Date(to), "%Y"))
            if (month_to <= month_range) {
                from = paste(year_to-1, 12+month_to-month_range, sub("[0-9]{4}-[0-9]{1,2}-","",to), sep="-")
            } else {
                from = paste(year_to, month_to-month_range, sub("[0-9]{4}-[0-9]{1,2}-","",to), sep="-")
            }
            
        } else if (grepl("[1-9][0-9]*y", date_range)) {
            year_range = as.integer(sub("y","",date_range))
            year_from = as.integer(format(as.Date(to), "%Y")) - year_range
            from = sub("^[0-9]{4}", year_from, to)
        } else {
            from = min_date
        }
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
    if (grepl('[0-9]{6}', symbol)) {
        syb = sub("^.*?([0-9]+).*$","\\1",symbol)
    if (nchar(syb)==6 & (nchar(symbol)==7 | nchar(symbol)==6)) {
        mkt = ifelse(grepl("\\^",symbol),"index","stock")
        tags = tags_symbol_stockcn(symbol, mkt)
        
        ex_code = ifelse(grepl("sse|szse",tags), substr(tags,1,2), NULL)
        if (!is.null(ex_code)) symbol = paste(syb, ex_code, sep=".")
    }
    }
    return(toupper(symbol))
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


# check frequency is daily data
check_freq_isdaily = function(dt) {
    setkeyv(dt, "date")
    # check freq of input data
    diff_date = dt[, as.numeric(mean(date - shift(date, n=1, type="lag"), na.rm=TRUE)) ]
    
    isdaily = ifelse(diff_date > 2, FALSE, TRUE)
    return(isdaily)
}
########################### helper functions ###########################
# # select rows in a dataframe
# sel_row_df = function(df, col_name = NULL, stop_condi = NULL) {
#     
# }

# download and read excel file from website
#' @importFrom readxl read_excel
# @importFrom utils download.file
#' @importFrom curl curl_download new_handle
load_read_xl = function(url, handle=new_handle()) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    curl_download(url, destfile = temp, handle = handle)
    dat = read_excel(temp)
    
    return(setDT(dat))
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
    return(setDT(dat))
}



# fill 0/na in a vector with last non 0/na value
fill0 = function(x, from_last = FALSE) {
    x[x==0] <- NA
    x2 = na.locf0(x, fromLast = from_last)
    
    # xdt = data.table(x = x)
    # while (xdt[x==0,.N] & xdt[,rowid:=.I][x==0, !all(rowid == .I)]) {
    #     xdt[, x_lag := shift(x, type='lag')
    #         ][x==0, x := x_lag]
    # }
    # x2 = xdt$x
    
    return(x2)
}
#' @importFrom zoo na.locf0
fillna = function(x, from_last = FALSE) {
    # https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
    
    x2 = na.locf0(x, fromLast = from_last)
    
    # xdt = data.table(x = x)
    # while (xdt[is.na(x),.N] & xdt[,rowid:=.I][is.na(x), !all(rowid == .I)]) {
    #     xdt[, x_lag := shift(x, type='lag')
    #       ][is.na(x), x := x_lag]
    # }
    # x2 = xdt$x
    
    return(x2)
}


# convert date to second 
date_to_sec = function(date=Sys.time()) {
    datetime = as.POSIXct(as.Date(date, origin = "1970-01-01"))
    return(trunc(as.numeric(datetime))) 
}

# end date of month
first_month_date = function(date) {
    as.Date(paste0(format(date, '%Y-%m'),'-01'))
}
last_month_date = function(date) {
    first_month_date(first_month_date(date) + 31) - 1
}

# loop on get_data1
load_dat_loop = function(symbol, func, args=list(), print_step) {
    runif = dt_list = NULL
    syb_len = length(symbol)
    for (i in 1:syb_len) {
        syb_i = symbol[i]
        # print
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, syb_len)), collapse = '/'), syb_i))
        dt_list[[syb_i]] = try(do.call(func, c(syb_i, args)), silent = TRUE)
        # sleep for 1s
        # Sys.sleep(runif(1))
    }
    return(dt_list)
}


# extract table from html via xml2 package
#' @import data.table
xml_table = function(wb, num=NULL, sup_rm=NULL, attr=NULL, header=FALSE) {
    doc0 = xml_find_all(wb, paste0("//table",attr)) # attr = '[@cellpadding="2"]'
    if (!is.null(num)) doc0 = doc0[num]
    
    doc = lapply(
        doc0,
        function(x) xml_text(xml_find_all(x, ".//tr"))
    )
    
    dt = lapply(doc, function(x) {
        if (!is.null(sup_rm)) x = gsub(sup_rm, "", x)
        
        dat = data.table(x = x)[, tstrsplit(x, "[\n\t\r]+")]
        if (header) {
            dat = setnames(dat[-1], as.character(dat[1]))
        }
        return(dat)
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



# select rows from a dataframe
# dt, a dataframe to be selected
# column, the targe column
# input_string, the pattern to match the rows in target column
# onerow, whether to return one row or multiple rows
select_rows_df = function(dt, column=NULL, input_string=NULL, onerow=FALSE) {
    seleted_rows = NULL
    
    while (is.null(seleted_rows) || nrow(seleted_rows) == 0) { # stop looping, if selected rows >=1
        if (is.null(input_string)) {
            print(setDT(copy(dt))[,lapply(.SD, format)], topn = 50)
            if (is.null(column)) {
                txt = "select rows via ('r'+rowid): "
            } else {
                txt = sprintf("select rows via ('r'+rowid) or (%s): ", column)
            }
            if (onerow) txt = sub('rows', 'one row', txt)
            sel_id = readline(txt)
            # if (sel_id == 'back') return('back')
        } else {
            sel_id = input_string
        }
        
        if (grepl('^r[1-9].*$', sel_id)) { # select rows via rowid 
            while (any(grepl("^r", sel_id))) {
                sel_id_string = gsub('r', '', sel_id)
                sel_id_string = gsub("[^[0-9:-]+", ",", sel_id_string)
                sel_id_string = gsub('-', ':', sel_id_string)
                sel_id_string = gsub('^[,:]+|[,:]+$', '', sel_id_string)
                sel_id = eval(parse( text = sprintf('c(%s)', sel_id_string) ))
                sel_id = intersect(sel_id, dt[,.I])
                if (length(sel_id)==0) {
                    sel_id = 'r'
                } else {
                    seleted_rows=dt[sel_id,]
                }
            }
        } else { # select rows via pattern matching
            seleted_rows=dt[grep(sel_id, dt[[column]], fixed = FALSE)]
            if (nrow(seleted_rows) == 0) seleted_rows=dt[grep(sel_id, dt[[column]], fixed = TRUE)]
            if (nrow(seleted_rows) == 0) input_string = NULL
        }
        
    }
    
    # if return only one row, then return the first row in the selected rows
    if (onerow & nrow(seleted_rows) >1) {
        # cat(sprintf('Only %s was selected\n.', seleted_rows[1]))
        seleted_rows = seleted_rows[1]
    }
    return(seleted_rows)
}


# ceiling on decimal
ceiling2 = function(x) {
    x_sci = format(x, scientific = TRUE, digits=2)
    z = ceiling(as.numeric(substr(x_sci, 1, 3)))
    e = substr(x_sci, 4, nchar(x_sci))
    as.numeric(paste0(z, e))
}

# is date/time class
isdatetime = function(x) {
    any(class(x) %in% c("Date","POSIXlt","POSIXct","POSIXt"))
}
