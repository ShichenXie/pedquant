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
    if (!grepl("max|ytd|qtd|mtd|[1-9][0-9]*d|[1-9][0-9]*w|[1-9,10,11]m|[1-9][0-9]*y", tolower(date_range))) {
        date_range = default
        warning(sprintf('The \'date_range\' is set to %s.', date_range))
    }
    return(date_range)
}

# check date format of from/to
check_fromto = function(fromto, type="date", shift = 0) {
    type = check_arg(type, c("date", "time"), "date")
    
    # type: dates or times
    if (inherits(fromto, "character")) {
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

getfrom_xm = function(date_range, to) {
    if (inherits(to, 'character')) to = as.Date(to)
    to_year = year(to)
    to_month = month(to)
    to_day = mday(to)
    
    xmonth_range = as.integer(sub("m","",date_range))
    
    if (to_month > xmonth_range) {
        from = paste(to_year, to_month-xmonth_range, to_day, sep="/")
    } else {
        from = paste(to_year - (floor(xmonth_range/12)+1), to_month + 12 - xmonth_range %% 12, to_day, sep="/")
    }
    
    return(as.Date(from))
}
getfrom_xtd = function(date_range, to) {
    if (inherits(to, 'character')) to = as.Date(to)
    to_year = year(to)
    to_quarter = quarter(to)
    to_month = month(to)
    
    if (date_range == 'ytd') {
        from = paste(to_year, "01", "01", sep="/")
    } else if (date_range == 'qtd') {
        from = paste(to_year, to_quarter*3-2, "01", sep="/") 
    } else if (date_range == 'mtd') {
        from = paste(to_year, to_month, "01", sep="/")
    }
    
    return(as.Date(from))
}
get_fromto = function(date_range, from, to, min_date = '1000-01-01', default_date_range = 'max') {
    date_range = check_date_range(date_range, default = default_date_range)
    to = check_fromto(to)
    min_date = check_fromto(min_date)
    
    if (inherits(from, 'character') & any(from == '')) from = NULL
    if (is.null(from)) {
        if (grepl("[yqm]td", date_range)) {
            from = getfrom_xtd(date_range, to)
        } else if (grepl("[1-9][0-9]*d", date_range)) {
            from = to - as.integer(sub("d","",date_range))
        } else if (grepl("[1-9][0-9]*w", date_range)) {
            from = to - as.integer(sub("w","",date_range))*7
        } else if (grepl("[1-9][0-9]*m", date_range)) {
            for (i in c(0, 1, -1, 2, -2)) {
                from = try(getfrom_xm(date_range, to+i), silent = TRUE)
                if (!inherits(from, 'try-error')) {
                    if (i != 0) from = from - i/abs(i)
                    break
                }
            }
        } else if (grepl("[1-9][0-9]*y", date_range)) {
            year_from = year(to) - as.integer(sub("y","",date_range))
            from = sub("^[0-9]{4}", year_from, to)
        } else {
            from = min_date
        }
    }
    from = check_fromto(from)
    if (from < min_date) from = min_date
    
    # set class
    if (inherits(to, "Date")) {
        from = as.Date(from)
    } else {
        from = as.POSIXct(from)
    }
    
    return(list(f=from, t = to))
}

# this function has been removed
tags_symbol_stockcn = function(symbol, mkt, only_tags = TRUE) {
    syb = syb3 = NULL
    
    sm = data.table(
        syb = sub(".*?(\\d+).*","\\1", symbol), mkt = mkt
    )[nchar(syb)==6, syb3 := substr(syb,1,3)
    ][nchar(syb)==5, tags := ifelse(substr(syb,1,2)=="08", "hkex,,gem", "hkex,,main")]

    tags = rbind(
        merge(sm[nchar(syb)==6][,tags:=NULL], tags_dt(), all.x = TRUE, by = c('mkt', 'syb3')),
        sm[nchar(syb)==5][],
        fill = TRUE
    )#[!is.na(tags)]#[,tags]
    
    
    # symbol2 = sub(".*?(\\d+).*","\\1", syb)
    # # print(symbol2)
    # if (nchar(symbol2)==6) {
    #     if (mkt=="stock") {
    #         tags = switch (substr(symbol2,1,3),
    #                 "600"="sse,A,main",
    #                 "601"="sse,A,main",
    #                 "603"="sse,A,main",
    #                 "900"="sse,B",
    #                 "000"="szse,A,main",
    #                 "001"="szse,A,main",
    #                 "002"="szse,A,sme",
    #                 "003"="szse,A,sme",
    #                 "004"="szse,A,sme",
    #                 "300"="szse,A,chinext",
    #                 "200"="szse,B",
    #         )
    #     } else if (mkt=="index") {
    #         tags = switch (substr(symbol2,1,3),
    #                 "000"="sse",
    #                 "399"="szse"
    #         )
    #     }
    # } else if (nchar(symbol2)==5) {
    #     tags = ifelse(substr(symbol2,1,2)=="08", "hkex,,gem", "hkex,,main")
    # }
    if (only_tags) tags = tags[,tags]
    return(tags)
}
# tags of SSE/SZSE shares symbols
tags_dt = function() {
    tags = exchg_code = NULL
    
    fread(
        "
        mkt syb3 tags
        stock 600 sse,A,main
        stock 601 sse,A,main
        stock 603 sse,A,main
        stock 605 sse,A,main
        stock 688 sse,A,star
        stock 900 sse,B,main
        stock 000 szse,A,main
        stock 001 szse,A,main
        stock 002 szse,A,sme
        stock 003 szse,A,sme
        stock 004 szse,A,sme
        stock 300 szse,A,chinext
        stock 301 szse,A,chinext
        stock 200 szse,B,main
        stock 201 szse,B,main
        index 000 sse,-,-
        index 399 szse,-,-
        fund 15 szse,-,-
        fund 16 szse,-,-
        fund 18 szse,-,-
        fund 50 sse,-,-
        fund 51 sse,-,-
        fund 52 sse,-,-
        ",
        colClasses=list(character=1:3)
    )[,(c('exchange','AB','board')) := tstrsplit(tags,',')
     ][, exchg_code := toupper(substr(tags,1,2))
     ][exchg_code == 'SS', `:=`(city='sh', city_code='0')
     ][exchg_code == 'SZ', `:=`(city='sz', city_code='1')][]
    
}
check_symbol_cn = function(symbol, mkt = NULL) {
    exchg_code = syb3 = syb = syb_code = syb_num = city = . = NULL
     
    tags = tags_dt()[, exchg_code := tolower(exchg_code)][]
    
    cn_dt = setDT(list(symbol = tolower(symbol)))[, syb_num := 3]
    if (!is.null(mkt) & length(mkt) == 1) {
        mkt = rep(mkt, length(symbol))
        cn_dt[, mkt := mkt]
    } else if (!is.null(mkt) & length(mkt) == length(symbol)) {
        cn_dt[, mkt := mkt]
    } else if (is.null(mkt) & !all(grepl('[a-zA-Z]', symbol))) {
        fund_syb3 = tags[mkt == 'fund', paste0('^',syb3, collapse = '|')]
        
        cn_dt[grepl("^\\^",symbol), mkt := 'index'
          ][grepl(fund_syb3, symbol), `:=`(mkt = "fund", syb_num = 2)
          ][is.na(mkt), mkt := 'stock']
    }
    
    cn_dt = cn_dt[, `:=`(
        syb = sub("^.*?([0-9]+).*$","\\1",symbol),
        syb_code = gsub("[^a-zA-Z]+",'',symbol)
    )][,syb3 := substr(syb, 1, syb_num)
     ][syb_code %in% c('ss','sz'), exchg_code := syb_code
     ][syb_code %in% c('sh','sz'), city := syb_code][]
    
    cn_tag_lst = lapply(list(
        cn_dt[!is.na(exchg_code)][,.(syb3, exchg_code, symbol, syb)],
        cn_dt[is.na(exchg_code) & !is.na(city)][,.(syb3, city, symbol, syb)],
        cn_dt[is.na(exchg_code) & is.na(city)][,.(mkt, syb3, symbol, syb)]
    ), function(c) {
        by_cols = intersect(names(c), names(tags))
        sub_cn_tags = merge(c, tags, by = by_cols, all.x = TRUE, sort = FALSE)
        return(sub_cn_tags)
    })
    cn_tag = rbindlist(cn_tag_lst, fill = TRUE)
    
    return(cn_tag)
}
# check SSE/SZSE share symbols to download data from 163/tx/yahoo
check_symbol_for_163 = function(symbol, mkt = NULL) {
    syb_163 = city_code = syb = tags = NULL
    
    symbol_163 = check_symbol_cn(
        symbol, mkt
    )[, syb_163 := paste0(city_code,syb)
    ][is.na(tags), syb_163 := symbol
    ][, syb_163]
    
    return(symbol_163)
}
check_symbol_for_tx = function(symbol, mkt = NULL) {
    syb_tx = city = syb = tags = NULL
    
    symbol_tx = check_symbol_cn(
        symbol, mkt
    )[, syb_tx := paste0(city,syb)
    ][is.na(tags), syb_tx := symbol
    ][, syb_tx]
    
    return(symbol_tx)
}
check_symbol_for_yahoo = function(symbol, mkt = NULL) {
    syb_yh = syb = exchg_code = tags = NULL
    
    if (all(grepl('[0-9]{6}', symbol))) {
        symbol = check_symbol_cn(
            symbol, mkt
        )[, syb_yh := paste(syb, exchg_code, sep = '.') #paste0(city,syb)
        ][is.na(tags), syb_yh := symbol
        ][][, syb_yh]
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

#' @importFrom curl has_internet nslookup
check_internet = function(host=NULL) {
    if (isFALSE(has_internet())) stop('No internet connection.')
    if (!is.null(host)) {
        nslookup(host)
    }
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
# load_read_csv2 = function(url, encode="UTF-8") {
#     temp = tempfile()
#     download.file(url=url, destfile=temp, quiet=TRUE)
#     dat = read.csv(temp, fileEncoding = encode)
#     unlink(temp)
# 
#     return(dat)
# }
load_read_csv = function(url, encode="UTF-8", handle=new_handle(), csv_header=TRUE) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    curl_download(url, destfile = temp, handle = handle)
    dat = suppressWarnings(read.csv(temp, fileEncoding = encode, header = csv_header))
    return(setDT(dat))
}


# @importFrom webdriver run_phantomjs Session install_phantomjs
load_web_source = function(url) {
    Session = install_phantomjs = run_phantomjs = NULL
    
    pjs <- try(run_phantomjs(), silent = TRUE)
    if (inherits(pjs, 'try-error')) {
        cat('Installing phantomjs ...\n')
        install_phantomjs()
        pjs <- try(run_phantomjs(), silent = TRUE)
    }
    ses <- Session$new(port = pjs$port)
    ses$go(url)
    wb = ses$getSource()
    return(wb)
}

# @importFrom RSelenium
load_web_source2 = function(url, sleep_time = 0, close_remDr = TRUE) {
    remoteDriver = NULL
    # RSelenium ref
    ## [Selenium](http://www.seleniumhq.org)
    ## [Selenium with Python](https://selenium-python.readthedocs.io/installation.html)
    ## [RSelenium: Basics](https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html)
    ## [Installing ChromeDriver on macOS](https://www.kenst.com/2015/03/installing-chromedriver-on-mac-osx/)
    
    # docker # browser + webDriver + selenium
    ## https://docs.docker.com/docker-for-mac/
    ## https://hub.docker.com/u/selenium/
    ## [An Introduction to Using Selenium-Docker Containers for End-to-End Testing](https://robotninja.com/blog/introduction-using-selenium-docker-containers-end-end-testing/)
    
    # docker command
    ## docker run hello-world
    ## docker pull selenium/standalone-chrome
    ## docker image ls
    ## docker run -d -p 4445:4444 selenium/standalone-chrome
    ## docker ps
    ## docker stop CONTAINER

    remDr <- remoteDriver(port = 4445L, browserName = "chrome")
    remDr$open(silent = TRUE)
    
    # navigate
    remDr$navigate(url)
    # if (sleep_time>0) Sys.sleep(sleep_time)
    
    wb = remDr$getPageSource()[[1]]
    # XML::htmlParse(wb)
    # remDr$getTitle()
    if (close_remDr) remDr$close()
    
    return(wb)
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
    
    # # data.worldbank.org
    # if (src=="wb") return(list(App_Token = '6NXEsbrbAhTma4LEVC74QWUlF', Secret_Token = 'n3TnGpllsYFkTLpI_sjexEll07OIY6LS9ECw'))
}



# select rows from a dataframe
txt_to_rowid = function(txt) {
    txt = gsub("[^[0-9:-]+", ",", txt)
    txt = gsub('-', ':', txt)
    txt = gsub('^[,:]+|[,:]+$', '', txt)
    rowid = eval(parse( text = sprintf('c(%s)', txt) ))
    return(rowid)
}
# dt, a dataframe to be selected
# column, the targe column
# input_string, the pattern to match the rows in target column
# onerow, whether to return one row or multiple rows
select_rows_df = function(dt, column=NULL, input_string=NULL, onerow=FALSE) {
    seleted_rows = NULL
    
    if (onerow) input_string = input_string[1]
    
    while (is.null(seleted_rows) || nrow(seleted_rows) == 0) { # stop looping, if selected rows >=1
        if (is.null(input_string)) {
            print(setDT(copy(dt))[,lapply(.SD, format)], topn = 50)
            if (is.null(column)) {
                txt = "select rows via 'rX': "
            } else {
                txt = sprintf("select rows via 'rX'(r1,3-5) or '%s': ", column)
            }
            if (onerow) txt = sub('rows', 'one row', txt)
            sel_id = readline(txt)
            # if (sel_id == 'back') return('back')
        } else {
            sel_id = input_string
        }
        
        if (identical(sel_id, 'all')) {
            seleted_rows = dt
        } else if (all(grepl('^r[1-9]+[0-9,-:]*$', sel_id)) && length(sel_id)==1) { # select rows via rowid 
            while (any(grepl("^r", sel_id))) {
                sel_id_string = gsub('r', '', sel_id)

                sel_id = intersect(txt_to_rowid(sel_id_string), dt[,.I])
                if (length(sel_id)==0) {
                    sel_id = 'r'
                } else {
                    seleted_rows=dt[sel_id,]
                }
            }
        } else { # select rows via pattern matching
            sr = unlist(sapply(
                unlist(strsplit(sel_id, '\\s*,\\s*')), 
                function(s) grep(sprintf('^%s',s), dt[[column]])
            ))
            if (length(sr) == 0) sr = txt_to_rowid(sel_id)
            
            seleted_rows = if (length(sr) == 0) dt[.0] else dt[sr]
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
    inherits(x, c("Date","POSIXlt","POSIXct","POSIXt"))
}

# chinese font by os
chn_font_family = function() {
    switch(
        Sys.info()[['sysname']],
        Windows= 'SimHei',
        Darwin = 'Hei Regular',
        NA
    )
}
