########################### condition functions ###########################
# check dt
check_dt = function(dt, symb_name = FALSE, check_date = TRUE) {
    if (is.null(dt)) return(dt)
    
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    dt = setDT(dt)[]
    
    dtcols = names(dt)
    
    if (check_date & 'date' %in% dtcols) dt = dt[, date := as_date(date)]
        
    if (symb_name) {
        for (sn in c('symbol', 'name')) {
            if (!(sn %in% dtcols)) dt[[sn]] = sn
        }
    }
    
    setkeyv(dt, intersect(c('symbol', 'date'), names(dt)) )
    return(dt)
}

    
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




# check column name
check_xcol = function(dt, x) {
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    
    if (is.null(x)) {
        if ('close' %in% names(dt)) {
            x = 'close'
        } else if ('value' %in% names(dt)) {
            x = 'value'
        } else {
            stop('Please specify the asset price column.')
        }
    } else if (!(x %in% names(dt))) {
        if (grepl('\\|', x)) {
            x = intersect(unlist(strsplit(x, '\\|')), names(dt))[1] 
            warning(sprintf('The column of %s is chosen.', x))
        } else {
            stop(sprintf('The column %s is not exist in the input data.', x))
        }
    }
    return(x)
}

# this function has been removed
tags_symbol_stockcn = function(symbol, mkt, only_tags = TRUE) {
    syb = syb3 = NULL
    
    sm = data.table(
        syb = sub(".*?(\\d+).*","\\1", symbol), mkt = mkt
    )[nchar(syb)==6, syb3 := substr(syb,1,3)
    ][nchar(syb)==5, tags := ifelse(substr(syb,1,2)=="08", "hkex,,gem", "hkex,,main")]

    tags = rbind(
        merge(sm[nchar(syb)==6][,tags:=NULL], syb_cntags(), all.x = TRUE, by = c('mkt', 'syb3')),
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
# http://www.sse.com.cn/
# https://szse.com.cn/
# https://www.neeq.com.cn/
# https://www.bse.cn/
syb_cntags = function() {
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
        stock 43 neeq,-,neeq
        stock 83 neeq,-,neeq
        stock 87 neeq,-,neeq
        stock 400 neeq,-,neeq
        stock 420 neeq,-,neeq
        index 000 sse,-,-
        index 399 szse,-,-
        fund 15 szse,-,-
        fund 16 szse,-,-
        fund 18 szse,-,-
        fund 50 sse,-,-
        fund 51 sse,-,-
        fund 52 sse,-,-
        fund 56 sse,-,-
        fund 58 sse,-,-
        ",
        colClasses=list(character=1:3)
    )[,(c('exchange','AB','board')) := tstrsplit(tags,',')
     ][, exchg_code := toupper(substr(tags,1,2))
     ][exchg_code == 'SS', `:=`(city='sh', city_code='0')
     ][exchg_code == 'SZ', `:=`(city='sz', city_code='1')
     ][exchg_code == 'BS', `:=`(city='bj')
     ][grepl('neeq', tags), `:=`(city='nq')]
    
}
syb_add_cntags = function(symbol, market = NULL) {
    . = city = exchg_code = mkt = rid = syb = syb3 = syb_exp = NULL
     
    tags = syb_cntags()[, exchg_code := tolower(exchg_code)][]
    
    lst_syb = list(
        rid = seq_along(symbol), 
        symbol = tolower(as.character(symbol))
    )
    if (is.null(market)) {
        lst_syb$mkt = rep(as.character(NA), length(symbol))
    } else {
        if (length(market) == 1) market = rep(market, length(symbol))
        lst_syb$mkt = as.character(market)
    }
    
    # create a data.table of symbol
    dt_syb = setDT(lst_syb)[is.na(mkt) & grepl("^\\^",symbol), mkt := 'index' 
    ][is.na(mkt) & grepl("[0-9]{6}",symbol), mkt := 'stock'
    ][, syb3 := substr(sub('^\\^', '', symbol), 1, 3)
    ][!grepl('[0-9]{3}', syb3), syb3 := NA
    ][grepl('\\.(hk|ss|sz|bs)$',symbol), exchg_code := sub('.+\\.(hk|ss|sz|bs)$', '\\1', symbol)
    ][grepl('\\.(hk|sh|sz|bj)$',symbol), city := sub('.+\\.(hk|sh|sz|bj)$', '\\1', symbol)
    ][!is.na(exchg_code) | !is.na(city), mkt := NA]
    
    # merge dt_syb with tags
    dt_syb3 = dt_syb[!is.na(syb3)]
    dt_syb_tags = rbindlist(lapply(list(
        dt_syb3[!is.na(exchg_code)][, .(rid, symbol, syb3, exchg_code)], # syb3, exchg_code
        dt_syb3[is.na(exchg_code) & !is.na(city)][, .(rid, symbol, syb3, city)], # syb3, city
        dt_syb3[is.na(exchg_code) &  is.na(city)][, .(rid, symbol, syb3, mkt)], # syb3, mkt
        dt_syb3[, .(rid, symbol, syb3, mkt='fund')] # syb3, mkt(fund)
    ), function(x) {
        by_cols = intersect(names(x), names(tags))
        rbindlist(list(
            merge(copy(x), tags, by = by_cols), 
            merge(copy(x)[,syb3 := substr(syb3,1,2)], tags, by = by_cols)
        ), fill = TRUE)
    }), fill = TRUE)
    
    rbind(
        dt_syb_tags, dt_syb, fill=TRUE
    )[, .SD[1], keyby = 'rid'
    ][, syb := sub("^.*?([0-9]+).*$","\\1",symbol)
    ][!is.na(exchg_code), syb_exp := paste(syb, city, sep='.')
    ][is.na(syb_exp), syb_exp := syb
    ][, syb_exp := toupper(syb_exp)
    ][]
    
}
# check SSE/SZSE share symbols to download data from 163/tx/yahoo
syb_fmt_163 = function(symbol, mkt = NULL) {
    syb_163 = city_code = syb = tags = NULL
    
    symbol_163 = syb_add_cntags(
        symbol, mkt
    )[, syb_163 := paste0(city_code,syb)
    ][is.na(tags), syb_163 := symbol
    ][, syb_163]
    
    return(symbol_163)
}
syb_fmt_tx = function(symbol, mkt = NULL) {
    syb_tx = city = syb = tags = NULL
    
    symbol_tx = syb_add_cntags(
        symbol, mkt
    )[, syb_tx := paste0(city,syb)
    ][is.na(tags), syb_tx := symbol
    ][, syb_tx]
    
    return(symbol_tx)
}
syb_fmt_output = function(symbol, mkt = NULL) {
    syb_yh = syb = exchg_code = tags = cnsyb = city = NULL
    
    dtsyb = data.table(syb = symbol)[, cnsyb := grepl('[0-9]{6}', syb)]
    
    if (nrow(dtsyb[cnsyb == TRUE]) > 0) {
        sybout = syb_add_cntags(
            dtsyb[cnsyb == TRUE,syb], mkt
        )[, syb_yh := paste(syb, toupper(city), sep = '.') #paste0(city,syb)
        ][is.na(tags), syb_yh := symbol
        ][][, syb_yh]
    }
    
    dtsyb[cnsyb == TRUE, syb := sybout]
    
    return(toupper(dtsyb$syb))
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
#' @importFrom readr read_csv
#' @importFrom curl curl_download new_handle
load_read_csv2 = function(url) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    download.file(url=url, destfile=temp, quiet=TRUE)
    dat = read_csv(temp, show_col_types=FALSE)

    return(dat)
}
load_read_csv = function(url, encode="UTF-8", handle=new_handle(), csv_header=TRUE) {
    temp = tempfile()
    on.exit(unlink(temp))
    
    curl_download(url, destfile = temp, handle = handle)
    dat = try(suppressWarnings(read.csv(temp, fileEncoding = encode, header = csv_header)), silent = TRUE)
    if (inherits(dat, 'try-error')) return(invisible()) 
    else return(setDT(dat))
}

load_read_json = function(url) {
    url = "https://raw.githubusercontent.com/NateScarlet/holiday-cn/master/2007.json"
    
    temp = tempfile()
    on.exit(unlink(temp))
    
    download.file(url=url, destfile=temp, quiet=TRUE)
    dat = read_csv(temp, show_col_types=FALSE)
    
    
}

#' @importFrom jsonlite read_json
read_api_tsanghi = function(url) {
    dmp = read_json(url)
    setDT(rbindlist(lapply(dmp$data, function(x) {
        as.data.frame(lapply(x, function(x) {
            if (is.null(x)) x = NA 
            return(x)
        }))
    })))[]
}

#' @importFrom readr read_lines
#' @importFrom httr POST add_headers
read_api_sina = function(url) {
    datmp = GET(
        url, 
        add_headers(referer = "https://finance.sina.com.cn/")
    ) %>% 
        read_html(encoding = 'GBK') %>% 
        html_nodes('p') %>% 
        html_text() %>% 
        read_lines()
    
    return(datmp)
}
read_apidata_sina = function(url, sybs, cols_name) {
    doc = doc2 = symbol = NULL
    datmp = read_api_sina(url)
    
    dt = data.table(
        doc = datmp
    )[#, doc := iconv(doc, 'GB18030', 'UTF-8')
    ][, doc2 := sub('.+=\"(.+)\".+', '\\1', doc)
    ][, symbol := sybs
    ][doc != doc2
    ]
    
    dt2 = dt[, tstrsplit(doc2, ',')]
    dt2 = setnames(dt2, cols_name[1:ncol(dt2)])[, symbol := dt$symbol]
    return(dt2)
}

read_api_eastmoney = function(url) {
    datmp = POST(url) %>% 
        content(encoding = 'UTF-8')
        
    # GET(url) %>% 
    #     read_html() %>% 
    #     html_nodes('p') %>% 
    #     html_text() %>% 
    #     fromJSON(datmp)
    
    # datmp = rbindlist(lapply(datmp$data$diff, setDT))
    
    return(datmp)
}
read_apidata_eastmoney = function(url, type='history', datmp = NULL) {
    doc = NULL
    if (is.null(datmp)) datmp = read_api_eastmoney(url)
    
    if (is.null(datmp$data)) return(invisible())
    
    if (type == 'history') {
        dat = data.table(doc = datmp$data$klines)[, tstrsplit(doc, ',')][,`:=`(
            symbol = datmp$data$code, 
            name = datmp$data$name, 
            market = datmp$data$market
        )][]
    } else if (type == 'real_us') {
        dat = rbindlist(lapply(datmp$data$diff, setDT))
        
    } else if (type == 'real_cn') {
        dat = rbindlist(lapply(datmp$data$diff, setDT))
    }
    
    return(dat)
}

read_api_tencent = function(url) {
    datmp = GET(url) %>% 
        read_html(encoding = 'GBK') %>% 
        html_nodes('p') %>% 
        html_text() %>% 
        read_lines()
    
    return(datmp)
}
read_apidata_tencent = function(url) {
    . = doc = doc2 = NULL 
    
    datmp = read_api_tencent(url)
    
    dat = data.table(
        doc = datmp
    )[, .(doc2 = sub('.+=\"(.+)\".+', '\\1', doc))
    ][, tstrsplit(doc2, '~')]
    
    return(dat)
}
# fill 0/na in a vector with last non 0/na value
fill0 = function(x, from_last = FALSE) {
    x[x==0] <- NA
    x2 = na.locf0(x, fromLast = from_last)
    
    return(x2)
}
#' @importFrom zoo na.locf0
fillna = function(x, from_last = FALSE) {
    # https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
    
    x2 = na.locf0(x, fromLast = from_last)
    # data.table::nafill 
    
    return(x2)
}


# loop on get_data1
#' @importFrom stats rnorm
load_dat_loop = function(symbol, func, args=list(), print_step=0, sleep = 0, ...) {
    
    runif = dt_list = NULL
    syb_len = length(symbol)
    for (i in 1:syb_len) {
        syb_i = symbol[i]
        # print
        if ((print_step>0) & (i %% print_step == 0)) cat(sprintf('%s %s\n', paste0(format(c(i, syb_len)), collapse = '/'), syb_i))
        datmp = try(do.call(func, c(syb_i, args)), silent = TRUE)
        dt_list[[syb_i]] = datmp[]
        # sleep for 1s
        if (sleep > 0) Sys.sleep(abs(rnorm(1, mean=sleep)))
    }
    return(dt_list[])
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



# remove not available data
rm_error_dat = function(datlst) {
    # remove error symbols
    error_symbols = names(datlst)[which(sapply(datlst, function(x) inherits(x, 'try-error') | any(grepl('Error', x)) ))]
    
    if (length(error_symbols) > 0) {
        warning(sprintf('The following symbols can\'t imported:\n%s', paste0(error_symbols, collapse=', ')))
        datlst = datlst[setdiff(names(datlst), error_symbols)]
    }
    return(datlst)
}



