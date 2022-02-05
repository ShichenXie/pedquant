# query forex real data ------
# http://hq.sinajs.cn/list=fx_susdcnh
md_forex_real_sina = function(symbols, ...) {
    symbol = name = NULL
    
    # symbols = c("AUDUSD", "USDCNY")
    symbols = tolower(symbols)
    url = sprintf('http://hq.sinajs.cn/list=%s', paste0(sprintf('fx_s%s', symbols), collapse = ',') )
    
    dat = read_apidata_sina(
        url = url, 
        sybs = symbols, 
        cols_name = c('time', 'buy', 'sell', 'close_prev', 'volatility', 'open', 'high', 'low', 'close', 'name', 'change_pct', 'change', 'amplitude', 'broker', paste0('v', 1:3), 'date'))
    
    
    dat = dat[, `:=`(
        date = as.Date(date), 
        name = sub('\u5373\u671f\u6c47\u7387$', '', name)
    )][, c('symbol', 'name', 'date', 'open', 'high', 'low', 'close', 'close_prev', 'change', 'change_pct', 'volatility', 'amplitude', 'time'), with = FALSE]
    
    return(dat)
}
# freeforexapi
md_forex_real_freeforex = function(fx) {
    forex_reals = function(fs) {
        f = paste0(toupper(fs), collapse = ',')
        dat = fromJSON(sprintf('https://www.freeforexapi.com/api/live?pairs=%s', f))
        
        dt = lapply(dat$rates, function(d) d[[1]])
        return(dt)
    }
    
    fxls = c(
        "AUDUSD", "EURGBP", "EURUSD", "GBPUSD", "NZDUSD", "USDAED", "USDAFN", "USDALL", "USDAMD",
        "USDANG", "USDAOA", "USDARS", "USDATS", "USDAUD", "USDAWG", "USDAZM", "USDAZN", "USDBAM",
        "USDBBD", "USDBDT", "USDBEF", "USDBGN", "USDBHD", "USDBIF", "USDBMD", "USDBND", "USDBOB",
        "USDBRL", "USDBSD", "USDBTN", "USDBWP", "USDBYN", "USDBYR", "USDBZD", "USDCAD", "USDCDF",
        "USDCHF", "USDCLP", "USDCNH", "USDCNY", "USDCOP", "USDCRC", "USDCUC", "USDCUP", "USDCVE",
        "USDCYP", "USDCZK", "USDDEM", "USDDJF", "USDDKK", "USDDOP", "USDDZD", "USDEEK", "USDEGP",
        "USDERN", "USDESP", "USDETB", "USDEUR", "USDFIM", "USDFJD", "USDFKP", "USDFRF", "USDGBP",
        "USDGEL", "USDGGP", "USDGHC", "USDGHS", "USDGIP", "USDGMD", "USDGNF", "USDGRD", "USDGTQ",
        "USDGYD", "USDHKD", "USDHNL", "USDHRK", "USDHTG", "USDHUF", "USDIDR", "USDIEP", "USDILS",
        "USDIMP", "USDINR", "USDIQD", "USDIRR", "USDISK", "USDITL", "USDJEP", "USDJMD", "USDJOD",
        "USDJPY", "USDKES", "USDKGS", "USDKHR", "USDKMF", "USDKPW", "USDKRW", "USDKWD", "USDKYD",
        "USDKZT", "USDLAK", "USDLBP", "USDLKR", "USDLRD", "USDLSL", "USDLTL", "USDLUF", "USDLVL",
        "USDLYD", "USDMAD", "USDMDL", "USDMGA", "USDMGF", "USDMKD", "USDMMK", "USDMNT", "USDMOP",
        "USDMRO", "USDMRU", "USDMTL", "USDMUR", "USDMVR", "USDMWK", "USDMXN", "USDMYR", "USDMZM",
        "USDMZN", "USDNAD", "USDNGN", "USDNIO", "USDNLG", "USDNOK", "USDNPR", "USDNZD", "USDOMR",
        "USDPAB", "USDPEN", "USDPGK", "USDPHP", "USDPKR", "USDPLN", "USDPTE", "USDPYG", "USDQAR",
        "USDROL", "USDRON", "USDRSD", "USDRUB", "USDRWF", "USDSAR", "USDSBD", "USDSCR", "USDSDD",
        "USDSDG", "USDSEK", "USDSGD", "USDSHP", "USDSIT", "USDSKK", "USDSLL", "USDSOS", "USDSPL",
        "USDSRD", "USDSRG", "USDSTD", "USDSTN", "USDSVC", "USDSYP", "USDSZL", "USDTHB", "USDTJS",
        "USDTMM", "USDTMT", "USDTND", "USDTOP", "USDTRL", "USDTRY", "USDTTD", "USDTVD", "USDTWD",
        "USDTZS", "USDUAH", "USDUGX", "USDUSD", "USDUYU", "USDUZS", "USDVAL", "USDVEB", "USDVEF",
        "USDVES", "USDVND", "USDVUV", "USDWST", "USDXAF", "USDXAG", "USDXAU", "USDXBT", "USDXCD",
        "USDXDR", "USDXOF", "USDXPD", "USDXPF", "USDXPT", "USDYER", "USDZAR", "USDZMK", "USDZMW",
        "USDZWD"
    )
    fx = toupper(fx)
    
    rtp1 = list()
    if (fx %in% fxls) {
        rtp1 = forex_reals(fx)
    } else {
        fx1 = substr(fx,1,3)
        fx2 = substr(fx,4,6)

        if (any(sapply(c(fx1, fx2), function(x) {
            x == 'USD'
        }))) {
            rtp1 = forex_reals(paste0(fx2,fx1))
        } else {
            fs = sapply(c(fx1, fx2), function(x) {
                fxls[grep(sprintf('USD%s',x), fxls)]
            })
            p2 = forex_reals(fs)
            rtp1[[fx]] = p2[[2]]/p2[[1]]
        }
    }
    return(rtp1)
}


# fred forex
func_forex_symbol = function() {
    main = NULL
    forex_symbol_fred = setDT(list(
        symbol = c("usdxtm",
                   "usdbrl","usdcad","usdcny","usddkk","usdhkd","usdinr","usdjpy","usdkrw","usdmyr","usdmxn",
                   "usdnok","usdsek","usdzar","usdsgd","usdlkr","usdchf","usdtwd","usdthb","audusd","eurusd",
                   "nzdusd","gbpusd","btcusd","bchusd","ethusd","ltcusd"), 
        name = c("US Dollar Index Trade Weighted",
                 "US Dollar/Brazilian Real","US Dollar/Canadian Dollar","US Dollar/Chinese Yuan Renminbi",
                 "US Dollar/Danish Krone","US Dollar/Hong Kong Dollar","US Dollar/Indian Rupee","US Dollar/Japanese Yen",
                 "US Dollar/South-Korean Won","US Dollar/Malaysian Ringgit","US Dollar/Mexican Peso","US Dollar/Norwegian Kroner",
                 "US Dollar/Swedish Krona","US Dollar/South African Rand","US Dollar/Singapore Dollar","US Dollar/Sri Lanka Rupee",
                 "US Dollar/Swiss Franc","US Dollar/Taiwan Dollar","US Dollar/Thai Baht","Australian Dollar/US Dollar",
                 "Euro/US Dollar","New Zealand Dollar/US Dollar","British Pound/US Dollar",
                 "Bitcoin/US Dollar","Bitcoin Cash/US Dollar","Ethereum/US Dollar","Litecoin/US Dollar"
        ),
        symbol_fred = c("DTWEXM",
                        "DEXBZUS","DEXCAUS","DEXCHUS","DEXDNUS","DEXHKUS","DEXINUS","DEXJPUS","DEXKOUS","DEXMAUS","DEXMXUS",
                        "DEXNOUS","DEXSDUS","DEXSFUS","DEXSIUS","DEXSLUS","DEXSZUS","DEXTAUS","DEXTHUS","DEXUSAL","DEXUSEU",
                        "DEXUSNZ","DEXUSUK","CBBTCUSD","CBBCHUSD","CBETHUSD","CBLTCUSD")
    ))[c(1,20,22,19,21,7,3,16,2,10,6,1,23), main := TRUE]
    return(forex_symbol_fred[])
}


# ,"usdx"
# ,"Trade Weighted USDX Major Currencies"
# ,"DTWEXM"

forex_symbol_oanda = setDT(list(
    symbol = c(
        "usd","afn","all","dzd","adf","adp","aoa","aon","ars","amd","awg","aud","ats","azm","azn","bsd",
        "bhd","bdt","bbd","byr","bef","bzd","bmd","btn","bob","bam","bwp","brl","gbp","bnd","bgn","bif",
        "xof","xaf","xpf","khr","cad","cve","kyd","clp","cny","cop","kmf","cdf","crc","hrk","cuc","cup",
        "cyp","czk","dkk","djf","dop","nlg","xeu","xcd","ecs","egp","svc","eek","etb","eur","fkp","fjd",
        "fim","frf","gmd","gel","dem","ghc","ghs","gip","xau","grd","gtq","gnf","gyd","htg","hnl","hkd",
        "huf","isk","inr","idr","irr","iqd","iep","ils","itl","jmd","jpy","jod","kzt","kes","kwd","kgs",
        "lak","lvl","lbp","lsl","lrd","lyd","ltl","luf","mop","mkd","mga","mgf","mwk","myr","mvr","mtl",
        "mro","mur","mxn","mdl","mnt","mad","mzm","mzn","mmk","ang","nad","npr","nzd","nio","ngn","kpw",
        "nok","omr","pkr","xpd","pab","pgk","pyg","pen","php","xpt","pln","pte","qar","rol","ron","rub",
        "rwf","wst","std","sar","rsd","scr","sll","xag","sgd","skk","sit","sbd","sos","zar","krw","esp",
        "lkr","shp","sdd","sdp","sdg","srd","srg","szl","sek","chf","syp","twd","tzs","thb","top","ttd",
        "tnd","trl","try","tmm","ugx","uah","uyu","aed","vuv","veb","vnd","yer","yun","zmk","zwd"), 
    name = c(
        "US Dollar", 
        "Afghanistan Afghani", "Albanian Lek", "Algerian Dinar", "Andorran Franc", "Andorran Peseta", 
        "Angolan Kwanza", "Angolan New Kwanza", "Argentine Peso", "Armenian Dram", "Aruban Florin", 
        "Australian Dollar", "Austrian Schilling", "Azerbaijan Manat", "Azerbaijan New Manat", "Bahamian Dollar", 
        "Bahraini Dinar", "Bangladeshi Taka", "Barbados Dollar", "Belarusian Ruble", "Belgian Franc", 
        "Belize Dollar", "Bermudian Dollar", "Bhutan Ngultrum", "Bolivian Boliviano", "Bosnian Mark", 
        "Botswana Pula", "Brazilian Real", "British Pound", "Brunei Dollar", "Bulgarian Lev", 
        "Burundi Franc", "CFA Franc BCEAO", "CFA Franc BEAC", "CFP Franc", "Cambodian Riel", 
        "Canadian Dollar", "Cape Verde Escudo", "Cayman Islands Dollar", "Chilean Peso", "Chinese Yuan Renminbi", 
        "Colombian Peso", "Comoros Franc", "Congolese Franc", "Costa Rican Colon", "Croatian Kuna", 
        "Cuban Convertible Peso", "Cuban Peso", "Cyprus Pound", "Czech Koruna", "Danish Krone", 
        "Djibouti Franc", "Dominican R. Peso", "Dutch Guilder", "ECU", "East Caribbean Dollar", 
        "Ecuador Sucre", "Egyptian Pound", "El Salvador Colon", "Estonian Kroon", "Ethiopian Birr", 
        "Euro", "Falkland Islands Pound", "Fiji Dollar", "Finnish Markka", "French Franc", 
        "Gambian Dalasi", "Georgian Lari", "German Mark", "Ghanaian Cedi", "Ghanaian New Cedi", 
        "Gibraltar Pound", "Gold (oz.)", "Greek Drachma", "Guatemalan Quetzal", "Guinea Franc", 
        "Guyanese Dollar", "Haitian Gourde", "Honduran Lempira", "Hong Kong Dollar", "Hungarian Forint", 
        "Iceland Krona", "Indian Rupee", "Indonesian Rupiah", "Iranian Rial", "Iraqi Dinar", 
        "Irish Punt", "Israeli New Shekel", "Italian Lira", "Jamaican Dollar", "Japanese Yen", 
        "Jordanian Dinar", "Kazakhstan Tenge", "Kenyan Shilling", "Kuwaiti Dinar", "Kyrgyzstanian Som", 
        "Lao Kip", "Latvian Lats", "Lebanese Pound", "Lesotho Loti", "Liberian Dollar", 
        "Libyan Dinar", "Lithuanian Litas", "Luxembourg Franc", "Macau Pataca", "Macedonian Denar", 
        "Malagasy Ariary", "Malagasy Franc", "Malawi Kwacha", "Malaysian Ringgit", "Maldive Rufiyaa", 
        "Maltese Lira", "Mauritanian Ouguiya", "Mauritius Rupee", "Mexican Peso", "Moldovan Leu", 
        "Mongolian Tugrik", "Moroccan Dirham", "Mozambique Metical", "Mozambique New Metical", "Myanmar Kyat", 
        "NL Antillian Guilder", "Namibia Dollar", "Nepalese Rupee", "New Zealand Dollar", "Nicaraguan Cordoba Oro", 
        "Nigerian Naira", "North Korean Won", "Norwegian Kroner", "Omani Rial", "Pakistan Rupee", 
        "Palladium (oz.)", "Panamanian Balboa", "Papua New Guinea Kina", "Paraguay Guarani", "Peruvian Nuevo Sol", 
        "Philippine Peso", "Platinum (oz.)", "Polish Zloty", "Portuguese Escudo", "Qatari Rial", 
        "Romanian Lei", "Romanian New Lei", "Russian Rouble", "Rwandan Franc", "Samoan Tala", 
        "Sao Tome/Principe Dobra", "Saudi Riyal", "Serbian Dinar", "Seychelles Rupee", "Sierra Leone Leone", 
        "Silver (oz.)", "Singapore Dollar", "Slovak Koruna", "Slovenian Tolar", "Solomon Islands Dollar", 
        "Somali Shilling", "South African Rand", "South-Korean Won", "Spanish Peseta", "Sri Lanka Rupee", 
        "St. Helena Pound", "Sudanese Dinar", "Sudanese Old Pound", "Sudanese Pound", "Suriname Dollar", 
        "Suriname Guilder", "Swaziland Lilangeni", "Swedish Krona", "Swiss Franc", "Syrian Pound", 
        "Taiwan Dollar", "Tanzanian Shilling", "Thai Baht", "Tonga Pa'anga", "Trinidad/Tobago Dollar", 
        "Tunisian Dinar", "Turkish Lira", "Turkish New Lira", "Turkmenistan Manat", "Uganda Shilling", 
        "Ukraine Hryvnia", "Uruguayan Peso", "Utd. Arab Emir. Dirham", "Vanuatu Vatu", "Venezuelan Bolivar", 
        "Vietnamese Dong", "Yemeni Rial", "Yugoslav Dinar", "Zambian Kwacha", "Zimbabwe Dollar")
))

    
# query forex historical data ------
# https://www.oanda.com/fx-for-business/historical-rates
md_forex1_oanda = function(symbol, from, to) {
    name = NULL
    
    syb = tolower(symbol)
    # symbol
    syb1 = substr(syb,1,3)
    syb2 = substr(syb,4,6)
    check_syb = lapply(list(syb1,syb2), function(x) {
        if (forex_symbol_oanda[symbol == x, .N==0]) stop('x is not avaiable from oanda')
    })
    # from
    if (Sys.Date()-from > 179) from = Sys.Date()-179
    
    # query data
    oanda_url <- sprintf("https://www.oanda.com/fx-for-business/historical-rates/api/data/update/?&source=OANDA&adjustment=0&base_currency=%s&start_date=%s&end_date=%s&period=daily&price=mid&view=table&quote_currency_0=%s", syb1, from, to, syb2)
    tbl <- fromJSON(oanda_url, simplifyVector = FALSE)
    ddtt = setDT(lapply(list(date=1,value=2), function(col) {
        sapply(tbl$widget[[1]]$data, function(dat) as.numeric(dat[[col]]))
    }))[, date :=as.Date(.POSIXct(date/1000, tz = "UTC"))][, `:=`(
        symbol = syb, name = paste0(forex_symbol_oanda[symbol==syb1,name], '/', forex_symbol_oanda[symbol==syb2,name])
    )]
    setkey(ddtt, 'date')
    return(ddtt[])
}
# query forex from FRED
md_forex1_fred = function(syb, from, to) {
    symbol = symbol_fred = . = name = value = geo = NULL
    
    forex_symbol_fred = func_forex_symbol()
    syb_fred = forex_symbol_fred[symbol == tolower(syb), symbol_fred]
    if (length(syb_fred) == 0) return(NULL)
    # libor in history
    dt_forex_hist = ed_fred(
        syb_fred, from=from, to=to, print_step=0L
    )[[1]][,`:=`(symbol_fred = symbol, symbol = NULL, name = NULL
    )][forex_symbol_fred, on='symbol_fred', nomatch=0
       ][, .(symbol, name, date, value, geo, unit)
         ][!is.na(value)]
    # return
    return(dt_forex_hist)
}

# http://www.fxfupan.com/
md_forex1_fxfupan = function(syb) {
    # sybs = c('AUDJPY', 'AUDUSD', 'CHFJPY', 'EURCAD', 'EURCHF', 'EURGBP', 'EURJPY', 'EURUSD', 'GBPCHF', 'GBPJPY', 'GBPUSD', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDJPY', 'USDCHF', 'XAGUSD', 'XAUUSD') 
    
    url = 'http://www.forextester.com/templates/data/files/%s.zip'
    datmp = load_read_csv2(sprintf(url, syb))
    setnames(setDT(datmp), c('symbol', 'date', 'time', 'open', 'high', 'low', 'close', 'volume'))
    
    return(datmp)
}

#' query forex data
#' 
#' \code{md_forex} query forex market data from FRED (history data) or sina (real data).
#' 
#' @param symbol forex symbols. Default is NULL. 
#' @param type the data type, available values including history and real. Default is history. 
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is 3y.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L. 
#' @param ... Additional parameters.
#' 
#' @examples 
#' \dontrun{
#' # history data
#' dtfx_hist1 = md_forex(c('usdcny', 'usdjpy'))
#' 
#' # real data
#' dtfx_real = md_forex(c('eurusd', 'usdcny', 'usdjpy'), type = 'real')
#' 
#' # interactivly choose symbols
#' dtfx_hist2 = md_forex()
#' }
#' 
#' 
#' @import data.table 
#' @export
md_forex = function(symbol=NULL, type = 'history', date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = name = NULL
    
    # symbol
    forex_symbol_fred = func_forex_symbol()
    if (is.null(symbol)) syb = select_rows_df(forex_symbol_fred[,.(symbol,name)], column='symbol')[,symbol]
    syb = tolower(symbol)
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')
    ## source
    source = list(...)[['source']]
    
    # data
    if (type == 'history') {
        # load data by symbol
        dat_list = load_dat_loop(
            syb, 'md_forex1_fred', 
            args = list(from = from, to = to, ...), 
            print_step=print_step, ...)
        dat_list = rm_error_dat(dat_list)
    } else if (type == 'real') {
        dat_list = md_forex_real_sina(syb)
    }
    
    return(dat_list)
}

md_forex_symbol = function() {
    func_forex_symbol()[, c('symbol', 'name'), with = FALSE]
}