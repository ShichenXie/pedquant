index_symbol_yahoo = setDT(list(
    symbol = tolower(c(
        "^AEX","^AORD","^BFX","^XU100","^FCHI","^VIX","^BUK100P","^GDAXI","^DJA","^DJI","^DJT","^DJU","^STOXX50E","^N100","^FTSE","^FTMC","^FTAS","^KLSE","FTSEMIB.MI","^HSI","^IBEX","^BVSP","^MXX","^JKSE","^KS11","^MDAXI","^MERV","^NDX","^IXIC","^XAX","^NYA","^N225","PSEI.PS","^RUT","^GSPC","^BSESN","^AXJO","^NZ50","^GSPTSE","^SDAXI","000001.SS","399001.SZ","^STI","^TA125.TA","^TECDAX","^TWII"
    )),
    name = c(
        "AEX","ALL ORDINARIES","BEL 20","BIST 100","CAC 40","CBOE Volatility Index","Cboe UK 100 Price Return","DAX PERFORMANCE-INDEX","Dow Jones Composite Average","Dow Jones Industrial Average","Dow Jones Transportation Averag","Dow Jones Utility Average","ESTX 50 PR.EUR","EURONEXT 100","FTSE 100","FTSE 250","FTSE All Share","FTSE Bursa Malaysia KLCI","FTSE MIB Index","HANG SENG INDEX","IBEX 35","IBOVESPA","IPC MEXICO","Jakarta Composite Index","KOSPI Composite Index","MDAX PERFORMANCE-INDEX","MERVAL","NASDAQ 100","NASDAQ Composite","NYSE AMEX COMPOSITE INDEX","NYSE COMPOSITE (DJ)","Nikkei 225","PSEi INDEX","Russell 2000","S&P 500","S&P BSE SENSEX","S&P/ASX 200","S&P/NZX 50 INDEX GROSS","S&P/TSX Composite index","SDAX PERFORMANCEINDEX","SSE Composite Index","Shenzhen Component","STI Index","TA-125","TECDAX TR","TSEC weighted index"
    ),
    location = c(
        "Amsterdam","ASX","Brussels","Istanbul","Paris","Chicago Options","Stuttgart","XETRA","DJI","DJI","DJI","DJI","Zurich","Paris","FTSE","FTSE","FTSE","Kuala Lumpur","Milan","HKSE","MCE","Sao Paolo","Mexico","Jakarta","KSE","XETRA","Buenos Aires","Nasdaq GIDS","Nasdaq GIDS","NYSE","NYSE","Osaka","Philippine","Chicago Options","SNP","BSE","ASX","NZSE","Toronto","XETRA","Shanghai","Shenzhen","SES","Tel Aviv","XETRA","Taiwan"),
    currency = c(
        "EUR","AUD","EUR","TRY","EUR","USD","GBP","EUR","USD","USD","USD","USD","EUR","EUR","GBP","GBP","GBP","MYR","EUR","HKD","EUR","BRL","MXN","IDR","KRW","EUR","USD","USD","USD","USD","USD","JPY","PHP","USD","USD","INR","AUD","NZD","CAD","EUR","CNY","CNY","SGD","ILS","EUR","TWD")
))[order(-currency, location, name)]#[c(10,36,29,6,33,15,8,5,19,21,38,42,37,40), main:=TRUE]
func_indices_symbol = function() index_symbol_yahoo


# query index data
# 
# 
# @export
md_index = function(symbol=NULL, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = name = NULL
    # arguments
    # ## from/to

    ## symbol
    syb = tolower(symbol)
    if (is.null(symbol)) {
        syb = select_rows_df(index_symbol_yahoo[,.(symbol,name)], column='symbol')[,symbol]
    } else if (length(symbol)==1) {
        syb = select_rows_df(index_symbol_yahoo[,.(symbol,name)], column='symbol', input_string=syb)[,symbol]
    }
    syb = index_symbol_yahoo[, intersect(syb,symbol)]
    
    # data
    dat = try(do.call(md_stock, args = list(symbol = syb, source = 'yahoo', date_range = date_range, from = from, to = to, print_step = print_step, ...)), silent = TRUE)
    return(dat)
}