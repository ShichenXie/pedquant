commodity_symbol_fred = setDT(list(
    symbol = c('brent', 'wtic', 'natgas', 'goldfixam', 'goldfixpm', 'silvfixnn'),
    name = c('Crude Oil Brent', 'Crude Oil WTI', 'Natural Gas', 'Gold Fixing Price AM', 'Gold Fixing Price PM', 'Silver Fixing Price noon'),
    symbol_fred = c('DCOILBRENTEU', 'DCOILWTICO', 'DHHNGSP', 'GOLDAMGBD228NLBM', 'GOLDPMGBD228NLBM', 'SLVPRUSD')
))
func_commodity_symbol = function() commodity_symbol_fred

# query commodity price from FRED
md_commodity1_fred = function(syb, from, to) {
    symbol=symbol_fred=.=name=value=NULL
    
    syb_fred = commodity_symbol_fred[symbol == tolower(syb), symbol_fred]
    if (length(syb_fred) == 0) return(NULL)
    # libor in history
    dt_commo_hist = ed_fred(
        syb_fred, from=from, to=to, print_step=0L
    )[[1]][,`:=`(symbol_fred = symbol, symbol = NULL, name = NULL
    )][commodity_symbol_fred, on='symbol_fred', nomatch=0
       ][, .(symbol, name, date, value)
         ][!is.na(value)]
    
    setkey(dt_commo_hist, 'date')
    # return
    return(dt_commo_hist)
}

# query commodity data
# 
# 
# @export
md_commodity = function(symbol=NULL, date_range = '3y', from=NULL, to=Sys.Date(), print_step=1L, ...) {
    . = name = NULL
    
    # arguments
    syb = tolower(symbol)
    ## symbol
    if (is.null(symbol)) {
        syb = select_rows_df(commodity_symbol_fred[,.(symbol,name)], column='symbol')[,symbol]
    } else if (length(symbol)==1) {
        syb = select_rows_df(commodity_symbol_fred[,.(symbol,name)], column='symbol', input_string=syb)[,symbol]
    }
    syb = intersect(syb, commodity_symbol_fred$symbol)
    ## from/to
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1000-01-01", default_date_range = '3y')

    # data
    dat_list = load_dat_loop(syb, "md_commodity1_fred", args = list(from = from, to = to), print_step=print_step)
    return(dat_list)
}
