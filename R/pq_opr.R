#' crossover operators
#' 
#' Binary operators which create the upwards or downwards crossover signals.
#' 
#' @param x,y numeric vectors
#' 
#' @examples 
#' library(data.table)
#' library(pedquant)
#' 
#' data("dt_banks")
#' dtadj = md_stock_adjust(setDT(dt_banks)[symbol=='601988.SS'])
#' dtadjti = pq_addti(dtadj, x='close_adj', sma=list(n=200), sma=list(n=50))
#' 
#' dtorders = copy(dtadjti[[1]])[,.(symbol, name, date, close_adj, sma_50, sma_200)
#' ][sma_50 %x>% sma_200, `:=`(
#'     type = 'buy', prices = close_adj
#' )][sma_50 %x<% sma_200, `:=`(
#'     type = 'sell', prices = close_adj
#' )]
#' orders = dtorders[!is.na(type)]
#' head(orders)
#' 
#' @rdname crossover
#' @export
`%x>%` = function(x, y) {
    dx = x1 = x2 = NULL
    setDT(list(x1=x, x2=y))[
        , dx := x1-x2
    ][, dx > 0 & shift(dx, type = 'lag') <= 0]
}
#' @rdname crossover
#' @export
`%x<%` = function(x, y) {
    dx = x1 = x2 = NULL
    setDT(list(x1=x, x2=y))[
        , dx := x1-x2
    ][, dx < 0 & shift(dx, type = 'lag') >= 0]
}




pq1_opr = function(dt, opr1, x='date', y='close', syb='symbol', rm_na=FALSE) {
    . = f = name = NULL
    
    # cols of x & y
    cols = intersect(c(x,y), names(dt))
    if ('name' %in% names(dt)) dt_namcol = TRUE else dt_namcol = FALSE
    
    # split dt
    datlst = lapply(split(dt, by = syb, keep.by = FALSE), function(d) d[,cols,with=FALSE])
    
    # symbols used in operation string
    sybs = c()
    for (s in names(datlst)) {
        if (grepl(s, opr1, fixed=TRUE)) sybs = c(sybs, s)
    }
    
    # x from to
    xft = rbindlist(datlst[sybs],idcol=syb)
    ft = xft[,.(f=min(get(x)),t=max(get(x))),by=syb][,.(f=max(f),t=min(t))]   
    xft = unique(xft[,x,with=FALSE])[get(x)>=ft$f & get(x)<=ft$t]
    setorderv(xft, x)
    
    # assign xts objects
    oprsyb = oprnam = opr1
    for (s in sybs) {
        oprsyb = gsub(s,sprintf('`%s`',s),oprsyb,fixed = TRUE)
        if (dt_namcol) oprnam = sub(s,dt[get(syb)==s,name[.N]],oprnam)
        dtsyb = datlst[[s]][xft, on=x]
        if (isFALSE(rm_na)) dtsyb = dtsyb[, (y) := lapply(.SD, fillna), .SDcols=y]
        assign(s, as.xts.data.table(dtsyb))
    }
    
    # perform operation
    dtopr = eval(parse(text = oprsyb)) |> 
        as.data.table(keep.rownames = x) |> 
        na.omit(cols=y)
    dtopr[[syb]] = opr1
    dtopr[['name']] = oprnam
    cols = c(syb, 'name', cols)
    dtopr = dtopr[,cols,with=FALSE]
    
    return(dtopr)
}

#' dataframe operation
#' 
#' It performs arithmetic operation on numeric columns on multiple series. 
#' 
#' @param dt a list/dataframe of time series datasets.
#' @param opr operation string.
#' @param x the date column name, defaults to date.
#' @param y the numeric column names, defaults to close.
#' @param rm_na weather to remove NA values when perform arithmetic.
#' @param ... additional parameters.
#'  
#' @examples
#' data("dt_banks")
#' 
#' dt1 = pq_opr(dt_banks, '601288.SS/601988.SS')
#' print(dt1)
#' 
#' dt2 = pq_opr(dt_banks, c('(601288.SS+601988.SS)/2', '(601288.SS*601988.SS)^0.5'))
#' print(dt2)
#' 
#' @importFrom stats na.omit
#' @export 
pq_opr = function(dt, opr, x='date', y='close', rm_na=FALSE, ...) {
    args = list(...)
    if (!is.null(args$by)) syb = args$by else syb = 'symbol'
    
    dt = check_dt(copy(dt))
    setkeyv(dt,c(syb,x))
    
    opr = gsub(' ','',opr)
    oprlst = as.list(opr)
    names(oprlst) = opr
    
    lapply(
        oprlst, 
        function(o) {do.call(
            'pq1_opr', 
            args = list(dt=dt, opr=o, x=x, y=y, syb=syb, rm_na=rm_na)
        )}
    )
}

