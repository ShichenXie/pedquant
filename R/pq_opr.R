pq1_opr = function(dt, opr1, x='date', y='close', by='symbol', rm_na=FALSE) {
    . = f = name = NULL
    
    # cols of x & y
    cols = intersect(c(x,y), names(dt))
    if ('name' %in% names(dt)) dt_namcol = TRUE else dt_namcol = FALSE
    
    # split dt
    datlst = lapply(split(dt, by = by, keep.by = FALSE), function(d) d[,cols,with=FALSE])
    
    # symbols used in operation string
    sybs = c()
    for (s in names(datlst)) {
        if (grepl(s, opr1, fixed=TRUE)) sybs = c(sybs, s)
    }
    
    # x from to
    xft = rbindlist(datlst[sybs],idcol=by)
    ft = xft[,.(f=min(get(x)),t=max(get(x))),by=by][,.(f=max(f),t=min(t))]   
    xft = unique(xft[,x,with=FALSE])[get(x)>=ft$f & get(x)<=ft$t]
    setorderv(xft, x)
    
    # assign xts objects
    oprsyb = oprnam = opr1
    for (s in sybs) {
        oprsyb = gsub(s,sprintf('`%s`',s),oprsyb,fixed = TRUE)
        if (dt_namcol) oprnam = sub(s,dt[get(by)==s,name[.N]],oprnam)
        dtsyb = datlst[[s]][xft, on=x]
        if (isFALSE(rm_na)) dtsyb = dtsyb[, (y) := lapply(.SD, fillna), .SDcols=y]
        assign(s, as.xts.data.table(dtsyb))
    }
    
    # perform operation
    dtopr = eval(parse(text = oprsyb)) |> 
        as.data.table(keep.rownames = x) |> 
        na.omit(cols=y)
    dtopr[[by]] = opr1
    dtopr[['name']] = oprnam
    cols = c(by, 'name', cols)
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
#' @param by the column name to specify different series, defaults to symbol.
#' @param rm_na weather to remove NA values when perform arithmetic.
#'  
#' @examples
#' data("dt_banks")
#' dt1 = pq_opr(dt_banks, '601288.SS/601988.SS')
#' dt2 = pq_opr(dt_banks, c('(601288.SS+601988.SS)/2', '(601288.SS*601988.SS)^0.5'))
#' 
#' @importFrom stats na.omit
#' @export 
pq_opr = function(dt, opr, x='date', y='close', by='symbol', rm_na=FALSE) {
    dt = check_dt(copy(dt))
    setkeyv(dt,c(by,x))
    
    opr = gsub(' ','',opr)
    oprlst = as.list(opr)
    names(oprlst) = opr
    
    lapply(
        oprlst, 
        function(o) {do.call(
            'pq1_opr', 
            args = list(dt=dt, opr=o, x=x, y=y, by=by, rm_na=rm_na)
        )}
    )
}

