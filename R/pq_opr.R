# interval operators
# 
# Binary operators which create the interval signals.
# 
# @param x numeric vectors
# @param rng numeric vectors, top and bottom limitation
# 
# @examples 
# # 0:4 %()% c(1,3)
# 
# # 0:4 %[)% c(1,3)
# 
# # 0:4 %(]% c(1,3)
# 
# # 0:4 %[]% c(1,3)
# 
# @rdname interval
# @export
`%()%` = function(x, rng) {
    rng = sort(rng)
    x > rng[1] & x < rng[2]
}

# @rdname interval
# @export
`%[)%` = function(x, rng) {
    rng = sort(rng)
    x >= rng[1] & x < rng[2]
}

# @rdname interval
# @export
`%(]%` = function(x, rng) {
    rng = sort(rng)
    x > rng[1] & x <= rng[2]
}

# @rdname interval
# @export
`%[]%` = function(x, rng) {
    rng = sort(rng)
    x >= rng[1] & x <= rng[2]
}


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
#' boc = md_stock_adjust(setDT(dt_banks)[symbol=='601988.SH'])
#' bocti = pq_addti(boc, x='close_adj', sma=list(n=200), sma=list(n=50))
#' 
#' dtorders = copy(bocti[[1]])[,.(symbol, name, date, close_adj, sma_50, sma_200)
#' ][sma_50 %x>% sma_200, `:=`(
#'     side = 'buy', prices = close_adj
#' )][sma_50 %x<% sma_200, `:=`(
#'     side = 'sell', prices = close_adj
#' )][, (c('side', 'prices')) := lapply(.SD, shift), .SDcols = c('side', 'prices')]
#' orders = dtorders[!is.na(side)]
#' head(orders)
#' 
#' e = pq_plot(boc,  y='close_adj', addti = list(sma=list(n=200), sma=list(n=50)), orders = orders)
#' e[[1]]
#' 
#' @rdname crossover
#' @export
`%x>%` = function(x, y) {
    dx = x1 = x2 = NULL
    
    if (length(y) == 1) y = rep(y, length(x))
    
    setDT(list(x1=x, x2=y))[
        , dx := x1-x2
    ][, dx > 0 & shift(dx, type = 'lag') <= 0]
}
#' @rdname crossover
#' @export
`%x<%` = function(x, y) {
    dx = x1 = x2 = NULL
    
    if (length(y) == 1) y = rep(y, length(x))
    
    setDT(list(x1=x, x2=y))[
        , dx := x1-x2
    ][, dx < 0 & shift(dx, type = 'lag') >= 0]
}




pq1_opr = function(dt, opr1, x='close', syb='symbol', rid='date', rm_na=FALSE) {
    . = f = name = NULL
    
    # cols of rid & x
    cols = intersect(c(rid,x), names(dt))
    if ('name' %in% names(dt)) dt_namcol = TRUE else dt_namcol = FALSE
    
    # split dt
    datlst = lapply(split(dt, by = syb, keep.by = FALSE), function(d) d[,cols,with=FALSE])
    
    # symbols used in operation string
    sybs = c()
    for (s in names(datlst)) {
        if (grepl(s, opr1, fixed=TRUE)) sybs = c(sybs, s)
    }
    
    # rid from to
    xft = rbindlist(datlst[sybs],idcol=syb)
    ft = xft[,.(f=min(get(rid)),t=max(get(rid))),by=syb][,.(f=max(f),t=min(t))]   
    xft = unique(xft[,rid,with=FALSE])[get(rid)>=ft$f & get(rid)<=ft$t]
    setorderv(xft, rid)
    
    # assign xts objects
    oprsyb = oprnam = opr1
    for (s in sybs) {
        oprsyb = gsub(s,sprintf('`%s`',s),oprsyb,fixed = TRUE)
        if (dt_namcol) oprnam = sub(s,dt[get(syb)==s,name[.N]],oprnam)
        dtsyb = datlst[[s]][xft, on=rid]
        if (isFALSE(rm_na)) dtsyb = dtsyb[, (x) := lapply(.SD, fillna), .SDcols=x]
        assign(s, as.xts.data.table(dtsyb))
    }
    
    # perform operation
    dtopr = eval(parse(text = oprsyb)) |> 
        as.data.table(keep.rownames = rid) |> 
        na.omit(cols=x)
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
#' @param x the numeric column names, defaults to close.
#' @param rm_na weather to remove NA values when perform arithmetic.
#' @param ... additional parameters.
#'  
#' @examples
#' data("dt_banks")
#' 
#' dt1 = pq_opr(dt_banks, '601288.SH/601988.SH')
#' print(dt1)
#' 
#' dt2 = pq_opr(dt_banks, c('(601288.SH+601988.SH)/2', '(601288.SH*601988.SH)^0.5'))
#' print(dt2)
#' 
#' @importFrom stats na.omit
#' @export 
pq_opr = function(dt, opr, x='close', rm_na=FALSE, ...) {
    args = list(...)
    if (!is.null(args$by)) syb = args$by else syb = 'symbol'
    if (!is.null(args$rid)) rid = args$rid else rid = 'date'
    
    dt = check_dt(copy(dt))
    setkeyv(dt,c(syb,rid))
    
    opr = gsub(' ','',opr)
    oprlst = as.list(opr)
    names(oprlst) = opr
    
    lapply(
        oprlst, 
        function(o) {do.call(
            'pq1_opr', 
            args = list(dt=dt, opr=o, x=x, syb=syb, rid=rid, rm_na=rm_na)
        )}
    )
}

